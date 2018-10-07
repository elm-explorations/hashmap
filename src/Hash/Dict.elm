module Hash.Dict exposing
    ( Dict
    , empty, singleton, insert, update, remove
    , get, isEmpty, member, size
    , union, intersect, diff
    , toList, fromList, keys, values
    , foldl, foldr, map, filter, partition
    )

{-| A dictionary mapping unique keys to values.


# Dictionary

@docs Dict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs get, isEmpty, member, size


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList, keys, values


# Transform

@docs foldl, foldr, map, filter, partition

-}

import Bitwise
import Hash.FNV as FNV
import Hash.JsArray as JsArray exposing (JsArray)
import List.Extra as List


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.

This implementation differs from the one in core in that keys don't have to
be comparable, and that operations like foldl and foldr iterates the key-value
pairs in insertion order instead of sorted order.

-}
type
    Dict k v
    -- Bitmap (Int): The tree is compressed (empty nodes are not stored), so
    -- this bitmap stores what index each node is actually stored at.
    --
    -- Tree (NodeArray k v): An array of nodes. "Empty" nodes are not
    -- stored in the array, so we need a bitmap to figure out which
    -- index each node is actually stored at.
    --
    -- Int Dict: This datastructure stores each key-value pair in
    -- insertion order. Whenever we iterate over the hash-map, we actually
    -- iterate over this data structure. You can find the Int Dict impl.
    -- at the bottom of this file.
    = Dict Int (NodeArray k v) (IntDict k v)


type alias NodeArray k v =
    JsArray (Node k v)


type
    Node k v
    -- Index into Int Dict structure, the key hash, key and value.
    = Leaf Int Int k v
      -- Bitmap for the NodeArray and the actual NodeArray
    | SubTree Int (NodeArray k v)
      -- Hashing different keys can give us the same result. We call this a collision.
      -- The hash for each triplet, and a list of (Int Dict index, key and value)
    | Collision Int (List ( Int, k, v ))


{-| The size of each node array. Read Array documentation for more info.
-}
branchFactor : Int
branchFactor =
    32


{-| How many bits represents the branching factor. Read Array documentation for
more info.
-}
shiftStep : Int
shiftStep =
    5


{-| A mask which, when used in a bitwise and, reads the first `shiftStep` bits
in a number as a number of its own.
-}
bitMask : Int
bitMask =
    Bitwise.shiftRightZfBy (32 - shiftStep) 0xFFFFFFFF


{-| When used in a bitwise and, clears the first `shiftStep` bits from an int.
-}
invertedBitMask : Int
invertedBitMask =
    Bitwise.complement bitMask


{-| Every time we insert something into `Int Dict` we store the key-value at
the previous index + 1. Removing a key-value pair doesn't change this, so at
some point we will run out of indices. When we do, we need to rebuild the
structure an try again.

Since we use bitwise operations on indices, the rebuild threshold has to be
the largest int value for a 32-bit int.

-}
rebuildThreshold : Int
rebuildThreshold =
    0xFFFFFFFF


{-| The empty dictionary.
-}
empty : Dict k v
empty =
    Dict 0 JsArray.empty intDictEmpty


{-| Create a dictionary with one key-value pair.
-}
singleton : k -> v -> Dict k v
singleton key val =
    insert key val empty


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : Dict k v -> Bool
isEmpty (Dict bitmap _ _) =
    bitmap == 0


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict k v -> Int
size (Dict _ _ triplets) =
    triplets.size


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : k -> Dict k v -> Maybe v
get key (Dict bitmap nodes values) =
    getHelp 0 (FNV.hash key) key bitmap nodes


getHelp : Int -> Int -> k -> Int -> NodeArray k v -> Maybe v
getHelp shift hash key bitmap nodes =
    let
        index =
            -- Read `shift + shiftStep` bits from the hash as a number
            -- of its own.
            Bitwise.and bitMask (Bitwise.shiftRightZfBy shift hash)

        mask =
            -- Create a mask, a number where a specific bit is set. We
            -- can use this to figure out what the actual position of
            -- a node is in the tree.
            Bitwise.shiftLeftBy index 1
    in
    -- is the bit at `index` set?
    if Bitwise.and bitmap mask == mask then
        case JsArray.unsafeGet (compressedIndex index bitmap) nodes of
            Leaf _ eIdx eKey eVal ->
                if key == eKey then
                    Just eVal

                else
                    Nothing

            SubTree subBitmap subNodes ->
                getHelp (shift + shiftStep) hash key subBitmap subNodes

            Collision _ vals ->
                case List.find (\( _, k, _ ) -> k == key) vals of
                    Just ( _, _, val ) ->
                        Just val

                    Nothing ->
                        Nothing

    else
        Nothing


{-| Given an index and a bitmap, return the compressed index of a Node
in a NodeArray.
-}
compressedIndex : Int -> Int -> Int
compressedIndex index bitmap =
    -- The NodeArray at each level of a tree can be, at most, 32 in size.
    -- A bitmap can contain 32 bits. 1 bit represents a stored value.
    -- The compressed index is the number of elements to the left of the
    -- idx bit.
    let
        relevantBits =
            Bitwise.shiftLeftBy 1 (Bitwise.shiftLeftBy (31 - index) bitmap)

        -- Count the number of set bits (1 bits) in an Int.
        -- See: <https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel>
        b1 =
            relevantBits - Bitwise.and (Bitwise.shiftRightZfBy 1 relevantBits) 0x55555555

        b2 =
            Bitwise.and b1 0x33333333 + Bitwise.and (Bitwise.shiftRightZfBy 2 b1) 0x33333333
    in
    Bitwise.shiftRightZfBy 24 (Bitwise.and (b2 + Bitwise.shiftRightZfBy 4 b2) 0x0F0F0F0F * 0x01010101)


{-| Determine if a key is in a dictionary.
-}
member : k -> Dict k v -> Bool
member key (Dict bitmap nodes _) =
    case getHelp 0 (FNV.hash key) key bitmap nodes of
        Just _ ->
            True

        Nothing ->
            False


{-| Insert a key-value pair into the dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> Dict k v -> Dict k v
insert key value dict =
    let
        (Dict bitmap nodes triplets) =
            rebuildOnOverflow dict
    in
    insertHelp 0 (FNV.hash key) key value bitmap nodes triplets


{-| At some point we will run out of order indices, so we'll have to compress
the int dict by rebuilding the it
-}
rebuildOnOverflow : Dict k v -> Dict k v
rebuildOnOverflow ((Dict _ _ rootTriplets) as dict) =
    if rootTriplets.size >= rebuildThreshold then
        let
            helper : ( Int, k, v ) -> Dict k v -> Dict k v
            helper ( hash, key, value ) (Dict bitmap nodes triplets) =
                insertHelp 0 hash key value bitmap nodes triplets
        in
        intDictFoldl helper empty rootTriplets

    else
        dict


insertHelp :
    Int
    -> Int
    -> k
    -> v
    -> Int
    -> NodeArray k v
    -> IntDict k v
    -> Dict k v
insertHelp shift hash key value bitmap nodes triplets =
    let
        uncompressedIdx =
            Bitwise.and bitMask (Bitwise.shiftRightZfBy shift hash)

        comIdx =
            compressedIndex uncompressedIdx bitmap

        newShift =
            shift + shiftStep

        mask =
            Bitwise.shiftLeftBy uncompressedIdx 1

        hasValue =
            Bitwise.and bitmap mask == mask
    in
    if hasValue then
        case JsArray.unsafeGet comIdx nodes of
            (Leaf xHash xIdx xKey xVal) as node ->
                if xHash == hash then
                    if xKey == key then
                        Dict
                            bitmap
                            (JsArray.unsafeSet comIdx (Leaf xHash xIdx xKey value) nodes)
                            (intDictSet xIdx hash key value triplets)

                    else
                        let
                            element =
                                Collision hash
                                    [ ( triplets.nextIndex, key, value )
                                    , ( xIdx, xKey, xVal )
                                    ]
                        in
                        Dict
                            bitmap
                            (JsArray.unsafeSet comIdx element nodes)
                            (intDictPush hash key value triplets)

                else
                    let
                        subIdx =
                            Bitwise.and bitMask (Bitwise.shiftRightZfBy newShift xHash)

                        (Dict secondBitmap secondNodes newTriplets) =
                            insertHelp
                                newShift
                                hash
                                key
                                value
                                (Bitwise.shiftLeftBy subIdx 1)
                                (JsArray.singleton node)
                                triplets

                        subTree =
                            SubTree secondBitmap secondNodes
                    in
                    Dict
                        bitmap
                        (JsArray.unsafeSet comIdx subTree nodes)
                        newTriplets

            SubTree subBitmap subNodes ->
                let
                    (Dict newSubBitmap newSubNodes newTriplets) =
                        insertHelp newShift hash key value subBitmap subNodes triplets

                    newSub =
                        SubTree newSubBitmap newSubNodes
                in
                Dict
                    bitmap
                    (JsArray.unsafeSet comIdx newSub nodes)
                    newTriplets

            (Collision xHash pairs) as currValue ->
                if xHash == hash then
                    let
                        keyFinder ( _, k, _ ) =
                            k == key
                    in
                    case List.find keyFinder pairs of
                        Just ( existingIdx, _, _ ) ->
                            let
                                whenRemoved =
                                    List.filter keyFinder pairs

                                updated =
                                    ( existingIdx, key, value ) :: whenRemoved
                            in
                            Dict
                                bitmap
                                (JsArray.unsafeSet comIdx (Collision xHash updated) nodes)
                                (intDictSet existingIdx hash key value triplets)

                        Nothing ->
                            let
                                updated =
                                    ( triplets.nextIndex, key, value ) :: pairs
                            in
                            Dict
                                bitmap
                                (JsArray.unsafeSet comIdx (Collision hash updated) nodes)
                                (intDictPush hash key value triplets)

                else
                    let
                        collisionPos =
                            Bitwise.and bitMask (Bitwise.shiftRightZfBy newShift hash)

                        collisionMask =
                            Bitwise.shiftLeftBy collisionPos 1

                        (Dict subBitmap subNodes subTriplets) =
                            insertHelp
                                newShift
                                hash
                                key
                                value
                                (Bitwise.shiftLeftBy collisionPos 1)
                                (JsArray.singleton currValue)
                                triplets
                    in
                    Dict
                        bitmap
                        (JsArray.unsafeSet comIdx (SubTree subBitmap subNodes) nodes)
                        subTriplets

    else
        Dict
            (Bitwise.or bitmap mask)
            (JsArray.unsafeInsert comIdx (Leaf hash triplets.nextIndex key value) nodes)
            (intDictPush hash key value triplets)


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> Dict k v -> Dict k v
remove key (Dict bitmap nodes triplets) =
    removeHelp 0 (FNV.hash key) key bitmap nodes triplets


removeHelp :
    Int
    -> Int
    -> k
    -> Int
    -> NodeArray k v
    -> IntDict k v
    -> Dict k v
removeHelp shift hash key bitmap nodes triplets =
    let
        uncompIdx =
            Bitwise.and bitMask (Bitwise.shiftRightZfBy shift hash)

        compIdx =
            compressedIndex uncompIdx bitmap

        mask =
            Bitwise.shiftLeftBy uncompIdx 1

        hasValue =
            Bitwise.and bitmap mask == mask
    in
    if hasValue then
        case JsArray.unsafeGet compIdx nodes of
            Leaf _ eIdx eKey eVal ->
                if eKey == key then
                    Dict
                        (Bitwise.xor bitmap mask)
                        (JsArray.removeIndex compIdx nodes)
                        (intDictRemove eIdx triplets)

                else
                    Dict bitmap nodes triplets

            SubTree subBitmap subNodes ->
                let
                    (Dict newSubBitmap newSubNodes newTriplets) =
                        removeHelp
                            (shift + shiftStep)
                            hash
                            key
                            subBitmap
                            subNodes
                            triplets
                in
                if newSubBitmap == 0 then
                    Dict
                        (Bitwise.xor bitmap mask)
                        (JsArray.removeIndex compIdx nodes)
                        newTriplets

                else
                    Dict
                        bitmap
                        (JsArray.unsafeSet compIdx (SubTree newSubBitmap newSubNodes) nodes)
                        newTriplets

            Collision _ vals ->
                let
                    maybeIdx =
                        List.find (\( _, k, _ ) -> k == key) vals
                            |> Maybe.map (\( idx, _, _ ) -> idx)

                    newCollision =
                        case maybeIdx of
                            Just removeIdx ->
                                List.filter (\( _, k, _ ) -> k /= key) vals

                            Nothing ->
                                vals

                    newTriplets =
                        case maybeIdx of
                            Just removeIdx ->
                                intDictRemove removeIdx triplets

                            Nothing ->
                                triplets
                in
                case newCollision of
                    [] ->
                        Dict
                            (Bitwise.xor bitmap mask)
                            (JsArray.removeIndex compIdx nodes)
                            newTriplets

                    ( eIdx, eKey, eVal ) :: [] ->
                        Dict
                            bitmap
                            (JsArray.unsafeSet compIdx (Leaf hash eIdx eKey eVal) nodes)
                            newTriplets

                    _ ->
                        Dict
                            bitmap
                            (JsArray.unsafeSet compIdx (Collision hash newCollision) nodes)
                            newTriplets

    else
        Dict
            bitmap
            nodes
            triplets


{-| Update the value of a dictionary for a specific key with a given function.
The given function gets the current value as a parameter and its return value
determines if the value is updated or removed. New key-value pairs can be
inserted too.
-}
update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key fn ((Dict bitmap nodes triplets) as dict) =
    let
        hash =
            FNV.hash key
    in
    case fn (getHelp 0 hash key bitmap nodes) of
        Nothing ->
            removeHelp 0 hash key bitmap nodes triplets

        Just value ->
            insertHelp 0 hash key value bitmap nodes triplets



-- LISTS


{-| Convert an association list into a dictionary.
-}
fromList : List ( k, v ) -> Dict k v
fromList list =
    List.foldl (\( key, value ) acc -> insert key value acc) empty list


{-| Convert a dictionary into an association list of key-value pairs.
-}
toList : Dict k v -> List ( k, v )
toList (Dict _ _ triplets) =
    let
        helper : ( Int, k, v ) -> List ( k, v ) -> List ( k, v )
        helper ( _, key, value ) acc =
            ( key, value ) :: acc
    in
    intDictFoldr helper [] triplets


{-| Get all of the keys in a dictionary.

       keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]

-}
keys : Dict k v -> List k
keys (Dict _ _ triplets) =
    let
        helper : ( Int, k, v ) -> List k -> List k
        helper ( _, key, _ ) acc =
            key :: acc
    in
    intDictFoldr helper [] triplets


{-| Get all of the values in a dictionary as a List.

       values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]

-}
values : Dict k v -> List v
values (Dict _ _ triplets) =
    let
        helper : ( Int, k, v ) -> List v -> List v
        helper ( _, _, value ) acc =
            value :: acc
    in
    intDictFoldr helper [] triplets



-- TRANSFORM


{-| Fold over the key-value pairs in a dictionary in the order the key was
first inserted.
-}
foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
foldl fn acc (Dict _ _ triplets) =
    let
        helper : ( Int, k, v ) -> b -> b
        helper ( _, key, value ) acc =
            fn key value acc
    in
    intDictFoldl helper acc triplets


{-| Fold over the key-value pairs in a dictionary in the reverse order the key
was first inserted.
-}
foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr fn acc (Dict _ _ triplets) =
    let
        helper : ( Int, k, v ) -> b -> b
        helper ( _, key, value ) acc =
            fn key value acc
    in
    intDictFoldr helper acc triplets


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map fn (Dict rootBitmap rootNodes rootTriplets) =
    let
        valueHelper : ( Int, k, a ) -> ( Int, k, b )
        valueHelper ( i, k, v ) =
            ( i, k, fn k v )

        dictHelper : Node k a -> Node k b
        dictHelper node =
            case node of
                Leaf idx h k v ->
                    Leaf idx h k <| fn k v

                SubTree bitmap nodes ->
                    SubTree bitmap <| JsArray.map dictHelper nodes

                Collision h triplets ->
                    Collision h <| List.map valueHelper triplets

        intDictHelper : IntDictNode k a -> IntDictNode k b
        intDictHelper node =
            case node of
                IntLeaves bitmap leaves ->
                    IntLeaves bitmap <| JsArray.map valueHelper leaves

                IntSubTree bitmap subTree ->
                    IntSubTree bitmap <| JsArray.map intDictHelper subTree
    in
    Dict
        rootBitmap
        (JsArray.map dictHelper rootNodes)
        { nextIndex = rootTriplets.nextIndex
        , size = rootTriplets.size
        , startShift = rootTriplets.startShift
        , tree = JsArray.map intDictHelper rootTriplets.tree
        , treeBitmap = rootTriplets.treeBitmap
        , tail = JsArray.map valueHelper rootTriplets.tail
        , tailBitmap = rootTriplets.tailBitmap
        }


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter predicate (Dict _ _ rootTriplets) =
    let
        helper : ( Int, k, v ) -> Dict k v -> Dict k v
        helper ( hash, key, value ) ((Dict bitmap nodes triplets) as dict) =
            if predicate key value then
                insertHelp 0 hash key value bitmap nodes triplets

            else
                dict
    in
    intDictFoldl helper empty rootTriplets


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> Dict k v -> ( Dict k v, Dict k v )
partition predicate (Dict _ _ rootTriplets) =
    let
        helper : ( Int, k, v ) -> ( Dict k v, Dict k v ) -> ( Dict k v, Dict k v )
        helper ( hash, key, value ) ( t1, t2 ) =
            let
                (Dict bitmap1 nodes1 triplets1) =
                    t1

                (Dict bitmap2 nodes2 triplets2) =
                    t2
            in
            if predicate key value then
                ( insertHelp 0 hash key value bitmap1 nodes1 triplets1, t2 )

            else
                ( t1, insertHelp 0 hash key value bitmap2 nodes2 triplets2 )
    in
    intDictFoldl helper ( empty, empty ) rootTriplets



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict k v -> Dict k v -> Dict k v
union (Dict _ _ t1Triplets) ((Dict _ _ t2Triplets) as t2) =
    let
        unionSize =
            t1Triplets.size + t2Triplets.size

        init =
            if unionSize >= rebuildThreshold then
                intDictFoldl helper empty t2Triplets

            else
                t2

        helper : ( Int, k, v ) -> Dict k v -> Dict k v
        helper ( hash, key, value ) (Dict bitmap nodes triplets) =
            insertHelp 0 hash key value bitmap nodes triplets
    in
    intDictFoldl helper init t1Triplets


{-| Keep a key-value pair when its key appears in the second Dictionary.
Preference is given to values in the first Dictionary.
-}
intersect : Dict k v -> Dict k v -> Dict k v
intersect (Dict _ _ t1Triplets) (Dict t2Bitmap t2Nodes _) =
    let
        helper : ( Int, k, v ) -> Dict k v -> Dict k v
        helper ( hash, key, value ) ((Dict bitmap nodes triplets) as dict) =
            case getHelp 0 hash key t2Bitmap t2Nodes of
                Nothing ->
                    dict

                Just _ ->
                    insertHelp 0 hash key value bitmap nodes triplets
    in
    intDictFoldl helper empty t1Triplets


{-| Keep a key-value pair when its key does not appear in the second Dictionary.
-}
diff : Dict k v -> Dict k v -> Dict k v
diff t1 (Dict _ _ t2Triplets) =
    let
        helper : ( Int, k, v ) -> Dict k v -> Dict k v
        helper ( hash, key, value ) ((Dict bitmap nodes triplets) as dict) =
            removeHelp 0 hash key bitmap nodes triplets
    in
    intDictFoldl helper t1 t2Triplets



{- INT DICT

   This is an internal structure which is only used for remembering the insertion
   order of key-value pairs. We store the hash, key and value here as well, so
   fold operations become as fast as possible.

   The structure is sort of a combination of the above hash map and `Array`. This
   structure allows removing a value stored at a certain index, without changing the
   index of every other key-value pair. Unfortunetly, this does mean that the structure
   has to be rebuilt at some point, but that happens so rarely that it can be ignored.
-}


type alias IntDict k v =
    { nextIndex : Int
    , size : Int
    , startShift : Int
    , tree : IntDictNodeArray k v
    , treeBitmap : Int
    , tail : JsArray ( Int, k, v )
    , tailBitmap : Int
    }


type alias IntDictNodeArray k v =
    JsArray (IntDictNode k v)


type IntDictNode k v
    = IntLeaves Int (JsArray ( Int, k, v ))
    | IntSubTree Int (IntDictNodeArray k v)


intDictEmpty : IntDict k v
intDictEmpty =
    { nextIndex = 0
    , size = 0
    , startShift = shiftStep
    , tree = JsArray.empty
    , treeBitmap = 0
    , tail = JsArray.empty
    , tailBitmap = 0
    }


intDictPush : Int -> k -> v -> IntDict k v -> IntDict k v
intDictPush hash key value dict =
    let
        newTail =
            JsArray.push ( hash, key, value ) dict.tail

        newSize =
            dict.size + 1

        newTailLength =
            JsArray.length newTail

        bitmapShift =
            Bitwise.and bitMask dict.nextIndex

        newTailBitmap =
            Bitwise.or dict.tailBitmap (Bitwise.shiftLeftBy bitmapShift 1)
    in
    if newTailLength == branchFactor then
        let
            overflow =
                Bitwise.shiftRightZfBy shiftStep newSize > Bitwise.shiftLeftBy dict.startShift 1

            tailNode =
                IntLeaves newTailBitmap newTail
        in
        if overflow then
            let
                newShift =
                    dict.startShift + shiftStep

                ( newTreeBitmap, newTree ) =
                    IntSubTree dict.treeBitmap dict.tree
                        |> JsArray.singleton
                        |> intDictInsertTailInTree newShift dict.nextIndex tailNode 1
            in
            { nextIndex = dict.nextIndex + 1
            , size = newSize
            , startShift = newShift
            , tree = newTree
            , treeBitmap = newTreeBitmap
            , tail = JsArray.empty
            , tailBitmap = 0
            }

        else
            let
                ( newTreeBitmap, newTree ) =
                    intDictInsertTailInTree
                        dict.startShift
                        dict.nextIndex
                        tailNode
                        dict.treeBitmap
                        dict.tree
            in
            { nextIndex = dict.nextIndex + 1
            , size = newSize
            , startShift = dict.startShift
            , tree = newTree
            , treeBitmap = newTreeBitmap
            , tail = JsArray.empty
            , tailBitmap = 0
            }

    else
        { nextIndex = dict.nextIndex + 1
        , size = newSize
        , startShift = dict.startShift
        , tree = dict.tree
        , treeBitmap = dict.treeBitmap
        , tail = newTail
        , tailBitmap = newTailBitmap
        }


intDictInsertTailInTree :
    Int
    -> Int
    -> IntDictNode k v
    -> Int
    -> IntDictNodeArray k v
    -> ( Int, IntDictNodeArray k v )
intDictInsertTailInTree shift index tail bitmap tree =
    let
        pos =
            Bitwise.and bitMask (Bitwise.shiftRightZfBy shift index)
    in
    if pos >= JsArray.length tree then
        let
            mask =
                Bitwise.shiftLeftBy pos 1

            newBitmap =
                Bitwise.or mask bitmap
        in
        if shift == shiftStep then
            ( newBitmap
            , JsArray.push tail tree
            )

        else
            let
                ( subBitmap, subTree ) =
                    intDictInsertTailInTree (shift - shiftStep) index tail 0 JsArray.empty
            in
            ( newBitmap
            , JsArray.push (IntSubTree subBitmap subTree) tree
            )

    else
        case JsArray.unsafeGet pos tree of
            IntSubTree subBitmap subTree ->
                let
                    ( newSubBitmap, newSubTree ) =
                        intDictInsertTailInTree (shift - shiftStep) index tail subBitmap subTree
                in
                ( bitmap
                , JsArray.unsafeSet pos (IntSubTree newSubBitmap newSubTree) tree
                )

            IntLeaves _ _ ->
                -- Should not happen as IntLeaves are at the bottom level,
                -- so the true branch of the if statement should've been executed
                ( bitmap, tree )


{-| Will only ever be called when we know the index, so we don't need
to do bounds checking.
-}
intDictSet : Int -> Int -> k -> v -> IntDict k v -> IntDict k v
intDictSet index hash key value dict =
    if index >= Bitwise.and invertedBitMask dict.nextIndex then
        let
            uncompressedIdx =
                Bitwise.and bitMask index

            comIdx =
                compressedIndex uncompressedIdx dict.tailBitmap
        in
        { nextIndex = dict.nextIndex
        , size = dict.size
        , startShift = dict.startShift
        , tree = dict.tree
        , treeBitmap = dict.treeBitmap
        , tail = JsArray.unsafeSet comIdx ( hash, key, value ) dict.tail
        , tailBitmap = dict.tailBitmap
        }

    else
        let
            returned =
                intDictSetHelp dict.startShift
                    index
                    ( hash, key, value )
                    dict.treeBitmap
                    dict.tree
        in
        case returned of
            IntSubTree subBitmap subTree ->
                { nextIndex = dict.nextIndex
                , size = dict.size
                , startShift = dict.startShift
                , tree = subTree
                , treeBitmap = subBitmap
                , tail = dict.tail
                , tailBitmap = dict.tailBitmap
                }

            IntLeaves _ _ ->
                -- Cannot happen
                dict


intDictSetHelp :
    Int
    -> Int
    -> ( Int, k, v )
    -> Int
    -> IntDictNodeArray k v
    -> IntDictNode k v
intDictSetHelp shift index value bitmap tree =
    let
        uncompressedIdx =
            Bitwise.and bitMask (Bitwise.shiftRightZfBy shift index)

        comIdx =
            compressedIndex uncompressedIdx bitmap
    in
    case JsArray.unsafeGet comIdx tree of
        IntSubTree subBitmap subTree ->
            let
                newSub =
                    intDictSetHelp
                        (shift - shiftStep)
                        index
                        value
                        subBitmap
                        subTree
            in
            JsArray.unsafeSet comIdx newSub tree
                |> IntSubTree subBitmap

        IntLeaves subBitmap subTree ->
            let
                leafUncompressedIdx =
                    Bitwise.and bitMask index

                leafComIdx =
                    compressedIndex leafUncompressedIdx subBitmap

                newSub =
                    JsArray.unsafeSet leafComIdx value subTree
                        |> IntLeaves subBitmap
            in
            JsArray.unsafeSet comIdx newSub tree
                |> IntSubTree bitmap


intDictRemove : Int -> IntDict k v -> IntDict k v
intDictRemove index dict =
    if index >= Bitwise.and invertedBitMask dict.nextIndex then
        let
            uncompressedIndex =
                Bitwise.and bitMask index

            compIdx =
                compressedIndex uncompressedIndex dict.tailBitmap

            mask =
                Bitwise.shiftLeftBy uncompressedIndex 1
        in
        { nextIndex = dict.nextIndex
        , size = dict.size - 1
        , startShift = dict.startShift
        , tree = dict.tree
        , treeBitmap = dict.treeBitmap
        , tail = JsArray.removeIndex compIdx dict.tail
        , tailBitmap = Bitwise.xor dict.tailBitmap mask
        }

    else
        case intDictRemoveHelper dict.startShift index dict.treeBitmap dict.tree of
            IntSubTree subBitmap subTree ->
                { nextIndex = dict.nextIndex
                , size = dict.size - 1
                , startShift = dict.startShift
                , tree = subTree
                , treeBitmap = subBitmap
                , tail = dict.tail
                , tailBitmap = dict.tailBitmap
                }

            IntLeaves _ _ ->
                -- Cannot happen
                dict


intDictRemoveHelper : Int -> Int -> Int -> IntDictNodeArray k v -> IntDictNode k v
intDictRemoveHelper shift index bitmap tree =
    let
        uncompressedIndex =
            Bitwise.and bitMask (Bitwise.shiftRightZfBy shift index)

        compIdx =
            compressedIndex uncompressedIndex bitmap
    in
    case JsArray.unsafeGet compIdx tree of
        IntSubTree subBitmap subTree ->
            let
                newSub =
                    intDictRemoveHelper (shift - shiftStep) index subBitmap subTree
            in
            JsArray.unsafeSet compIdx newSub tree
                |> IntSubTree bitmap

        IntLeaves subBitmap subTree ->
            let
                leafUncompressedIndex =
                    Bitwise.and bitMask index

                leafCompIdx =
                    compressedIndex leafUncompressedIndex subBitmap

                mask =
                    Bitwise.shiftLeftBy leafUncompressedIndex 1

                newSubBitmap =
                    Bitwise.xor subBitmap mask

                newSub =
                    JsArray.removeIndex leafCompIdx subTree
                        |> IntLeaves newSubBitmap
            in
            JsArray.unsafeSet compIdx newSub tree
                |> IntSubTree bitmap


intDictFoldl : (( Int, k, v ) -> acc -> acc) -> acc -> IntDict k v -> acc
intDictFoldl func baseCase dict =
    let
        helper : IntDictNode k v -> acc -> acc
        helper node acc =
            case node of
                IntLeaves _ subTree ->
                    JsArray.foldl func acc subTree

                IntSubTree _ subTree ->
                    JsArray.foldl helper acc subTree
    in
    JsArray.foldl func (JsArray.foldl helper baseCase dict.tree) dict.tail


intDictFoldr : (( Int, k, v ) -> acc -> acc) -> acc -> IntDict k v -> acc
intDictFoldr func baseCase dict =
    let
        helper : IntDictNode k v -> acc -> acc
        helper node acc =
            case node of
                IntLeaves _ subTree ->
                    JsArray.foldr func acc subTree

                IntSubTree _ subTree ->
                    JsArray.foldr helper acc subTree
    in
    JsArray.foldr helper (JsArray.foldr func baseCase dict.tail) dict.tree
