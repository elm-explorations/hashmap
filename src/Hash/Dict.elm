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
-}
type
    Dict k v
    -- The first argument is a bitmap (32 bits). If a bit is set, it means
    -- that a value is stored at that index (NodeArray is 32 in size).
    -- NodeArray contains Nodes (look below).
    --
    -- A Dict is essentially an Array. We use a hash function to convert the
    -- key to an Int, which we use as an index.
    -- Since the hash function doesn't return sequential values, we use a
    -- bitmap to compress the NodeArray. The actual index of a key depends
    -- on how many bits are set before the actual index in the bitmap.
    --
    -- Example: The hash for key '0' tells us that the element should be
    -- stored at index 9. In the bitmap, there are no set bits (ones) before
    -- index 9, so we actually store the key-value pair at index 0.
    = Dict Int (NodeArray k v) Int (IntDict k v)


type alias NodeArray k v =
    JsArray (Node k v)


type
    Node k v
    -- In addition to Leaf's and SubTree's (read the code for Array for more info)
    -- we also have to consider Collisions. A hash function can potentially return
    -- the same hash for two different keys. In that case we store the key-value
    -- pairs in a list, and need to run an equality check when retrieving, or removing,
    -- said key. If we use a hash-function which causes a lot of collisions, our Dict
    -- essentially degrades into a List.
    = Leaf Int Int k v
    | SubTree Int (NodeArray k v)
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


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Dict 0 JsArray.empty 0 intDictEmpty


{-| Create a dictionary with one key-value pair.
-}
singleton : k -> v -> Dict k v
singleton key val =
    insert key val empty


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : Dict k v -> Bool
isEmpty (Dict bitmap _ _ _) =
    bitmap == 0


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict k v -> Int
size (Dict _ _ _ triplets) =
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
get key (Dict bitmap nodes _ values) =
    getHelp 0 (FNV.hash key) key bitmap nodes


getHelp : Int -> Int -> k -> Int -> NodeArray k v -> Maybe v
getHelp shift hash key bitmap nodes =
    let
        idx =
            Bitwise.and bitMask (Bitwise.shiftRightZfBy shift hash)

        mask =
            Bitwise.shiftLeftBy idx 1
    in
    if Bitwise.and bitmap mask == mask then
        case JsArray.unsafeGet (compressedIndex idx bitmap) nodes of
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
compressedIndex idx bitmap =
    -- The NodeArray at each level of a tree can be, at most, 32 in size.
    -- A bitmap can contain 32 bits. 1 bit represents a stored value.
    -- The compressed index is the number of elements to the left of the
    -- idx bit.
    let
        relevantBits =
            Bitwise.shiftLeftBy 1 (Bitwise.shiftLeftBy (31 - idx) bitmap)

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
member key (Dict bitmap nodes _ _) =
    getHelp 0 (FNV.hash key) key bitmap nodes /= Nothing


{-| Insert a key-value pair into the dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> Dict k v -> Dict k v
insert key value (Dict bitmap nodes nextIndex triplets) =
    insertHelp 0 (FNV.hash key) key value bitmap nodes nextIndex triplets


insertHelp :
    Int
    -> Int
    -> k
    -> v
    -> Int
    -> NodeArray k v
    -> Int
    -> IntDict k v
    -> Dict k v
insertHelp shift hash key value bitmap nodes nextIndex triplets =
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
                            nextIndex
                            (intDictSet xIdx hash key value triplets)

                    else
                        let
                            element =
                                Collision hash [ ( nextIndex, key, value ), ( xIdx, xKey, xVal ) ]
                        in
                        Dict
                            bitmap
                            (JsArray.unsafeSet comIdx element nodes)
                            (nextIndex + 1)
                            (intDictPush hash key value triplets)

                else
                    let
                        subIdx =
                            Bitwise.and bitMask (Bitwise.shiftRightZfBy newShift xHash)

                        (Dict secondBitmap secondNodes newNextIndex newTriplets) =
                            insertHelp
                                newShift
                                hash
                                key
                                value
                                (Bitwise.shiftLeftBy subIdx 1)
                                (JsArray.singleton node)
                                nextIndex
                                triplets

                        subTree =
                            SubTree secondBitmap secondNodes
                    in
                    Dict
                        bitmap
                        (JsArray.unsafeSet comIdx subTree nodes)
                        newNextIndex
                        newTriplets

            SubTree subBitmap subNodes ->
                let
                    (Dict newSubBitmap newSubNodes newIdx newTriplets) =
                        insertHelp newShift hash key value subBitmap subNodes nextIndex triplets

                    newSub =
                        SubTree newSubBitmap newSubNodes
                in
                Dict
                    bitmap
                    (JsArray.unsafeSet comIdx newSub nodes)
                    newIdx
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
                                nextIndex
                                (intDictSet existingIdx hash key value triplets)

                        Nothing ->
                            let
                                updated =
                                    ( nextIndex, key, value ) :: pairs
                            in
                            Dict
                                bitmap
                                (JsArray.unsafeSet comIdx (Collision hash updated) nodes)
                                (nextIndex + 1)
                                (intDictPush hash key value triplets)

                else
                    let
                        collisionPos =
                            Bitwise.and bitMask (Bitwise.shiftRightZfBy newShift hash)

                        collisionMask =
                            Bitwise.shiftLeftBy collisionPos 1

                        (Dict subBitmap subNodes subIndex subTriplets) =
                            insertHelp
                                newShift
                                hash
                                key
                                value
                                (Bitwise.shiftLeftBy collisionPos 1)
                                (JsArray.singleton currValue)
                                nextIndex
                                triplets
                    in
                    Dict
                        bitmap
                        (JsArray.unsafeSet comIdx (SubTree subBitmap subNodes) nodes)
                        subIndex
                        subTriplets

    else
        Dict
            (Bitwise.or bitmap mask)
            (JsArray.unsafeInsert comIdx (Leaf hash nextIndex key value) nodes)
            (nextIndex + 1)
            (intDictPush hash key value triplets)


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> Dict k v -> Dict k v
remove key (Dict bitmap nodes nextIndex triplets) =
    removeHelp 0 (FNV.hash key) key bitmap nodes nextIndex triplets


removeHelp :
    Int
    -> Int
    -> k
    -> Int
    -> NodeArray k v
    -> Int
    -> IntDict k v
    -> Dict k v
removeHelp shift hash key bitmap nodes nextIndex triplets =
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
                        nextIndex
                        (intDictRemove eIdx triplets)

                else
                    Dict bitmap nodes nextIndex triplets

            SubTree subBitmap subNodes ->
                let
                    (Dict newSubBitmap newSubNodes _ newTriplets) =
                        removeHelp
                            (shift + shiftStep)
                            hash
                            key
                            subBitmap
                            subNodes
                            nextIndex
                            triplets
                in
                if newSubBitmap == 0 then
                    Dict
                        (Bitwise.xor bitmap mask)
                        (JsArray.removeIndex compIdx nodes)
                        nextIndex
                        newTriplets

                else
                    Dict
                        bitmap
                        (JsArray.unsafeSet compIdx (SubTree newSubBitmap newSubNodes) nodes)
                        nextIndex
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
                            nextIndex
                            newTriplets

                    ( eIdx, eKey, eVal ) :: [] ->
                        Dict
                            bitmap
                            (JsArray.unsafeSet compIdx (Leaf hash eIdx eKey eVal) nodes)
                            nextIndex
                            newTriplets

                    _ ->
                        Dict
                            bitmap
                            (JsArray.unsafeSet compIdx (Collision hash newCollision) nodes)
                            nextIndex
                            newTriplets

    else
        Dict
            bitmap
            nodes
            nextIndex
            triplets


{-| Update the value of a dictionary for a specific key with a given function.
The given function gets the current value as a parameter and its return value
determines if the value is updated or removed. New key-value pairs can be
inserted too.
-}
update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key fn (Dict bitmap nodes nextIndex triplets) =
    let
        hash =
            FNV.hash key
    in
    case fn (getHelp 0 hash key bitmap nodes) of
        Nothing ->
            removeHelp 0 hash key bitmap nodes nextIndex triplets

        Just value ->
            insertHelp 0 hash key value bitmap nodes nextIndex triplets



-- LISTS


{-| Convert an association list into a dictionary.
-}
fromList : List ( k, v ) -> Dict k v
fromList list =
    List.foldl (\( key, value ) acc -> insert key value acc) empty list


{-| Convert a dictionary into an association list of key-value pairs.
-}
toList : Dict k v -> List ( k, v )
toList (Dict _ _ _ triplets) =
    let
        helper : Int -> k -> v -> List ( k, v ) -> List ( k, v )
        helper _ key value acc =
            ( key, value ) :: acc
    in
    intDictFoldr helper [] triplets


{-| Get all of the keys in a dictionary.

       keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]

-}
keys : Dict k v -> List k
keys (Dict _ _ _ triplets) =
    let
        helper : Int -> k -> v -> List k -> List k
        helper _ key _ acc =
            key :: acc
    in
    intDictFoldr helper [] triplets


{-| Get all of the values in a dictionary as a List.

       values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]

-}
values : Dict k v -> List v
values (Dict _ _ _ triplets) =
    let
        helper : Int -> k -> v -> List v -> List v
        helper _ _ value acc =
            value :: acc
    in
    intDictFoldr helper [] triplets



-- TRANSFORM


{-| Fold over the key-value pairs in a dictionary.
-}
foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
foldl fn acc (Dict _ _ _ triplets) =
    let
        helper : Int -> k -> v -> b -> b
        helper _ key value acc =
            fn key value acc
    in
    intDictFoldl helper acc triplets


foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr fn acc (Dict _ _ _ triplets) =
    let
        helper : Int -> k -> v -> b -> b
        helper _ key value acc =
            fn key value acc
    in
    intDictFoldr helper acc triplets


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map fn (Dict _ _ _ rootTriplets) =
    let
        helper : Int -> k -> a -> Dict k b -> Dict k b
        helper hash key value ((Dict bitmap nodes nextIndex triplets) as dict) =
            insertHelp 0 hash key (fn key value) bitmap nodes nextIndex triplets
    in
    intDictFoldl helper empty rootTriplets


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter predicate (Dict _ _ _ rootTriplets) =
    let
        helper : Int -> k -> v -> Dict k v -> Dict k v
        helper hash key value ((Dict bitmap nodes nextIndex triplets) as dict) =
            if predicate key value then
                insertHelp 0 hash key value bitmap nodes nextIndex triplets

            else
                dict
    in
    intDictFoldl helper empty rootTriplets


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> Dict k v -> ( Dict k v, Dict k v )
partition predicate (Dict _ _ _ rootTriplets) =
    let
        helper : Int -> k -> v -> ( Dict k v, Dict k v ) -> ( Dict k v, Dict k v )
        helper hash key value ( t1, t2 ) =
            let
                (Dict bitmap1 nodes1 nextIndex1 triplets1) =
                    t1

                (Dict bitmap2 nodes2 nextIndex2 triplets2) =
                    t2
            in
            if predicate key value then
                ( insertHelp 0 hash key value bitmap1 nodes1 nextIndex1 triplets1, t2 )

            else
                ( t1, insertHelp 0 hash key value bitmap2 nodes2 nextIndex2 triplets2 )
    in
    intDictFoldl helper ( empty, empty ) rootTriplets



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict k v -> Dict k v -> Dict k v
union (Dict _ _ _ t1Triplets) t2 =
    let
        helper : Int -> k -> v -> Dict k v -> Dict k v
        helper hash key value ((Dict bitmap nodes nextIndex triplets) as dict) =
            insertHelp 0 hash key value bitmap nodes nextIndex triplets
    in
    intDictFoldl helper t2 t1Triplets


{-| Keep a key-value pair when its key appears in the second Dictionary.
Preference is given to values in the first Dictionary.
-}
intersect : Dict k v -> Dict k v -> Dict k v
intersect (Dict _ _ _ t1Triplets) (Dict t2Bitmap t2Nodes _ _) =
    let
        helper : Int -> k -> v -> Dict k v -> Dict k v
        helper hash key value ((Dict bitmap nodes nextIndex triplets) as dict) =
            case getHelp 0 hash key t2Bitmap t2Nodes of
                Nothing ->
                    dict

                Just _ ->
                    insertHelp 0 hash key value bitmap nodes nextIndex triplets
    in
    intDictFoldl helper empty t1Triplets


{-| Keep a key-value pair when its key does not appear in the second Dictionary.
-}
diff : Dict k v -> Dict k v -> Dict k v
diff t1 (Dict _ _ _ t2Triplets) =
    let
        helper : Int -> k -> v -> Dict k v -> Dict k v
        helper hash key value ((Dict bitmap nodes nextIndex triplets) as dict) =
            removeHelp 0 hash key bitmap nodes nextIndex triplets
    in
    intDictFoldl helper t1 t2Triplets



{- INT DICT -}


type alias IntDict k v =
    { nextIndex : Int
    , size : Int
    , startShift : Int
    , tree : IntDictNodeArray k v
    , treeBitmap : Int
    , tail : IntDictNodeArray k v
    , tailBitmap : Int
    }


type alias IntDictNodeArray k v =
    JsArray (IntDictNode k v)


type IntDictNode k v
    = IntLeaf Int k v
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
            JsArray.push (IntLeaf hash key value) dict.tail

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

            tailSubTree =
                IntSubTree newTailBitmap newTail
        in
        if overflow then
            let
                newShift =
                    dict.startShift + shiftStep

                ( newTreeBitmap, newTree ) =
                    JsArray.singleton (IntSubTree dict.treeBitmap dict.tree)
                        |> intDictInsertTailInTree newShift dict.nextIndex tailSubTree dict.treeBitmap
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
                        tailSubTree
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

        newShift =
            shift - shiftStep
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
                    intDictInsertTailInTree newShift index tail 0 JsArray.empty
            in
            ( newBitmap
            , JsArray.push (IntSubTree subBitmap subTree) tree
            )

    else
        case JsArray.unsafeGet pos tree of
            IntSubTree subBitmap subTree ->
                let
                    ( newSubBitmap, newSubTree ) =
                        intDictInsertTailInTree newShift index tail subBitmap subTree
                in
                ( bitmap
                , JsArray.unsafeSet pos (IntSubTree newSubBitmap newSubTree) tree
                )

            IntLeaf _ _ _ ->
                -- Cannot happen
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
        , tail = JsArray.unsafeSet comIdx (IntLeaf hash key value) dict.tail
        , tailBitmap = dict.tailBitmap
        }

    else
        let
            returned =
                intDictSetHelp dict.startShift
                    index
                    (IntLeaf hash key value)
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

            -- Cannot happen.
            IntLeaf _ _ _ ->
                dict


intDictSetHelp :
    Int
    -> Int
    -> IntDictNode k v
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
                    intDictSetHelp (shift - shiftStep)
                        index
                        value
                        subBitmap
                        subTree
            in
            JsArray.unsafeSet comIdx newSub tree
                |> IntSubTree subBitmap

        IntLeaf _ _ _ ->
            JsArray.unsafeSet comIdx value tree
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

            -- Cannot happen.
            IntLeaf _ _ _ ->
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

        IntLeaf _ _ _ ->
            let
                mask =
                    Bitwise.shiftLeftBy uncompressedIndex 1

                newBitmap =
                    Bitwise.xor bitmap mask
            in
            JsArray.removeIndex compIdx tree
                |> IntSubTree newBitmap


intDictFoldl : (Int -> k -> v -> acc -> acc) -> acc -> IntDict k v -> acc
intDictFoldl func baseCase dict =
    let
        helper : IntDictNode k v -> acc -> acc
        helper node acc =
            case node of
                IntLeaf hash key value ->
                    func hash key value acc

                IntSubTree _ subTree ->
                    JsArray.foldl helper acc subTree
    in
    JsArray.foldl helper (JsArray.foldl helper baseCase dict.tree) dict.tail


intDictFoldr : (Int -> k -> v -> acc -> acc) -> acc -> IntDict k v -> acc
intDictFoldr func baseCase dict =
    let
        helper : IntDictNode k v -> acc -> acc
        helper node acc =
            case node of
                IntLeaf hash key value ->
                    func hash key value acc

                IntSubTree _ subTree ->
                    JsArray.foldr helper acc subTree
    in
    JsArray.foldr helper (JsArray.foldr helper baseCase dict.tail) dict.tree
