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

import Array.Hamt as Array exposing (Array)
import Bitwise
import Dict as OrderedDict
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
    = Dict Int (NodeArray k v) Int (OrderedDict.Dict Int ( Int, k, v ))


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


{-| How many bits represents the branching factor (32). Read Array documentation for
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


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Dict 0 JsArray.empty 0 OrderedDict.empty


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
size (Dict _ _ _ values) =
    OrderedDict.size values


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
    -- A bitmap can contain 32 bits. 1 bits represents a stored value.
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
    -> OrderedDict.Dict Int ( Int, k, v )
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
            Leaf xHash xIdx xKey xVal ->
                if xHash == hash then
                    if xKey == key then
                        Dict
                            bitmap
                            (setByIndex mask comIdx (Leaf xHash xIdx xKey value) bitmap nodes)
                            nextIndex
                            (OrderedDict.insert xIdx ( hash, key, value ) triplets)

                    else
                        let
                            element =
                                Collision hash [ ( nextIndex, key, value ), ( xIdx, xKey, xVal ) ]
                        in
                        Dict
                            bitmap
                            (setByIndex mask comIdx element bitmap nodes)
                            (nextIndex + 1)
                            (OrderedDict.insert nextIndex ( hash, key, value ) triplets)

                else
                    let
                        (Dict firstBitmap firstNodes _ _) =
                            insertHelp newShift xHash xKey xVal 0 JsArray.empty xIdx triplets

                        (Dict secondBitmap secondNodes newNextIndex newTriplets) =
                            insertHelp
                                newShift
                                hash
                                key
                                value
                                firstBitmap
                                firstNodes
                                nextIndex
                                triplets

                        subTree =
                            SubTree secondBitmap secondNodes
                    in
                    Dict
                        (Bitwise.or bitmap mask)
                        (setByIndex mask comIdx subTree bitmap nodes)
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
                    (Bitwise.or bitmap mask)
                    (setByIndex mask comIdx newSub bitmap nodes)
                    newIdx
                    newTriplets

            (Collision xHash pairs) as currValue ->
                if xHash == hash then
                    case List.find (\( _, k, _ ) -> k == key) pairs of
                        Just ( existingIdx, _, _ ) ->
                            -- TODO: Need to update the value in collision list
                            Dict
                                bitmap
                                nodes
                                nextIndex
                                (OrderedDict.insert existingIdx ( hash, key, value ) triplets)

                        Nothing ->
                            let
                                newNodes =
                                    setByIndex
                                        mask
                                        comIdx
                                        (Collision hash (( nextIndex, key, value ) :: pairs))
                                        bitmap
                                        nodes
                            in
                            Dict
                                bitmap
                                newNodes
                                (nextIndex + 1)
                                (OrderedDict.insert nextIndex ( hash, key, value ) triplets)

                else
                    let
                        collisionPos =
                            Bitwise.and bitMask (Bitwise.shiftRightZfBy shift hash)

                        collisionNodePos =
                            compressedIndex collisionPos 0

                        collisionMask =
                            Bitwise.shiftLeftBy collisionPos 1

                        collisionNodes =
                            setByIndex
                                collisionMask
                                collisionNodePos
                                currValue
                                0
                                JsArray.empty

                        (Dict subBitmap subNodes subIndex subTriplets) =
                            insertHelp
                                newShift
                                hash
                                key
                                value
                                (Bitwise.or 0 collisionMask)
                                collisionNodes
                                nextIndex
                                triplets
                    in
                    Dict
                        (Bitwise.or bitmap mask)
                        (setByIndex mask comIdx (SubTree subBitmap subNodes) bitmap nodes)
                        subIndex
                        subTriplets

    else
        Dict
            (Bitwise.or bitmap mask)
            (setByIndex mask comIdx (Leaf hash nextIndex key value) bitmap nodes)
            (nextIndex + 1)
            (OrderedDict.insert nextIndex ( hash, key, value ) triplets)


{-| Insert a Node at the given index, returning an updated Dict.
-}
setByIndex : Int -> Int -> Node k v -> Int -> NodeArray k v -> NodeArray k v
setByIndex mask comIdx val bitmap nodes =
    if Bitwise.and bitmap mask == mask then
        JsArray.unsafeSet comIdx val nodes

    else
        JsArray.unsafeInsert comIdx val nodes


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> Dict k v -> Dict k v
remove key (Dict bitmap nodes counter values) =
    let
        ( removeIdx, newBitmap, newNodes ) =
            removeHelp 0 (FNV.hash key) key bitmap nodes

        newValues =
            if removeIdx == -1 then
                values

            else
                OrderedDict.remove removeIdx values
    in
    Dict newBitmap newNodes counter newValues


removeHelp : Int -> Int -> k -> Int -> NodeArray k v -> ( Int, Int, NodeArray k v )
removeHelp shift hash key bitmap nodes =
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
                    ( eIdx
                    , Bitwise.xor bitmap mask
                    , JsArray.removeIndex compIdx nodes
                    )

                else
                    ( -1
                    , bitmap
                    , nodes
                    )

            SubTree subBitmap subNodes ->
                let
                    ( removeIdx, newSubBitmap, newSubNodes ) =
                        removeHelp (shift + shiftStep) hash key subBitmap subNodes
                in
                ( removeIdx
                , Bitwise.or bitmap mask
                , setByIndex mask
                    compIdx
                    (SubTree newSubBitmap newSubNodes)
                    bitmap
                    nodes
                )

            Collision _ vals ->
                let
                    removeIdx =
                        List.find (\( _, k, _ ) -> k == key) vals
                            |> Maybe.map (\( idx, _, _ ) -> idx)
                            |> Maybe.withDefault -1

                    newCollision =
                        List.filter (\( _, k, _ ) -> k /= key) vals
                in
                case newCollision of
                    [] ->
                        ( removeIdx
                        , Bitwise.xor bitmap mask
                        , JsArray.removeIndex compIdx nodes
                        )

                    ( eIdx, eKey, eVal ) :: [] ->
                        ( removeIdx
                        , Bitwise.or bitmap mask
                        , setByIndex mask compIdx (Leaf hash eIdx eKey eVal) bitmap nodes
                        )

                    _ ->
                        ( removeIdx
                        , Bitwise.or bitmap mask
                        , setByIndex mask
                            compIdx
                            (Collision hash newCollision)
                            bitmap
                            nodes
                        )

    else
        ( -1
        , bitmap
        , nodes
        )


{-| Update the value of a dictionary for a specific key with a given function.
The given function gets the current value as a parameter and its return value
determines if the value is updated or removed. New key-value pairs can be
inserted too.
-}
update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key fn ((Dict bitmap nodes nextIndex triplets) as dict) =
    let
        hash =
            FNV.hash key
    in
    case fn (getHelp 0 hash key bitmap nodes) of
        Nothing ->
            remove key dict

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
    OrderedDict.foldr (\_ ( _, k, v ) acc -> ( k, v ) :: acc) [] triplets


{-| Get all of the keys in a dictionary.

       keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]

-}
keys : Dict k v -> List k
keys (Dict _ _ _ triplets) =
    OrderedDict.foldr (\_ ( _, k, _ ) acc -> k :: acc) [] triplets


{-| Get all of the values in a dictionary as a List.

       values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]

-}
values : Dict k v -> List v
values (Dict _ _ _ triplets) =
    OrderedDict.foldr (\_ ( _, _, v ) acc -> v :: acc) [] triplets



-- TRANSFORM


{-| Fold over the key-value pairs in a dictionary.
-}
foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
foldl fn acc ((Dict _ _ _ keys) as dict) =
    let
        helper : Int -> ( Int, k, v ) -> b -> b
        helper idx ( _, key, value ) acc =
            fn key value acc
    in
    OrderedDict.foldl helper acc keys


foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr fn acc ((Dict _ _ _ triplets) as dict) =
    let
        helper : Int -> ( Int, k, v ) -> b -> b
        helper _ ( _, key, value ) acc =
            fn key value acc
    in
    OrderedDict.foldr helper acc triplets


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map fn (Dict _ _ _ rootTriplets) =
    let
        helper : Int -> ( Int, k, a ) -> Dict k b -> Dict k b
        helper _ ( hash, key, value ) (Dict bitmap nodes nextIndex triplets) =
            insertHelp 0 hash key (fn key value) bitmap nodes nextIndex triplets
    in
    OrderedDict.foldl helper empty rootTriplets


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter predicate (Dict _ _ _ rootTriplets) =
    let
        helper : Int -> ( Int, k, v ) -> Dict k v -> Dict k v
        helper _ ( hash, key, value ) ((Dict bitmap nodes nextIndex triplets) as dict) =
            if predicate key value then
                insertHelp 0 hash key value bitmap nodes nextIndex triplets

            else
                dict
    in
    OrderedDict.foldl helper empty rootTriplets


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> Dict k v -> ( Dict k v, Dict k v )
partition predicate (Dict _ _ _ rootTriplets) =
    let
        helper : Int -> ( Int, k, v ) -> ( Dict k v, Dict k v ) -> ( Dict k v, Dict k v )
        helper _ ( hash, key, value ) ( (Dict bitmap1 nodes1 nextIndex1 triplets1) as t1, (Dict bitmap2 nodes2 nextIndex2 triplets2) as t2 ) =
            if predicate key value then
                ( insertHelp 0 hash key value bitmap1 nodes1 nextIndex1 triplets1, t2 )

            else
                ( t1, insertHelp 0 hash key value bitmap2 nodes2 nextIndex2 triplets2 )
    in
    OrderedDict.foldl helper ( empty, empty ) rootTriplets



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict k v -> Dict k v -> Dict k v
union (Dict _ _ _ t1Triplets) t2 =
    let
        helper : Int -> ( Int, k, v ) -> Dict k v -> Dict k v
        helper _ ( hash, key, value ) (Dict bitmap nodes nextIndex triplets) =
            insertHelp 0 hash key value bitmap nodes nextIndex triplets
    in
    OrderedDict.foldl helper t2 t1Triplets


{-| Keep a key-value pair when its key appears in the second Dictionary.
Preference is given to values in the first Dictionary.
-}
intersect : Dict k v -> Dict k v -> Dict k v
intersect (Dict _ _ _ t1Triplets) t2 =
    let
        helper : Int -> ( Int, k, v ) -> Dict k v -> Dict k v
        helper _ ( hash, key, value ) ((Dict bitmap nodes nextIndex triplets) as dict) =
            case get key t2 of
                Just _ ->
                    insertHelp 0 hash key value bitmap nodes nextIndex triplets

                Nothing ->
                    dict
    in
    OrderedDict.foldl helper empty t1Triplets


{-| Keep a key-value pair when its key does not appear in the second Dictionary.
-}
diff : Dict k v -> Dict k v -> Dict k v
diff t1 t2 =
    foldl (\k _ t -> remove k t) t1 t2
