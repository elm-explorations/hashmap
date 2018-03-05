module Hash.Dict
    exposing
        ( Dict
        , empty
        , singleton
        , isEmpty
        , size
        , get
        , member
        , insert
        , update
        , remove
        , fromList
        , toList
        , keys
        , values
        , map
        , fold
        , filter
        , partition
        , union
        , intersect
        , diff
        )

{-| A dictionary mapping unique keys to values.


# Dictionary

@docs Dict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs get, isEmpty, member, size


# Combine

@docs union, intersect, diff, merge


# Lists

@docs toList, fromList, keys, values


# Transform

@docs map, fold, filter, partition

-}

import Bitwise
import List.Extra as List
import Hash.FNV as FNV
import Hash.JsArray as JsArray exposing (JsArray)


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type Dict k v
    = Dict Int (NodeArray k v)


type alias NodeArray k v =
    JsArray (Node k v)


type Node k v
    = Element Int k v
    | SubTree (Dict k v)
    | Collision Int (List ( k, v ))


valueByIndex : Int -> Int -> Dict k v -> Maybe (Node k v)
valueByIndex idx nodePos (Dict positionMap nodes) =
    let
        mask =
            Bitwise.shiftLeftBy idx 0x01

        hasValue =
            Bitwise.and positionMap mask == mask
    in
        if hasValue then
            Just <| JsArray.unsafeGet nodePos nodes
        else
            Nothing


setByIndex : Int -> Int -> Node k v -> Dict k v -> Dict k v
setByIndex idx nodePos val (Dict positionMap nodes) =
    let
        mask =
            Bitwise.shiftLeftBy idx 0x01

        alteredBitmap =
            Bitwise.or positionMap mask

        shouldReplace =
            Bitwise.and positionMap mask == mask

        newNodeArray =
            if shouldReplace then
                JsArray.unsafeSet nodePos val nodes
            else
                JsArray.unsafeInsert nodePos val nodes
    in
        Dict alteredBitmap newNodeArray


removeByIndex : Int -> Int -> Dict k v -> Dict k v
removeByIndex idx nodePos (Dict positionMap nodes) =
    let
        mask =
            Bitwise.shiftLeftBy idx 0x01

        alteredBitmap =
            Bitwise.xor positionMap mask
    in
        Dict alteredBitmap (JsArray.removeIndex nodePos nodes)


hashPositionWithShift : Int -> Int -> Int
hashPositionWithShift shift hash =
    Bitwise.and 0x1F <| Bitwise.shiftRightZfBy shift hash


nodePosition : Int -> Int -> Int
nodePosition idx posMap =
    countSetBits (Bitwise.shiftLeftBy 1 (Bitwise.shiftLeftBy (31 - idx) posMap))


{-| No idea how this works. Stole code from stack overflow.
-}
countSetBits : Int -> Int
countSetBits bitmap =
    let
        b1 =
            bitmap - (Bitwise.and (Bitwise.shiftRightZfBy 1 bitmap) 0x55555555)

        b2 =
            (Bitwise.and b1 0x33333333) + (Bitwise.and (Bitwise.shiftRightZfBy 2 b1) 0x33333333)
    in
        Bitwise.shiftRightZfBy 24 ((Bitwise.and (b2 + (Bitwise.shiftRightZfBy 4 b2)) 0x0F0F0F0F) * 0x01010101)


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Dict 0 JsArray.empty


{-| Create a dictionary with one key-value pair.
-}
singleton : k -> v -> Dict k v
singleton key val =
    insert key val empty


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : Dict k v -> Bool
isEmpty (Dict _ arr) =
    arr == JsArray.empty


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict k v -> Int
size dict =
    fold (\_ _ acc -> acc + 1) 0 dict


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : k -> Dict k v -> Maybe v
get key dict =
    getHelp 0 (FNV.hash key) key dict


getHelp : Int -> Int -> k -> Dict k v -> Maybe v
getHelp shift hash key (Dict positionMap nodes) =
    let
        pos =
            hashPositionWithShift shift hash

        mask =
            Bitwise.shiftLeftBy pos 0x01

        hasValue =
            Bitwise.and positionMap mask == mask
    in
        if hasValue then
            case JsArray.unsafeGet (nodePosition pos positionMap) nodes of
                Element _ eKey value ->
                    if key == eKey then
                        Just value
                    else
                        Nothing

                SubTree subNodes ->
                    getHelp (shift + 5) hash key subNodes

                Collision _ vals ->
                    Maybe.map Tuple.second
                        (List.find (\( k, _ ) -> k == key) vals)
        else
            Nothing


{-| Determine if a key is in a dictionary.
-}
member : k -> Dict k v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Insert a key-value pair into the dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> Dict k v -> Dict k v
insert key value dict =
    insertHelp 0 (FNV.hash key) key value dict


insertHelp : Int -> Int -> k -> v -> Dict k v -> Dict k v
insertHelp shift hash key val ((Dict positionMap _) as dict) =
    let
        pos =
            hashPositionWithShift shift hash

        blobPos =
            nodePosition pos positionMap

        newShift =
            shift + 5
    in
        case valueByIndex pos blobPos dict of
            Nothing ->
                setByIndex pos blobPos (Element hash key val) dict

            Just currValue ->
                case currValue of
                    Element xHash xKey xVal ->
                        if xHash == hash then
                            if xKey == key then
                                setByIndex pos blobPos (Element hash key val) dict
                            else
                                let
                                    element =
                                        Collision hash [ ( key, val ), ( xKey, xVal ) ]
                                in
                                    setByIndex pos blobPos element dict
                        else
                            let
                                subNodes =
                                    empty
                                        |> insertHelp newShift xHash xKey xVal
                                        |> insertHelp newShift hash key val
                                        |> SubTree
                            in
                                setByIndex pos blobPos subNodes dict

                    SubTree subNodes ->
                        let
                            sub =
                                insertHelp newShift hash key val subNodes
                        in
                            setByIndex pos blobPos (SubTree sub) dict

                    Collision xHash subNodes ->
                        if xHash == hash then
                            let
                                newNodes =
                                    ( key, val ) :: (List.filter (\( k, _ ) -> k /= key) subNodes)
                            in
                                setByIndex pos blobPos (Collision hash newNodes) dict
                        else
                            let
                                collisionPos =
                                    hashPositionWithShift newShift xHash

                                collisionBlobPos =
                                    nodePosition collisionPos 0

                                newNodes =
                                    empty
                                        |> setByIndex collisionPos collisionBlobPos currValue
                                        |> insertHelp newShift hash key val
                                        |> SubTree
                            in
                                setByIndex pos blobPos newNodes dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> Dict k v -> Dict k v
remove key dict =
    removeHelp 0 (FNV.hash key) key dict


removeHelp : Int -> Int -> k -> Dict k v -> Dict k v
removeHelp shift hash key ((Dict positionMap _) as dict) =
    let
        pos =
            hashPositionWithShift shift hash

        nodePos =
            nodePosition pos positionMap
    in
        case valueByIndex pos nodePos dict of
            Nothing ->
                dict

            Just node ->
                case node of
                    Element _ eKey value ->
                        if eKey == key then
                            removeByIndex pos nodePos dict
                        else
                            dict

                    SubTree subDict ->
                        let
                            newNodes =
                                removeHelp (shift + 5) hash key subDict
                        in
                            setByIndex pos nodePos (SubTree newNodes) dict

                    Collision _ vals ->
                        let
                            newCollision =
                                List.filter (\( k, _ ) -> k /= key) vals
                        in
                            case newCollision of
                                [] ->
                                    removeByIndex pos nodePos dict

                                ( eKey, eVal ) :: [] ->
                                    setByIndex pos nodePos (Element hash eKey eVal) dict

                                _ ->
                                    setByIndex pos nodePos (Collision hash newCollision) dict


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key fn dict =
    let
        hash =
            FNV.hash key
    in
        case fn <| getHelp 0 hash key dict of
            Nothing ->
                removeHelp 0 hash key dict

            Just val ->
                insertHelp 0 hash key val dict



-- LISTS


{-| Convert an association list into a dictionary.
-}
fromList : List ( k, v ) -> Dict k v
fromList list =
    List.foldl (\( key, value ) acc -> insert key value acc) empty list


{-| Convert a dictionary into an association list of key-value pairs.
-}
toList : Dict k v -> List ( k, v )
toList dict =
    fold (\k v acc -> ( k, v ) :: acc) [] dict


{-| Get all of the keys in a dictionary.

    keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]

-}
keys : Dict k v -> List k
keys dict =
    fold (\k _ acc -> k :: acc) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]

-}
values : Dict k v -> List v
values dict =
    fold (\_ v acc -> v :: acc) [] dict



-- TRANSFORM


{-| Fold over the key-value pairs in a dictionary.
-}
fold : (k -> v -> b -> b) -> b -> Dict k v -> b
fold fn acc (Dict _ arr) =
    JsArray.foldl
        (\node acc ->
            case node of
                Element _ key val ->
                    fn key val acc

                SubTree nodes ->
                    fold fn acc nodes

                Collision _ vals ->
                    let
                        colFold ( k, v ) acc =
                            fn k v acc
                    in
                        List.foldl colFold acc vals
        )
        acc
        arr


foldWithHash : (Int -> k -> v -> b -> b) -> b -> Dict k v -> b
foldWithHash fn acc (Dict _ arr) =
    JsArray.foldl
        (\node acc ->
            case node of
                Element hash key val ->
                    fn hash key val acc

                SubTree nodes ->
                    foldWithHash fn acc nodes

                Collision hash vals ->
                    let
                        colFold ( k, v ) acc =
                            fn hash k v acc
                    in
                        List.foldl colFold acc vals
        )
        acc
        arr


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map fn (Dict posMap nodes) =
    Dict posMap
        (JsArray.map
            (\node ->
                case node of
                    Element hash key val ->
                        Element hash key (fn key val)

                    SubTree subDict ->
                        SubTree (map fn subDict)

                    Collision hash vals ->
                        let
                            helper ( k, v ) =
                                ( k, fn k v )
                        in
                            Collision hash (List.map helper vals)
            )
            nodes
        )


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter predicate dict =
    let
        helper hash key value dict =
            if predicate key value then
                insertHelp 0 hash key value dict
            else
                dict
    in
        foldWithHash helper empty dict


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> Dict k v -> ( Dict k v, Dict k v )
partition predicate dict =
    let
        helper hash key value ( t1, t2 ) =
            if predicate key value then
                ( insertHelp 0 hash key value t1, t2 )
            else
                ( t1, insertHelp 0 hash key value t2 )
    in
        foldWithHash helper ( empty, empty ) dict



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict k v -> Dict k v -> Dict k v
union t1 t2 =
    foldWithHash (\h k v t -> insertHelp 0 h k v t) t2 t1


{-| Keep a key-value pair when its key appears in the second Dictionary.
Preference is given to values in the first Dictionary.
-}
intersect : Dict k v -> Dict k v -> Dict k v
intersect t1 t2 =
    let
        helper h k v t =
            case getHelp 0 h k t2 of
                Just _ ->
                    insertHelp 0 h k v t

                Nothing ->
                    t
    in
        foldWithHash helper empty t1


{-| Keep a key-value pair when its key does not appear in the second Dictionary.
-}
diff : Dict k v -> Dict k v -> Dict k v
diff t1 t2 =
    foldWithHash (\h k _ t -> removeHelp 0 h k t) t1 t2
