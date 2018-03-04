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

{-| A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.


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

import Hash.Hamt as Hamt exposing (Tree)
import Hash.FNV as FNV


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type alias Dict k v =
    Tree k v


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Hamt.empty


{-| Create a dictionary with one key-value pair.
-}
singleton : k -> v -> Dict k v
singleton key val =
    insert key val empty


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    dict == Hamt.empty


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict k v -> Int
size =
    Hamt.size


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
    Hamt.get (FNV.hash key) key dict


{-| Determine if a key is in a dictionary.
-}
member : k -> Dict k v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> Dict k v -> Dict k v
insert key value dict =
    Hamt.set (FNV.hash key) key value dict


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key fn dict =
    case fn <| get key dict of
        Just val ->
            insert key val dict

        Nothing ->
            remove key dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> Dict k v -> Dict k v
remove key dict =
    Hamt.remove (FNV.hash key) key dict



-- LISTS


{-| Convert an association list into a dictionary.
-}
fromList : List ( k, v ) -> Dict k v
fromList list =
    List.foldl (\( key, value ) acc -> insert key value acc) empty list


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : Dict k v -> List ( k, v )
toList dict =
    fold (\k v acc -> ( k, v ) :: acc) [] dict


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

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


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map f dict =
    Hamt.foldl (\key val acc -> insert key (f key val) acc) empty dict


{-| Fold over the key-value pairs in a dictionary.
-}
fold : (k -> v -> b -> b) -> b -> Dict k v -> b
fold f acc dict =
    Hamt.foldl f acc dict


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter predicate dictionary =
    let
        add key value dict =
            if predicate key value then
                insert key value dict
            else
                dict
    in
        fold add empty dictionary


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> Dict k v -> ( Dict k v, Dict k v )
partition predicate dict =
    let
        add key value ( t1, t2 ) =
            if predicate key value then
                ( insert key value t1, t2 )
            else
                ( t1, insert key value t2 )
    in
        fold add ( empty, empty ) dict



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict k v -> Dict k v -> Dict k v
union t1 t2 =
    fold insert t2 t1


{-| Keep a key-value pair when its key appears in the second Dictionary.
Preference is given to values in the first Dictionary.
-}
intersect : Dict k v -> Dict k v -> Dict k v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second Dictionary.
-}
diff : Dict k v -> Dict k v -> Dict k v
diff t1 t2 =
    filter (\k v -> not <| member k t2) t1
