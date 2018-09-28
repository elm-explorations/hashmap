module Hash.Set exposing
    ( Set
    , empty, singleton, insert, remove
    , isEmpty, member, size
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
    )

{-| A set of unique values.


# Sets

@docs Set


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import Hash.Dict as Dict exposing (Dict)


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type Set a
    = Set (Dict a Bool)


{-| Create an empty set.
-}
empty : Set a
empty =
    Set Dict.empty


{-| Create a set with one value.
-}
singleton : a -> Set a
singleton k =
    Set (Dict.singleton k True)


{-| Insert a value into a set.
-}
insert : a -> Set a -> Set a
insert k (Set dict) =
    Set (Dict.insert k True dict)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> Set a -> Set a
remove k (Set dict) =
    Set (Dict.remove k dict)


{-| Determine if a set is empty.
-}
isEmpty : Set a -> Bool
isEmpty (Set dict) =
    Dict.isEmpty dict


{-| Determine if a value is in a set.
-}
member : a -> Set a -> Bool
member k (Set dict) =
    Dict.member k dict


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size (Set dict) =
    Dict.size dict


{-| Get the union of two sets. Keep all values.
-}
union : Set a -> Set a -> Set a
union (Set d1) (Set d2) =
    Set (Dict.union d1 d2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set a -> Set a -> Set a
intersect (Set d1) (Set d2) =
    Set (Dict.intersect d1 d2)


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set a -> Set a -> Set a
diff (Set d1) (Set d2) =
    Set (Dict.diff d1 d2)


{-| Convert a set into a list.
-}
toList : Set a -> List a
toList (Set dict) =
    Dict.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List a -> Set a
fromList xs =
    List.foldl insert empty xs


{-| Fold over the values in a set.
-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl fn init (Set dict) =
    Dict.foldl (\k _ acc -> fn k acc) init dict


{-| Fold over the values in a set.
-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr fn init (Set dict) =
    Dict.foldr (\k _ acc -> fn k acc) init dict


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> b) -> Set a -> Set b
map fn (Set dict) =
    Set (Dict.foldl (\k _ b -> Dict.insert (fn k) True b) Dict.empty dict)


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> Set a -> Set a
filter p (Set dict) =
    Set (Dict.filter (\k _ -> p k) dict)


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> Set a -> ( Set a, Set a )
partition p (Set dict) =
    let
        ( trues, falses ) =
            Dict.partition (\k _ -> p k) dict
    in
    ( Set trues, Set falses )
