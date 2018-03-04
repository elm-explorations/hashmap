module Hash.Set
    exposing
        ( Set
        , empty
        , singleton
        , insert
        , remove
        , isEmpty
        , member
        , size
        , fold
        , map
        , filter
        , partition
        , union
        , intersect
        , diff
        , toList
        , fromList
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

@docs map, fold, filter, partition

-}

import Hash.Dict as Dict exposing (Dict)


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type alias Set a =
    Dict a ()


{-| Create an empty set.
-}
empty : Set a
empty =
    Dict.empty


{-| Create a set with one value.
-}
singleton : a -> Set a
singleton k =
    Dict.singleton k ()


{-| Insert a value into a set.
-}
insert : a -> Set a -> Set a
insert k d =
    Dict.insert k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> Set a -> Set a
remove k d =
    Dict.remove k d


{-| Determine if a set is empty.
-}
isEmpty : Set a -> Bool
isEmpty d =
    Dict.isEmpty d


{-| Determine if a value is in a set.
-}
member : a -> Set a -> Bool
member k d =
    Dict.member k d


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size d =
    Dict.size d


{-| Get the union of two sets. Keep all values.
-}
union : Set a -> Set a -> Set a
union d1 d2 =
    Dict.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set a -> Set a -> Set a
intersect d1 d2 =
    Dict.intersect d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set a -> Set a -> Set a
diff d1 d2 =
    Dict.diff d1 d2


{-| Convert a set into a list.
-}
toList : Set a -> List a
toList d =
    Dict.keys d


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List a -> Set a
fromList xs =
    List.foldl insert empty xs


{-| Fold over the values in a set.
-}
fold : (a -> b -> b) -> b -> Set a -> b
fold f b d =
    Dict.fold (\k _ b -> f k b) b d


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> a') -> Set a -> Set a'
map f s =
    fromList (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> Set a -> Set a
filter p d =
    Dict.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> Set a -> ( Set a, Set a )
partition p d =
    Dict.partition (\k _ -> p k) d
