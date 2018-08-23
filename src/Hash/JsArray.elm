module Hash.JsArray exposing
    ( JsArray
    , empty, singleton, initialize
    , length, unsafeGet, unsafeSet, unsafeInsert, removeIndex, push
    , foldl, foldr, map, slice
    , appendN, indexedMap, initializeFromList
    )

{-| This library provides an immutable version of native javascript arrays.

NOTE: All manipulations causes a copy of the entire array, this can be slow.
For general purpose use, try the `Array` module instead.


# Arrays

@docs JsArray


# Creation

@docs empty, singleton, initialize, listInitialize


# Basics

@docs length, unsafeGet, unsafeSet, unsafeInsert, removeIndex, push


# Transformation

@docs foldl, foldr, map, slice, merge

-}

import Native.JsArray


{-| Representation of a javascript array.
-}
type JsArray a
    = JsArray a


{-| Return an empty array.
-}
empty : JsArray a
empty =
    Native.JsArray.empty


{-| Return an array containing a single value.
-}
singleton : a -> JsArray a
singleton =
    Native.JsArray.singleton


{-| Return the length of the array.
-}
length : JsArray a -> Int
length =
    Native.JsArray.length


{-| Initialize an array. `initalize n offset fn` creates an array of length `n`
with the element at index `i` initialized to the result of `(f (i + offset))`.

The offset parameter is there so one can avoid creating a closure for this use
case. This is an optimization that has proved useful in the `Array` module.

    initialize 3 5 identity == [ 5, 6, 7 ]

-}
initialize : Int -> Int -> (Int -> a) -> JsArray a
initialize =
    Native.JsArray.initialize


{-| Initialize an array from a list. `initializeFromList n ls` creates an array of,
at most, `n` elements from the list. The return value is a tuple containing the
created array as well as a list without the first `n` elements.

This function was created specifically for the `Array` module, which never wants
to create `JsArray`s above a certain size. That being said, because every
manipulation of `JsArray` results in a copy, users should always try to keep
these as small as possible. The `n` parameter should always be set to a
reasonably small value.

-}
initializeFromList : Int -> List a -> ( JsArray a, List a )
initializeFromList =
    Native.JsArray.initializeFromList


{-| Returns the element at the given index.

WARNING: This function does not perform bounds checking.
Make sure you know the index is within bounds when using this function.

-}
unsafeGet : Int -> JsArray a -> a
unsafeGet =
    Native.JsArray.unsafeGet


{-| Sets the element at the given index.

WARNING: This function does not perform bounds checking.
Make sure you know the index is within bounds when using this function.

-}
unsafeSet : Int -> a -> JsArray a -> JsArray a
unsafeSet =
    Native.JsArray.unsafeSet


{-| Inserts element at given index.

WARNING: This function does not perform bounds checking.
Make sure you know the index is within bounds when using this function.

-}
unsafeInsert : Int -> a -> JsArray a -> JsArray a
unsafeInsert =
    Native.JsArray.unsafeInsert


{-| Returns an array without the element stored at index n. If index is out of range, a copy
of the same array is returned.
-}
removeIndex : Int -> JsArray a -> JsArray a
removeIndex =
    Native.JsArray.removeIndex


{-| Push an element onto the array.
-}
push : a -> JsArray a -> JsArray a
push =
    Native.JsArray.push


{-| Reduce the array from the left.
-}
foldl : (a -> b -> b) -> b -> JsArray a -> b
foldl =
    Native.JsArray.foldl


{-| Reduce the array from the right.
-}
foldr : (a -> b -> b) -> b -> JsArray a -> b
foldr =
    Native.JsArray.foldr


{-| Apply a function on every element in an array.
-}
map : (a -> b) -> JsArray a -> JsArray b
map =
    Native.JsArray.map


{-| Apply a function on every element and its index in an array.
An offset allows to modify the index passed to the function.

    indexedMap (,) 5 (repeat 3 3) == Array [ ( 5, 3 ), ( 6, 3 ), ( 7, 3 ) ]

-}
indexedMap : (Int -> a -> b) -> Int -> JsArray a -> JsArray b
indexedMap =
    Native.JsArray.indexedMap


{-| Get a sub section of an array: `(slice start end array)`.
The `start` is a zero-based index where we will start our slice.
The `end` is a zero-based index that indicates the end of the slice.
The slice extracts up to, but no including, the `end`.

Both `start` and `end` can be negative, indicating an offset from the end
of the array. Popping the last element of the array is therefore:
`slice 0 -1 arr`.

In the case of an impossible slice, the empty array is returned.

-}
slice : Int -> Int -> JsArray a -> JsArray a
slice =
    Native.JsArray.slice


{-| Appends `n` elements from array `b` onto array `a`: `(appendN n a b)`.

The `n` parameter is required by the `Array` module, which never wants to
create `JsArray`s above a certain size, even when appending.

-}
appendN : Int -> JsArray a -> JsArray a -> JsArray a
appendN =
    Native.JsArray.appendN
