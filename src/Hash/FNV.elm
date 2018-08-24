module Hash.FNV exposing (hash)

import Native.FNV


fnvOffset : Int
fnvOffset =
    2166136261


hash : a -> Int
hash =
    Native.FNV.hash fnvOffset
