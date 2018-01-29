module Hash.FNV exposing (hash)

import Native.FNV


hash : a -> Int
hash =
    Native.FNV.hash
