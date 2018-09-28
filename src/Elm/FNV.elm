module Elm.FNV exposing (hash)

import Elm.Kernel.FNV


fnvOffset : Int
fnvOffset =
    2166136261


hash : a -> Int
hash =
    Elm.Kernel.FNV.hash fnvOffset
