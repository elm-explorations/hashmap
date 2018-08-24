module Main exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict as Dict
import Hash.Dict as Dict2


main : BenchmarkProgram
main =
    program <| suite 100


suite : Int -> Benchmark
suite n =
    let
        half =
            n // 2

        quarter =
            half // 2

        ls =
            List.map3
                (\a b c -> [ a, b, c ])
                (List.map toKeyValuePair (List.range 0 half))
                (List.map toKeyValuePair (List.range quarter (half + quarter) |> List.reverse))
                (List.map toKeyValuePair (List.range (half + 1) n))
                |> List.concat

        toKeyValuePair n =
            ( n, n )

        setLs =
            List.map toKeyValuePair (List.range half (n + half))

        original =
            Dict.fromList ls

        updated =
            Dict2.fromList ls

        originalSetDict =
            Dict.fromList setLs

        updatedSetDict =
            Dict2.fromList setLs

        keys =
            List.map (\( k, v ) -> k) ls
    in
    describe (toString n ++ " elements")
        [ Benchmark.compare "Get"
            "LLRB"
            (\_ -> getter Dict.get keys original)
            "Hash"
            (\_ -> getter Dict2.get keys updated)
        , Benchmark.compare "Insert"
            "LLRB"
            (\_ -> Dict.fromList ls)
            "Hash"
            (\_ -> Dict2.fromList ls)
        , Benchmark.compare "Remove"
            "Core"
            (\_ -> remover Dict.remove keys original)
            "Hash"
            (\_ -> remover Dict2.remove keys updated)
        , Benchmark.compare "Remove one item"
            "Core"
            (\_ -> singleRemover Dict.remove keys original)
            "Hash"
            (\_ -> singleRemover Dict2.remove keys updated)
        , Benchmark.compare "Update insert"
            "Core"
            (\_ -> updater Dict.update (\_ -> Just -1) keys original)
            "Hash"
            (\_ -> updater Dict2.update (\_ -> Just -1) keys updated)
        , Benchmark.compare "Update remove"
            "Core"
            (\_ -> updater Dict.update (\_ -> Nothing) keys original)
            "Hash"
            (\_ -> updater Dict2.update (\_ -> Nothing) keys updated)
        , Benchmark.compare "Map"
            "Core"
            (\_ -> Dict.map (\k v -> v + 1) original)
            "Hash"
            (\_ -> Dict2.map (\k v -> v + 1) updated)
        , Benchmark.compare "Filter"
            "Core"
            (\_ -> Dict.filter (\k v -> v % 2 == 0) original)
            "Hash"
            (\_ -> Dict2.filter (\k v -> v % 2 == 0) updated)
        , Benchmark.compare "Union"
            "Core"
            (\_ -> Dict.union original originalSetDict)
            "Hash"
            (\_ -> Dict2.union updated updatedSetDict)
        , Benchmark.compare "Intersect"
            "Core"
            (\_ -> Dict.intersect original originalSetDict)
            "Hash"
            (\_ -> Dict2.intersect updated updatedSetDict)
        , Benchmark.compare "Diff"
            "Core"
            (\_ -> Dict.diff original originalSetDict)
            "Hash"
            (\_ -> Dict2.diff updated updatedSetDict)
        ]


getter : (a -> b -> c) -> List a -> b -> List c
getter f keys dict =
    List.foldl (\k acc -> f k dict :: acc) [] keys


updater : (a -> b -> c -> c) -> b -> List a -> c -> c
updater f1 f2 keys dict =
    List.foldl (\k acc -> f1 k f2 acc) dict keys


remover : (a -> b -> b) -> List a -> b -> b
remover f keys dict =
    List.foldl (\k acc -> f k acc) dict keys


singleRemover : (a -> b -> c) -> List a -> b -> List c
singleRemover f keys dict =
    List.foldl (\k acc -> f k dict :: acc) [] keys
