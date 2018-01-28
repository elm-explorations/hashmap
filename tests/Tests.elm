module Tests exposing (tests)

import Basics exposing (..)
import Dict as BaseDict
import Hash.Dict as Dict exposing (Dict)
import List
import Maybe exposing (..)
import Test exposing (..)
import Fuzz exposing (Fuzzer)
import Expect


animals : Dict.Dict String String
animals =
    Dict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


pairRange : Fuzzer Int
pairRange =
    Fuzz.intRange 0 1000


fuzzPairs : Fuzzer (List ( Int, Int ))
fuzzPairs =
    ( pairRange, pairRange )
        |> Fuzz.tuple
        |> Fuzz.list


fuzzDict : Fuzzer (Dict Int Int)
fuzzDict =
    Fuzz.map Dict.fromList fuzzPairs


tests : Test
tests =
    let
        buildTests =
            describe "build Tests"
                [ test "empty" <| \() -> Expect.equal (Dict.fromList []) (Dict.empty)
                , test "singleton" <| \() -> Expect.equal (Dict.fromList [ ( "k", "v" ) ]) (Dict.singleton "k" "v")
                , test "insert" <| \() -> Expect.equal (Dict.fromList [ ( "k", "v" ) ]) (Dict.insert "k" "v" Dict.empty)
                , test "insert replace" <| \() -> Expect.equal (Dict.fromList [ ( "k", "vv" ) ]) (Dict.insert "k" "vv" (Dict.singleton "k" "v"))
                , test "update" <| \() -> Expect.equal (Dict.fromList [ ( "k", "vv" ) ]) (Dict.update "k" (\v -> Just "vv") (Dict.singleton "k" "v"))
                , test "update Nothing" <| \() -> Expect.equal Dict.empty (Dict.update "k" (\v -> Nothing) (Dict.singleton "k" "v"))
                , test "remove" <| \() -> Expect.equal Dict.empty (Dict.remove "k" (Dict.singleton "k" "v"))
                , test "remove not found" <| \() -> Expect.equal (Dict.singleton "k" "v") (Dict.remove "kk" (Dict.singleton "k" "v"))
                , test "fromList excludes duplicates" <| \() -> Expect.equal (Dict.singleton 1 1) (Dict.fromList [ ( 1, 1 ), ( 1, 1 ) ])
                ]

        queryTests =
            describe "query Tests"
                [ test "member 1" <| \() -> Expect.equal True (Dict.member "Tom" animals)
                , test "member 2" <| \() -> Expect.equal False (Dict.member "Spike" animals)
                , test "get 1" <| \() -> Expect.equal (Just "cat") (Dict.get "Tom" animals)
                , test "get 2" <| \() -> Expect.equal Nothing (Dict.get "Spike" animals)
                , test "size of empty dictionary" <| \() -> Expect.equal 0 (Dict.size Dict.empty)
                , test "size of example dictionary" <| \() -> Expect.equal 2 (Dict.size animals)
                ]

        combineTests =
            describe "combine Tests"
                [ test "union" <| \() -> Expect.equal animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
                , test "union collison" <| \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
                , test "intersect" <| \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
                , test "intersect collision" <| \() -> Expect.equal (Dict.singleton "Tom" "wolf") (Dict.intersect (Dict.singleton "Tom" "wolf") animals)
                , test "diff" <| \() -> Expect.equal (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" <| \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
                , test "filter (numbers)" <| \() -> Expect.equal [ 2, 4, 6, 8, 10 ] (List.range 1 10 |> List.indexedMap (,) |> Dict.fromList |> Dict.filter (\_ v -> v % 2 == 0) |> Dict.values)
                , test "partition" <| \() -> Expect.equal ( Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse" ) (Dict.partition (\k v -> k == "Tom") animals)
                , test "partition (numbers)" <| \() -> Expect.equal ( [ 2, 4, 6, 8, 10 ], [ 1, 3, 5, 7, 9 ] ) (List.range 1 10 |> List.indexedMap (,) |> Dict.fromList |> Dict.partition (\_ v -> v % 2 == 0) |> (\( a, b ) -> ( Dict.values a, Dict.values b )))
                ]

        mergeTests =
            let
                insertBoth key leftVal rightVal dict =
                    Dict.insert key (leftVal ++ rightVal) dict

                s1 =
                    Dict.empty |> Dict.insert "u1" [ 1 ]

                s2 =
                    Dict.empty |> Dict.insert "u2" [ 2 ]

                s23 =
                    Dict.empty |> Dict.insert "u2" [ 3 ]

                b1 =
                    List.map (\i -> ( i, [ i ] )) (List.range 1 10) |> Dict.fromList

                b2 =
                    List.map (\i -> ( i, [ i ] )) (List.range 5 15) |> Dict.fromList

                bExpected =
                    [ ( 1, [ 1 ] ), ( 2, [ 2 ] ), ( 3, [ 3 ] ), ( 4, [ 4 ] ), ( 5, [ 5, 5 ] ), ( 6, [ 6, 6 ] ), ( 7, [ 7, 7 ] ), ( 8, [ 8, 8 ] ), ( 9, [ 9, 9 ] ), ( 10, [ 10, 10 ] ), ( 11, [ 11 ] ), ( 12, [ 12 ] ), ( 13, [ 13 ] ), ( 14, [ 14 ] ), ( 15, [ 15 ] ) ]
            in
                describe "merge Tests"
                    [ test "merge empties" <|
                        \() ->
                            Expect.equal (Dict.empty)
                                (Dict.merge Dict.insert insertBoth Dict.insert Dict.empty Dict.empty Dict.empty)
                    , test "merge singletons in order" <|
                        \() ->
                            Expect.equal [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                                ((Dict.merge Dict.insert insertBoth Dict.insert s1 s2 Dict.empty) |> Dict.toList)
                    , test "merge singletons out of order" <|
                        \() ->
                            Expect.equal [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                                ((Dict.merge Dict.insert insertBoth Dict.insert s2 s1 Dict.empty) |> Dict.toList)
                    , test "merge with duplicate key" <|
                        \() ->
                            Expect.equal [ ( "u2", [ 2, 3 ] ) ]
                                ((Dict.merge Dict.insert insertBoth Dict.insert s2 s23 Dict.empty) |> Dict.toList)
                    , test "partially overlapping" <|
                        \() ->
                            Expect.equal bExpected
                                ((Dict.merge Dict.insert insertBoth Dict.insert b1 b2 Dict.empty) |> Dict.toList)
                    ]

        fuzzTests =
            describe "Fuzz tests"
                [ fuzz2 fuzzPairs pairRange "Get works" <|
                    \pairs num ->
                        Dict.get num (Dict.fromList pairs)
                            |> Expect.equal (BaseDict.get num (BaseDict.fromList pairs))
                , fuzz fuzzPairs "Converting to/from list works" <|
                    \pairs ->
                        Dict.toList (Dict.fromList pairs)
                            |> Expect.equal (BaseDict.toList (BaseDict.fromList pairs))
                , fuzz2 fuzzPairs pairRange "Insert works" <|
                    \pairs num ->
                        Dict.toList (Dict.insert num num (Dict.fromList pairs))
                            |> Expect.equal (BaseDict.toList (BaseDict.insert num num (BaseDict.fromList pairs)))
                , fuzz2 fuzzPairs pairRange "Removal works" <|
                    \pairs num ->
                        Dict.toList (Dict.remove num (Dict.fromList pairs))
                            |> Expect.equal (BaseDict.toList (BaseDict.remove num (BaseDict.fromList pairs)))
                , fuzz2 fuzzPairs fuzzPairs "Union works" <|
                    \pairs pairs2 ->
                        Dict.toList (Dict.union (Dict.fromList pairs) (Dict.fromList pairs2))
                            |> Expect.equal (BaseDict.toList (BaseDict.union (BaseDict.fromList pairs) (BaseDict.fromList pairs2)))
                , fuzz2 fuzzPairs fuzzPairs "Intersect works" <|
                    \pairs pairs2 ->
                        Dict.toList (Dict.intersect (Dict.fromList pairs) (Dict.fromList pairs2))
                            |> Expect.equal (BaseDict.toList (BaseDict.intersect (BaseDict.fromList pairs) (BaseDict.fromList pairs2)))
                , fuzz2 fuzzPairs fuzzPairs "Diff works" <|
                    \pairs pairs2 ->
                        Dict.toList (Dict.diff (Dict.fromList pairs) (Dict.fromList pairs2))
                            |> Expect.equal (BaseDict.toList (BaseDict.diff (BaseDict.fromList pairs) (BaseDict.fromList pairs2)))
                ]
    in
        describe "Dict Tests"
            [ buildTests
            , queryTests
            , combineTests
            , transformTests
            , mergeTests
            , fuzzTests
            ]
