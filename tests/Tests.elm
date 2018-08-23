module Tests exposing (tests)

import Basics exposing (..)
import Dict as CoreImpl
import Expect
import Fuzz exposing (Fuzzer)
import Hash.Dict as Dict exposing (Dict)
import Hash.Set as Set
import List
import List.Extra as List
import Maybe exposing (..)
import Test exposing (..)


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


comparisonList : Dict.Dict comparable a -> List ( comparable, a )
comparisonList dict =
    Dict.toList dict
        |> List.sortBy Tuple.first


type alias CollisionObject =
    { a : Int, b : String }


firstCollider : CollisionObject
firstCollider =
    { a = 42508, b = "42508" }


secondCollider : CollisionObject
secondCollider =
    { a = 63992, b = "63992" }


collisionObjects : List CollisionObject
collisionObjects =
    [ firstCollider
    , secondCollider
    ]


tests : Test
tests =
    let
        buildTests =
            describe "build Tests"
                [ test "empty" <|
                    \() -> Expect.equal (Dict.fromList []) Dict.empty
                , test "singleton" <|
                    \() -> Expect.equal (Dict.fromList [ ( "k", "v" ) ]) (Dict.singleton "k" "v")
                , test "insert" <|
                    \() -> Expect.equal (Dict.fromList [ ( "k", "v" ) ]) (Dict.insert "k" "v" Dict.empty)
                , test "insert replace" <|
                    \() -> Expect.equal (Dict.fromList [ ( "k", "vv" ) ]) (Dict.insert "k" "vv" (Dict.singleton "k" "v"))
                , test "update" <|
                    \() -> Expect.equal (Dict.fromList [ ( "k", "vv" ) ]) (Dict.update "k" (\v -> Just "vv") (Dict.singleton "k" "v"))
                , test "update Nothing" <|
                    \() ->
                        Dict.singleton "k" "v"
                            |> Dict.update "k" (\v -> Nothing)
                            |> Dict.toList
                            |> Expect.equal []
                , test "remove" <|
                    \() ->
                        Dict.singleton "k" "v"
                            |> Dict.remove "k"
                            |> Dict.toList
                            |> Expect.equal []
                , test "remove not found" <|
                    \() -> Expect.equal (Dict.singleton "k" "v") (Dict.remove "kk" (Dict.singleton "k" "v"))
                , test "fromList excludes duplicates" <|
                    \() -> Expect.equal (Dict.singleton 1 1) (Dict.fromList [ ( 1, 1 ), ( 1, 1 ) ])
                ]

        queryTests =
            describe "query Tests"
                [ test "member 1" <|
                    \() -> Expect.equal True (Dict.member "Tom" animals)
                , test "member 2" <|
                    \() -> Expect.equal False (Dict.member "Spike" animals)
                , test "get 1" <|
                    \() -> Expect.equal (Just "cat") (Dict.get "Tom" animals)
                , test "get 2" <|
                    \() -> Expect.equal Nothing (Dict.get "Spike" animals)
                , test "size of empty dictionary" <|
                    \() -> Expect.equal 0 (Dict.size Dict.empty)
                , test "size of example dictionary" <|
                    \() -> Expect.equal 2 (Dict.size animals)
                ]

        combineTests =
            describe "combine Tests"
                [ test "union" <|
                    \() ->
                        Expect.equal
                            (comparisonList animals)
                            (comparisonList
                                (Dict.union
                                    (Dict.singleton "Jerry" "mouse")
                                    (Dict.singleton "Tom" "cat")
                                )
                            )
                , test "union collison" <|
                    \() ->
                        Expect.equal
                            (comparisonList (Dict.singleton "Tom" "cat"))
                            (comparisonList
                                (Dict.union
                                    (Dict.singleton "Tom" "cat")
                                    (Dict.singleton "Tom" "mouse")
                                )
                            )
                , test "intersect" <|
                    \() ->
                        Expect.equal
                            (comparisonList (Dict.singleton "Tom" "cat"))
                            (comparisonList
                                (Dict.intersect
                                    animals
                                    (Dict.singleton "Tom" "cat")
                                )
                            )
                , test "intersect collision" <|
                    \() ->
                        Expect.equal
                            (comparisonList (Dict.singleton "Tom" "wolf"))
                            (comparisonList
                                (Dict.intersect
                                    (Dict.singleton "Tom" "wolf")
                                    animals
                                )
                            )
                , test "diff" <|
                    \() ->
                        Expect.equal
                            (comparisonList (Dict.singleton "Jerry" "mouse"))
                            (comparisonList
                                (Dict.diff
                                    animals
                                    (Dict.singleton "Tom" "cat")
                                )
                            )
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" <|
                    \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
                , test "filter (numbers)" <|
                    \() ->
                        Expect.equal [ 2, 4, 6, 8, 10 ]
                            (List.range 1 10 |> List.indexedMap (,) |> Dict.fromList |> Dict.filter (\_ v -> v % 2 == 0) |> comparisonList |> List.map Tuple.second)
                , test "partition" <|
                    \() ->
                        Expect.equal
                            ( Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse" )
                            (Dict.partition (\k v -> k == "Tom") animals)
                , test "partition (numbers)" <|
                    \() ->
                        Expect.equal ( [ 2, 4, 6, 8, 10 ], [ 1, 3, 5, 7, 9 ] )
                            (List.range 1 10
                                |> List.indexedMap (,)
                                |> Dict.fromList
                                |> Dict.partition (\_ v -> v % 2 == 0)
                                |> (\( a, b ) ->
                                        ( comparisonList a |> List.map Tuple.second
                                        , comparisonList b |> List.map Tuple.second
                                        )
                                   )
                            )
                ]

        fuzzTests =
            describe "Fuzz tests"
                [ fuzz2 fuzzPairs pairRange "Get works" <|
                    \pairs num ->
                        Dict.get num (Dict.fromList pairs)
                            |> Expect.equal (CoreImpl.get num (CoreImpl.fromList pairs))
                , fuzz fuzzPairs "Converting to/from list works" <|
                    \pairs ->
                        comparisonList (Dict.fromList pairs)
                            |> Expect.equal (CoreImpl.toList (CoreImpl.fromList pairs))
                , fuzz fuzzPairs "Insert order is maintained" <|
                    \pairs ->
                        let
                            deduped =
                                List.uniqueBy Tuple.first pairs
                        in
                        deduped
                            |> Dict.fromList
                            |> Dict.toList
                            |> Expect.equal deduped
                , fuzz2 fuzzPairs pairRange "Insert works" <|
                    \pairs num ->
                        comparisonList (Dict.insert num num (Dict.fromList pairs))
                            |> Expect.equal (CoreImpl.toList (CoreImpl.insert num num (CoreImpl.fromList pairs)))
                , fuzz2 fuzzPairs pairRange "Removal works" <|
                    \pairs num ->
                        comparisonList (Dict.remove num (Dict.fromList pairs))
                            |> Expect.equal (CoreImpl.toList (CoreImpl.remove num (CoreImpl.fromList pairs)))
                , fuzz fuzzPairs "Map works" <|
                    \pairs ->
                        comparisonList (Dict.map (\k v -> k + v) (Dict.fromList pairs))
                            |> Expect.equal (CoreImpl.toList (CoreImpl.map (\k v -> k + v) (CoreImpl.fromList pairs)))
                , fuzz2 fuzzPairs fuzzPairs "Union works" <|
                    \pairs pairs2 ->
                        comparisonList (Dict.union (Dict.fromList pairs) (Dict.fromList pairs2))
                            |> Expect.equal (CoreImpl.toList (CoreImpl.union (CoreImpl.fromList pairs) (CoreImpl.fromList pairs2)))
                , fuzz2 fuzzPairs fuzzPairs "Intersect works" <|
                    \pairs pairs2 ->
                        comparisonList (Dict.intersect (Dict.fromList pairs) (Dict.fromList pairs2))
                            |> Expect.equal (CoreImpl.toList (CoreImpl.intersect (CoreImpl.fromList pairs) (CoreImpl.fromList pairs2)))
                , fuzz2 fuzzPairs fuzzPairs "Diff works" <|
                    \pairs pairs2 ->
                        comparisonList (Dict.diff (Dict.fromList pairs) (Dict.fromList pairs2))
                            |> Expect.equal (CoreImpl.toList (CoreImpl.diff (CoreImpl.fromList pairs) (CoreImpl.fromList pairs2)))
                ]

        collisionTests =
            describe "Collision tests"
                [ test "Insert" <|
                    \() ->
                        Set.toList (Set.fromList collisionObjects)
                            |> Expect.equal collisionObjects
                , test "Remove" <|
                    \() ->
                        Set.toList (Set.remove firstCollider (Set.fromList collisionObjects))
                            |> Expect.equal [ secondCollider ]
                , test "Get" <|
                    \() ->
                        Set.member secondCollider (Set.fromList collisionObjects)
                            |> Expect.equal True
                ]
    in
    describe "Dict Tests"
        [ buildTests
        , queryTests
        , combineTests
        , transformTests
        , fuzzTests
        , collisionTests
        ]
