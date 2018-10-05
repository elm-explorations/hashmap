module Tests exposing (tests)

import Basics exposing (..)
import Dict as CoreImpl
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int)
import Hash.Dict as Dict exposing (Dict)
import Hash.Set as Set
import List
import List.Extra as List
import Maybe exposing (..)
import Test exposing (..)


animals : Dict.Dict String String
animals =
    Dict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


type alias CollisionObject =
    { a : Int, b : String }


firstCollider : CollisionObject
firstCollider =
    { a = 449311, b = "449311" }


secondCollider : CollisionObject
secondCollider =
    { a = 989797, b = "989797" }


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
                , test "size" <|
                    \() ->
                        Dict.empty
                            |> Dict.insert "k1" "v"
                            |> Dict.insert "k2" "v"
                            |> Dict.insert "k1" "y"
                            |> Dict.remove "k2"
                            |> Dict.size
                            |> Expect.equal 1
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
                            (Dict.toList animals)
                            (Dict.toList
                                (Dict.union
                                    (Dict.singleton "Jerry" "mouse")
                                    (Dict.singleton "Tom" "cat")
                                )
                            )
                , test "union collison" <|
                    \() ->
                        Expect.equal
                            (Dict.toList (Dict.singleton "Tom" "cat"))
                            (Dict.toList
                                (Dict.union
                                    (Dict.singleton "Tom" "cat")
                                    (Dict.singleton "Tom" "mouse")
                                )
                            )
                , test "intersect" <|
                    \() ->
                        Expect.equal
                            (Dict.toList (Dict.singleton "Tom" "cat"))
                            (Dict.toList
                                (Dict.intersect
                                    animals
                                    (Dict.singleton "Tom" "cat")
                                )
                            )
                , test "intersect collision" <|
                    \() ->
                        Expect.equal
                            (Dict.toList (Dict.singleton "Tom" "wolf"))
                            (Dict.toList
                                (Dict.intersect
                                    (Dict.singleton "Tom" "wolf")
                                    animals
                                )
                            )
                , test "diff" <|
                    \() ->
                        Expect.equal
                            (Dict.toList (Dict.singleton "Jerry" "mouse"))
                            (Dict.toList
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
                            (List.range 1 10
                                |> List.indexedMap (,)
                                |> Dict.fromList
                                |> Dict.filter (\_ v -> v % 2 == 0)
                                |> Dict.toList
                                |> List.map Tuple.second
                            )
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
                                        ( Dict.toList a |> List.map Tuple.second
                                        , Dict.toList b |> List.map Tuple.second
                                        )
                                   )
                            )
                ]

        fuzzTests =
            describe "Fuzz tests"
                [ fuzz2 fuzzPairs int "Get works" <|
                    \pairs num ->
                        Dict.get num (Dict.fromList pairs)
                            |> Expect.equal (CoreImpl.get num (CoreImpl.fromList pairs))
                , fuzz fuzzPairs "Converting to/from list works" <|
                    \pairs ->
                        pairs
                            |> Dict.fromList
                            |> Dict.toList
                            |> List.sortBy Tuple.first
                            |> Expect.equal (CoreImpl.toList (CoreImpl.fromList pairs))
                , fuzz fuzzPairs "Insert order is maintained" <|
                    \pairs ->
                        let
                            deduped =
                                List.uniqueBy Tuple.first pairs
                        in
                        pairs
                            |> Dict.fromList
                            |> Dict.toList
                            |> Expect.equal deduped
                , fuzz2 fuzzPairs int "Insert works" <|
                    \pairs num ->
                        Dict.insert num num (Dict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (CoreImpl.insert num num (CoreImpl.fromList pairs))
                                ]
                , fuzz2 fuzzPairs int "Removal works" <|
                    \pairs num ->
                        Dict.remove num (Dict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (CoreImpl.remove num (CoreImpl.fromList pairs))
                                ]
                , fuzz fuzzPairs "Map works" <|
                    \pairs ->
                        Dict.map (\k v -> k + v) (Dict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (CoreImpl.map (\k v -> k + v) (CoreImpl.fromList pairs))
                                ]
                , fuzz fuzzPairs "Filter works" <|
                    \pairs ->
                        Dict.filter (\k _ -> k % 2 == 0) (Dict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (CoreImpl.filter (\k _ -> k % 2 == 0) (CoreImpl.fromList pairs))
                                ]
                , fuzz2 fuzzPairs fuzzPairs "Union works" <|
                    \pairs pairs2 ->
                        Dict.union (Dict.fromList pairs) (Dict.fromList pairs2)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (CoreImpl.union (CoreImpl.fromList pairs) (CoreImpl.fromList pairs2))
                                ]
                , fuzz2 fuzzPairs fuzzPairs "Intersect works" <|
                    \pairs pairs2 ->
                        Dict.intersect (Dict.fromList pairs) (Dict.fromList pairs2)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (CoreImpl.intersect (CoreImpl.fromList pairs) (CoreImpl.fromList pairs2))
                                ]
                , fuzz2 fuzzPairs fuzzPairs "Diff works" <|
                    \pairs pairs2 ->
                        Dict.diff (Dict.fromList pairs) (Dict.fromList pairs2)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (CoreImpl.diff (CoreImpl.fromList pairs) (CoreImpl.fromList pairs2))
                                ]
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



-- HELPERS


fuzzPairs : Fuzzer (List ( Int, Int ))
fuzzPairs =
    ( int, int )
        |> Fuzz.tuple
        |> Fuzz.list


expectEqualDict : CoreImpl.Dict comparable a -> Dict.Dict comparable a -> Expectation
expectEqualDict core hash =
    let
        listify key value acc =
            ( key, value ) :: acc

        coreList =
            CoreImpl.foldr listify [] core

        hashList =
            Dict.toList hash
                |> List.sortBy Tuple.first
    in
    Expect.equal coreList hashList


expectSynchronized : Dict.Dict comparable a -> Expectation
expectSynchronized hash =
    Dict.foldl
        (\key value acc ->
            case Dict.get key hash of
                Just toCompare ->
                    if toCompare == value then
                        CoreImpl.insert key value acc

                    else
                        acc

                Nothing ->
                    acc
        )
        CoreImpl.empty
        hash
        |> (\toCompare -> expectEqualDict toCompare hash)
