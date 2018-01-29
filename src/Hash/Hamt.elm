module Hash.Hamt
    exposing
        ( Tree
        , empty
        , hashPositionWithShift
        , get
        , set
        , remove
        , foldl
        , size
        )

import Hash.JsArray as JsArray exposing (JsArray)
import Bitwise
import List.Extra exposing (find)


type alias Tree comparable v =
    { positionMap : Int
    , blobs : Blobs comparable v
    }


type alias Blobs comparable v =
    JsArray (Node comparable v)


type Node comparable v
    = Element Int comparable v
    | SubTree (Tree comparable v)
    | Collision Int (List ( comparable, v ))


empty : Tree comparable v
empty =
    { positionMap = 0
    , blobs = JsArray.empty
    }


setByIndex : Int -> Int -> Node comparable v -> Tree comparable v -> Tree comparable v
setByIndex idx blobPos val ls =
    let
        mask =
            Bitwise.shiftLeftBy idx 0x01

        alteredBitmap =
            Bitwise.or ls.positionMap mask

        shouldReplace =
            Bitwise.and ls.positionMap mask == mask

        newBlobs =
            if shouldReplace then
                JsArray.unsafeSet blobPos val ls.blobs
            else
                JsArray.unsafeInsert blobPos val ls.blobs
    in
        { positionMap = alteredBitmap
        , blobs = newBlobs
        }


valueByIndex : Int -> Int -> Tree comparable v -> Maybe (Node comparable v)
valueByIndex idx blobPos ls =
    let
        mask =
            Bitwise.shiftLeftBy idx 0x01

        hasValue =
            Bitwise.and ls.positionMap mask == mask
    in
        if hasValue then
            Just <| JsArray.unsafeGet blobPos ls.blobs
        else
            Nothing


removeByIndex : Int -> Int -> Tree comparable v -> Tree comparable v
removeByIndex idx blobPos ls =
    let
        mask =
            Bitwise.shiftLeftBy idx 0x01

        alteredBitmap =
            Bitwise.xor ls.positionMap mask
    in
        { positionMap = alteredBitmap
        , blobs = removeAt blobPos ls.blobs
        }


removeAt : Int -> Blobs comparable v -> Blobs comparable v
removeAt idx arr =
    let
        start =
            JsArray.slice 0 idx arr

        end =
            (JsArray.slice (idx + 1) (JsArray.length arr) arr)
    in
        JsArray.appendN 32 start end


hashPositionWithShift : Int -> Int -> Int
hashPositionWithShift shift hash =
    Bitwise.and 0x1F <| Bitwise.shiftRightZfBy shift hash


blobPosition : Int -> Int -> Int
blobPosition idx posMap =
    countBits (Bitwise.shiftLeftBy 1 (Bitwise.shiftLeftBy (31 - idx) posMap))


{-| No idea how this works. Stole code from stack overflow.
-}
countBits : Int -> Int
countBits bitmap =
    let
        b1 =
            bitmap - (Bitwise.and (Bitwise.shiftRightZfBy 1 bitmap) 0x55555555)

        b2 =
            (Bitwise.and b1 0x33333333) + (Bitwise.and (Bitwise.shiftRightZfBy 2 b1) 0x33333333)
    in
        Bitwise.shiftRightZfBy 24 ((Bitwise.and (b2 + (Bitwise.shiftRightZfBy 4 b2)) 0x0F0F0F0F) * 0x01010101)


get : Int -> comparable -> Tree comparable v -> Maybe v
get hash key ls =
    getHelp 0 hash key ls


getHelp : Int -> Int -> comparable -> Tree comparable v -> Maybe v
getHelp shift hash key ls =
    let
        pos =
            hashPositionWithShift shift hash

        blobPos =
            blobPosition pos ls.positionMap

        mask =
            Bitwise.shiftLeftBy pos 0x01

        hasValue =
            Bitwise.and ls.positionMap mask == mask
    in
        if hasValue then
            case JsArray.unsafeGet blobPos ls.blobs of
                Element _ eKey value ->
                    if key == eKey then
                        Just value
                    else
                        Nothing

                SubTree nodes ->
                    getHelp (shift + 5) hash key nodes

                Collision _ vals ->
                    Maybe.map Tuple.second
                        (find (\( k, _ ) -> k == key) vals)
        else
            Nothing


set : Int -> comparable -> v -> Tree comparable v -> Tree comparable v
set hash key val ls =
    setHelp 0 hash key val ls


setHelp : Int -> Int -> comparable -> v -> Tree comparable v -> Tree comparable v
setHelp shift hash key val ls =
    let
        pos =
            hashPositionWithShift shift hash

        blobPos =
            blobPosition pos ls.positionMap

        newShift =
            shift + 5
    in
        case valueByIndex pos blobPos ls of
            Nothing ->
                setByIndex pos blobPos (Element hash key val) ls

            Just currValue ->
                case currValue of
                    Element xHash xKey xVal ->
                        if xHash == hash then
                            if xKey == key then
                                setByIndex pos blobPos (Element hash key val) ls
                            else
                                let
                                    element =
                                        if key < xKey then
                                            Collision hash [ ( key, val ), ( xKey, xVal ) ]
                                        else
                                            Collision hash [ ( xKey, xVal ), ( key, val ) ]
                                in
                                    setByIndex pos blobPos element ls
                        else
                            let
                                subNodes =
                                    setHelp newShift xHash xKey xVal empty
                                        |> setHelp newShift hash key val
                                        |> SubTree
                            in
                                setByIndex pos blobPos subNodes ls

                    Collision xHash nodes ->
                        if xHash == hash then
                            let
                                newNodes =
                                    nodes
                                        |> List.filter (\( k, _ ) -> k /= key)
                                        |> ((::) ( key, val ))
                                        |> List.sortBy Tuple.first
                            in
                                setByIndex pos blobPos (Collision hash newNodes) ls
                        else
                            let
                                collisionPos =
                                    hashPositionWithShift newShift xHash

                                collisionBlobPos =
                                    blobPosition collisionPos empty.positionMap

                                subNodes =
                                    setByIndex collisionPos collisionBlobPos currValue empty
                                        |> setHelp newShift hash key val
                                        |> SubTree
                            in
                                setByIndex pos blobPos subNodes ls

                    SubTree nodes ->
                        let
                            sub =
                                setHelp newShift hash key val nodes
                        in
                            setByIndex pos blobPos (SubTree sub) ls


remove : Int -> comparable -> Tree comparable v -> Tree comparable v
remove hash key nl =
    removeHelp 0 hash key nl


removeHelp : Int -> Int -> comparable -> Tree comparable v -> Tree comparable v
removeHelp shift hash key nl =
    let
        pos =
            hashPositionWithShift shift hash

        blobPos =
            blobPosition pos nl.positionMap
    in
        case valueByIndex pos blobPos nl of
            Nothing ->
                nl

            Just node ->
                case node of
                    Element _ eKey value ->
                        if eKey == key then
                            removeByIndex pos blobPos nl
                        else
                            nl

                    SubTree nodes ->
                        let
                            newSub =
                                removeHelp (shift + 5) hash key nodes
                        in
                            if JsArray.length newSub.blobs == 1 then
                                let
                                    v =
                                        JsArray.unsafeGet 0 newSub.blobs
                                in
                                    setByIndex pos blobPos v nl
                            else
                                setByIndex pos blobPos (SubTree newSub) nl

                    Collision _ vals ->
                        let
                            newCollision =
                                List.filter (\( k, _ ) -> k /= key) vals
                        in
                            if List.length newCollision == 1 then
                                case List.head newCollision of
                                    Just ( k, v ) ->
                                        setByIndex pos blobPos (Element hash k v) nl

                                    Nothing ->
                                        Debug.crash "This should not happen."
                            else
                                setByIndex pos blobPos (Collision hash newCollision) nl


foldl : (comparable -> v -> a -> a) -> a -> Tree comparable v -> a
foldl folder acc nl =
    JsArray.foldl
        (\node acc ->
            case node of
                Element _ key val ->
                    folder key val acc

                SubTree nodes ->
                    foldl folder acc nodes

                Collision _ vals ->
                    let
                        colFold ( k, v ) acc =
                            folder k v acc
                    in
                        List.foldl colFold acc vals
        )
        acc
        nl.blobs


size : Tree comparable v -> Int
size nl =
    foldl (\_ _ acc -> acc + 1) 0 nl
