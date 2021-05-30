module Logic exposing (makeNewGame, checkWinner)

import Array as A exposing (Array)
import Json.Encode as E exposing (Value)
import List exposing (range)

makeNewGame : Int -> String -> String -> Value
makeNewGame strideSize gameId activePlayer =
    E.object
        [ ( "id", E.string gameId )
        , ( "player1", E.string gameId )
        , ( "activePlayer", E.string activePlayer )
        , ( "cells", E.string ( String.repeat ( strideSize * strideSize ) "-" ) )
        , ( "winner", E.null )
        , ( "size", E.int strideSize )
        ]

cellIndex : Int -> Int -> Int -> Int
cellIndex strideSize x y = y * strideSize + x

matcher : String -> (Maybe String, Bool) -> (Maybe String, Bool)
matcher s ( curr, match ) =
    ( Just s,
        if not match then match
        else case curr of
            Nothing -> True
            Just "-" -> False
            Just value -> value == s
    )

checkColumnAt : Int -> Array String -> Int -> Bool
checkColumnAt strideSize cells  x =
    (range 0 2)
        |> List.map (\y -> A.get ( cellIndex strideSize x y ) cells)
        |> List.filterMap identity
        |> List.foldl matcher ( Nothing, True )
        |> (\( _, match ) -> match)

checkRowAt : Int -> Array String -> Int -> Bool
checkRowAt strideSize cells  y =
    (range 0 2)
        |> List.map (\x -> A.get ( cellIndex strideSize x y) cells)
        |> List.filterMap identity
        |> List.foldl matcher ( Nothing, True )
        |> (\( _, match ) -> match)

checkDiagonals : Int -> Array String -> Bool
checkDiagonals strideSize cells  =
    let
        stridedCellIndex = cellIndex strideSize
        nwse =
            (List.map2 (\x y -> (x, y))
                (range 0 2)
                (range 0 2)
            )
            |> List.map (\(x, y) -> A.get (stridedCellIndex x y) cells)
            |> List.filterMap identity
            |> List.foldl matcher ( Nothing, True )
            |> (\( _, match ) -> match)
        swne =
            (List.map2 (\x y -> (x, y))
                (range 0 2)
                ((range 0 2) |> List.reverse)
            )
            |> List.map (\(x, y) -> A.get (stridedCellIndex x y) cells)
            |> List.filterMap identity
            |> List.foldl matcher ( Nothing, True )
            |> (\( _, match ) -> match)
    in
        nwse || swne

checkWinner : Int -> List String -> ( Int, Int ) -> Maybe String
checkWinner strideSize cells  (x, y) =
    let
        stridedCellIndex = cellIndex strideSize
        stridedCheckColumnAt = checkColumnAt strideSize
        stridedCheckRowAt = checkRowAt strideSize
        stridedCheckDiagonals = checkDiagonals strideSize

        arr = A.fromList cells
        winner = (A.get ( stridedCellIndex x y ) arr)
            |> Maybe.andThen (\s ->
                if s == "-" then
                   Nothing
                else if (stridedCheckColumnAt arr x) || (stridedCheckRowAt arr y) || (stridedCheckDiagonals arr) then
                    Just s
                else
                    Nothing
            )
    in
        winner
