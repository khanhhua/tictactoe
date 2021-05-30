module Logic exposing (makeNewGame, checkWinner, checkColumnAt, checkRowAt, checkDiagonals)

import Array as A exposing (Array)
import Json.Encode as E exposing (Value)
import List exposing (range)
import Basics exposing (clamp)

win_limit = 4
upper_bound = win_limit - 1

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

matcher : String -> Int -> String -> (String, Int ) -> (String, Int )
matcher key threshold nextToken ( current, counter ) =
    --Debug.log "matcher"
    ( nextToken
    , if counter >= threshold then counter
    else if nextToken == current && nextToken == key then counter + 1
    else if nextToken == key then 1
    else 0 -- reset count upon non-matching non-contiguous
    )

checkColumnAt : Int -> Array String -> String -> Int -> Int -> Bool
checkColumnAt strideSize cells key x y0 =
    let
        low = clamp 0 upper_bound ( y0 - upper_bound )
        high = clamp ( strideSize - upper_bound) ( strideSize - 1 ) ( y0 + upper_bound )
        matchFn = matcher key win_limit
    in
    (range low high)
        |> List.map (\y -> A.get ( cellIndex strideSize x y ) cells)
        |> List.filterMap identity
        |> List.foldl matchFn ( key, 0 )
        |> (\( _, counter ) -> counter == win_limit)

checkRowAt : Int -> Array String -> String -> Int -> Int -> Bool
checkRowAt strideSize cells key x0 y =
    let
        low = clamp 0 upper_bound ( x0 - upper_bound)
        high = clamp ( strideSize - upper_bound ) ( strideSize - 1 ) ( x0 + upper_bound)
        matchFn = matcher key win_limit
    in
    (range low high)
        |> List.map (\x -> A.get ( cellIndex strideSize x y) cells)
        |> List.filterMap identity
        |> List.foldl matchFn ( key, 0 )
        |> (\( _, counter ) -> counter == win_limit)

checkDiagonals : Int -> Array String -> String -> Int -> Int -> Bool
checkDiagonals strideSize cells key x0 y0 =
    let
        lowX = clamp 0 upper_bound ( x0 - upper_bound )
        highX = clamp upper_bound ( strideSize - 1 ) ( x0 + upper_bound )

        rangeX = range lowX highX
        --      y = ax + b
        -- ->   b = y0 - ax0
        rangeYnwse = rangeX |> List.map (\x -> x + y0 - x0)

        intersect = y0 + x0
        rangeYswne = rangeX |> List.map (\x -> -x + intersect)

        matchFn = matcher key win_limit
    in
    let
        stridedCellIndex = cellIndex strideSize
        nwse =
            ( List.map2 (\x y -> (x, y))
                rangeX
                rangeYnwse
            )
            |> List.map (\(x, y) -> A.get (stridedCellIndex x y) cells)
            |> List.filterMap identity
            |> List.foldl matchFn ( key, 0 )
            |> (\( _, counter ) -> counter == win_limit )
        swne =
            ( List.map2 (\x y -> (x, y))
                rangeX
                rangeYswne
            )
            |> List.map (\(x, y) -> A.get (stridedCellIndex x y) cells)
            |> List.filterMap identity
            |> List.foldl matchFn ( key, 0 )
            |> (\( _, counter ) -> counter == win_limit )
    in
        nwse || swne

checkWinner : Int -> List String -> ( Int, Int ) -> Maybe String
checkWinner strideSize cells (x, y) =
    let
        maybeKey = A.get ( stridedCellIndex x y ) arr

        stridedCellIndex = cellIndex strideSize
        stridedCheckColumnAt = checkColumnAt strideSize
        stridedCheckRowAt = checkRowAt strideSize
        stridedCheckDiagonals = checkDiagonals strideSize

        arr = A.fromList cells

        winner = case maybeKey of
            Nothing -> Nothing
            Just "-" -> Nothing
            Just key ->
                if (stridedCheckColumnAt arr key x y)
                    || (stridedCheckRowAt arr key x y)
                    || (stridedCheckDiagonals arr key x y) then
                    Just key
                else
                    Nothing
    in
        winner
