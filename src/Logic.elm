module Logic exposing (checkWinner)

import Array as A exposing (Array)
import List exposing (range)

cellIndex : Int -> Int -> Int
cellIndex x y = y * 3 + x

matcher : String -> (Maybe String, Bool) -> (Maybe String, Bool)
matcher s ( curr, match ) =
    ( Just s,
        if not match then match
        else case curr of
            Nothing -> True
            Just "-" -> False
            Just value -> value == s
    )

checkColumnAt : Array String -> Int -> Bool
checkColumnAt cells x =
    (range 0 2)
        |> List.map (\y -> A.get ( cellIndex x y ) cells)
        |> List.filterMap identity
        |> List.foldl matcher ( Nothing, True )
        |> (\( _, match ) -> match)

checkRowAt : Array String -> Int -> Bool
checkRowAt cells y =
    (range 0 2)
        |> List.map (\x -> A.get (cellIndex x y) cells)
        |> List.filterMap identity
        |> List.foldl matcher ( Nothing, True )
        |> (\( _, match ) -> match)

checkDiagonals : Array String -> Bool
checkDiagonals cells =
    let
        nwse =
            (List.map2 (\x y -> (x, y))
                (range 0 2)
                (range 0 2)
            )
            |> List.map (\(x, y) -> A.get (cellIndex x y) cells)
            |> List.filterMap identity
            |> List.foldl matcher ( Nothing, True )
            |> (\( _, match ) -> match)
        swne =
            (List.map2 (\x y -> (x, y))
                (range 0 2)
                ((range 0 2) |> List.reverse)
            )
            |> List.map (\(x, y) -> A.get (cellIndex x y) cells)
            |> List.filterMap identity
            |> List.foldl matcher ( Nothing, True )
            |> (\( _, match ) -> match)
    in
        nwse || swne

checkWinner : List String -> ( Int, Int ) -> Maybe String
checkWinner cells (x, y) =
    let
        arr = A.fromList cells
        winner = (A.get ( cellIndex x y ) arr)
            |> Maybe.andThen (\s ->
                if s == "-" then
                   Nothing
                else if (checkColumnAt arr x) || (checkRowAt arr y) || (checkDiagonals arr) then
                    Just s
                else
                    Nothing
            )
    in
        winner
