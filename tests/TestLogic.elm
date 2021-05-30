module TestLogic exposing (..)

import Array
import Logic exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)

toCells = Array.fromList << String.split ""

sample_a = toCells <|
       "1111-"
    ++ "-----"
    ++ "-----"
    ++ "-----"
    ++ "-----"

sample_a2 = toCells <|
       "-----"
    ++ "-1111"
    ++ "-----"
    ++ "-----"
    ++ "-----"

sample_b = toCells <|
       "-1111"
    ++ "-----"
    ++ "-----"
    ++ "-----"
    ++ "-----"

sample_c = toCells <|
       "1----"
    ++ "1----"
    ++ "1----"
    ++ "1----"
    ++ "-----"

sample_d = toCells <|
       "-----"
    ++ "1----"
    ++ "1----"
    ++ "1----"
    ++ "1----"

sample_e = toCells <|
       "1----"
    ++ "-1---"
    ++ "--1--"
    ++ "---1-"
    ++ "-----"

sample_f = toCells <|
       "-----"
    ++ "-1---"
    ++ "--1--"
    ++ "---1-"
    ++ "----1"

sample_g = toCells <|
       "---1-"
    ++ "--1--"
    ++ "-1---"
    ++ "1----"
    ++ "-----"

sample_h = toCells <|
       "-----"
    ++ "---1-"
    ++ "--1--"
    ++ "-1---"
    ++ "1----"

sample_i = toCells <|
       "-----"
    ++ "----1"
    ++ "---1-"
    ++ "--1--"
    ++ "-1---"


suite : Test
suite =
    describe "Logic Module"
        [ describe "checkRowAt"
            [ test "Sample A" <|
                (\_ ->
                    [ checkRowAt 5 sample_a "1" 0 0
                    , checkRowAt 5 sample_a "1" 1 0
                    , checkRowAt 5 sample_a "1" 2 0
                    , checkRowAt 5 sample_a "1" 3 0
                    , checkRowAt 5 sample_b "1" 4 0
                    ] |> Expect.equalLists [True, True, True, True, True]
                )
            , test "Sample B: Win a 5x5 game board at (1:1)" <|
                (\_ ->
                    checkRowAt 5 sample_a2 "1" 1 1 |> Expect.equal True
                )
            ]
        , describe "checkColumnAt"
            [ test "Sample C: Win a 5x5 game board at (0:0)" <|
                (\_ ->
                        let
                            result = checkColumnAt 5 sample_c "1" 0 0
                        in
                            True |> Expect.equal result
                    )
                , test "Sample C: Win a 5x5 game board at (0:1)" <|
                    (\_ ->
                        let
                            result = checkColumnAt 5 sample_c "1" 0 1
                        in
                            True |> Expect.equal result
                    )
                , test "Sample C: Win a 5x5 game board at (0:2)" <|
                    (\_ ->
                        let
                            result = checkColumnAt 5 sample_c "1" 0 2
                        in
                            True |> Expect.equal result
                    )
                , test "Sample C: Win a 5x5 game board at (0:3)" <|
                    (\_ ->
                        let
                            result = checkColumnAt 5 sample_c "1" 0 3
                        in
                            True |> Expect.equal result
                    )
                , test "Sample D: Win a 5x5 game board at (0:4)" <|
                    (\_ ->
                        let
                            result = checkColumnAt 5 sample_d "1" 0 4
                        in
                            True |> Expect.equal result
                    )
            ]
        , describe "checkDiagonals"
            [ test "Sample E: Win a 5x5 game board at (0:0)" <|
                 (\_ ->
                     let
                         result = checkDiagonals 5 sample_e "1" 0 0
                     in
                         True |> Expect.equal result
                 )
            , test "Sample E: Win a 5x5 game board at (1:1)" <|
                 (\_ ->
                     let
                         result = checkDiagonals 5 sample_e "1" 1 1
                     in
                         True |> Expect.equal result
                 )
            , test "Sample E: Win a 5x5 game board at (2:2)" <|
                 (\_ ->
                     let
                         result = checkDiagonals 5 sample_e "1" 2 2
                     in
                         True |> Expect.equal result
                 )
            , test "Sample E: Win a 5x5 game board at (3:3)" <|
                 (\_ ->
                     let
                         result = checkDiagonals 5 sample_e "1" 3 3
                     in
                         True |> Expect.equal result
                 )
            , test "Sample F: Win a 5x5 game board at (4:4)" <|
                 (\_ ->
                     let
                         result = checkDiagonals 5 sample_f "1" 4 4
                     in
                         True |> Expect.equal result
                 )
            , test "Sample G: Win a 5x5 game board at (0:3)" <|
                (\_ ->
                    let
                        result = checkDiagonals 5 sample_g "1" 0 3
                    in
                        True |> Expect.equal result
                )
            , test "Sample G: Win a 5x5 game board at (1:2)" <|
                (\_ ->
                    let
                        result = checkDiagonals 5 sample_g "1" 1 2
                    in
                        True |> Expect.equal result
                )
            ----------------------------------------------------
            , test "Sample H: checkDiagonals a 5x5 game board at (0:4)" <|
                (\_ ->
                    let
                        result = checkDiagonals 5 sample_h "1" 0 4
                    in
                        True |> Expect.equal result
                )
            , test "Sample H: checkDiagonals a 5x5 game board at (1:4)" <|
                (\_ ->
                    let
                        result = checkDiagonals 5 sample_i "1" 1 4
                    in
                        True |> Expect.equal result
                )
            ]
        , describe "checkWinner"
            [ test "Sample H: checkWinner a 5x5 game board at (0:4)" <|
                (\_ ->
                    let
                        result = checkWinner 5 ( Array.toList sample_h ) ( 0, 4 )
                    in
                        Just "1" |> Expect.equal result
                )
            ]
        ]
