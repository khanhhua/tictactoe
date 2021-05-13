port module Main exposing (main)

import Browser exposing (Document)
import Json.Encode as E
import Json.Decode as D exposing (Decoder, Error, Value)
import Elements exposing (boardElement, empty, gameListElement, profileElement)
import Html exposing (Html, button, div, h1, li, nav, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (range)
import Models exposing (Game, GameOverview, Profile, TaggedValue, decodeGame, decodeGameOverview, decodeProfile, decodeTaggedValue)
import Array as A exposing (Array)

port fbLogin : () -> Cmd msg
port fbSelectActiveGame : String -> Cmd msg
port fbUpdateCells : Value -> Cmd msg

-- TODO Refactor into a separate engine
port firebaseInput : (Value -> msg) -> Sub msg  -- Taking input from Firebase
port firebaseOutput : Value -> Cmd msg          -- Sending output to Firebase


main : Program () Model Msg
main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
    { profile : Maybe Profile
    , busy : Bool
    , activeGame : Maybe Game
    , gameList : List GameOverview
    }

type Msg
    = NoOp
    | Login
    | LoginComplete (Result Error Profile)
    | SelectActiveGame String
    | SelectActiveGameComplete (Result Error Game)
    | GameListUpdated (Result Error (List GameOverview))
    | Place Int Int
    | GameUpdated (Result Error Game)

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

playerToken : String -> Html msg
playerToken token =
    span [ class("player-token player-token-" ++ token) ] [ text " " ]

init : () -> ( Model , Cmd Msg )
init _ = (
    { profile = Nothing
    , busy = False
    , activeGame = Nothing
    , gameList = []
    }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            ( { model | busy = True }, fbLogin() )
        LoginComplete result ->
            let
                profile = case result of
                    Ok value -> Just value
                    Err _ -> Nothing
            in
            ( { model
            | busy = False, profile = profile }, Cmd.none
            )
        SelectActiveGame gameId ->
            ( { model | busy = True }, fbSelectActiveGame(gameId) )
        SelectActiveGameComplete result ->
            ( model, Cmd.none )
        Place x y ->
            let
                {- maybeToken is functionally dependant on
                    - maybe activeGame
                        - player1
                        - maybe player2
                    - maybe profile
                    Therefore the first step is to unwrap all maybes and nested maybes into a nested maybe tuple,
                    in which "andThen" the logic for token is
                -}
                canPlay = Maybe.map2 (\activePlayer profile -> activePlayer == profile)
                    (model.activeGame |> Maybe.andThen .activePlayer)
                    (model.profile |> Maybe.map .uid)

                maybeToken : Maybe String
                maybeToken = canPlay
                    |> Maybe.andThen (\canPlay_ ->
                        if not canPlay_ then
                            Nothing
                        else
                            ( Maybe.map3
                                (\player1 player2 profile -> (player1, player2, profile))
                                (model.activeGame |> Maybe.map .player1)
                                (model.activeGame |> Maybe.andThen .player2)
                                (model.profile |> Maybe.map .uid)
                            )
                            |> Maybe.andThen (\(player1, player2, profile) ->
                                if profile == player1 then Just "1"
                                else if profile == player2 then Just "2"
                                else Nothing
                            )
                    )
                param = Maybe.map5 (\game token activePlayer player1 player2 ->
                        ( game.id
                        , let key = y * 3 + x in
                            game.cells
                            |> List.indexedMap (\index s ->
                                if (index == key) && (s == "-") then token
                                    else s
                                )
                        , if activePlayer == player1 then player2 else player1
                        )
                    )
                    model.activeGame
                    maybeToken
                    (model.activeGame |> Maybe.andThen .activePlayer)
                    (model.activeGame |> Maybe.map .player1)
                    (model.activeGame |> Maybe.andThen .player2)
            in
            case param of
                Just (gameId, cells, activePlayer) ->
                    let
                        winner = Maybe.map2 (\player1 player2 ->
                            case checkWinner cells (x, y) of
                                Just "1" -> player1
                                Just "2" -> player2
                                _ -> "None"
                            )
                                (model.activeGame |> Maybe.map .player1)
                                (model.activeGame |> Maybe.andThen .player2)
                        value = E.object
                            [ ( "gameId", E.string gameId )
                            , ( "activePlayer", E.string activePlayer )
                            , ( "cells", E.string (cells |> String.join "") )
                            , ( "winner", winner |> Maybe.withDefault "None" |> E.string )
                            ]
                    in
                    ( model, fbUpdateCells value )
                Nothing -> ( model, Cmd.none)
        GameListUpdated result ->
            case result of
                Ok gameList ->
                    ( { model | gameList = gameList }, Cmd.none )
                Err err ->
                    ( model, Cmd.none )
        GameUpdated result ->
            case result of
                Ok game ->
                    -- TODO Show dialog to notify WINNER
                    ( { model | activeGame = Just game }
                    , Cmd.none )
                Err err ->
                    ( model, Cmd.none )
        _ ->
            ( model, Cmd.none )

-- SUBSCRIPTIONS

firebaseMsgRouter : Result Error TaggedValue -> Msg
firebaseMsgRouter result =
    case result of
        Err _ -> NoOp
        Ok taggedValue ->
            case taggedValue.tag of
                "profile" ->
                    taggedValue.value |> D.decodeValue decodeProfile |> LoginComplete
                "gameList" ->
                    taggedValue.value |> D.decodeValue (D.list decodeGameOverview) |> GameListUpdated
                "game" ->
                    taggedValue.value |> D.decodeValue decodeGame |> GameUpdated
                _ -> NoOp

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ firebaseInput (D.decodeValue decodeTaggedValue >> firebaseMsgRouter)
        ]

view : Model -> Html Msg
view model =
    let
        cells = model.activeGame |> Maybe.map .cells
    in
    div []
        [ nav [class "navbar navbar-expand-lg navbar-light bg-light mb-3"]
            [ span [ class("navbar-brand") ] [ text "Tic-Tac-Toe"  ]
            , ul [ class("navbar-nav ml-auto") ]
                [ li [ class("nav-item") ]
                    [ case model.profile of
                        Nothing ->
                            button
                                [ class("btn btn-primary")
                                , onClick Login
                                ]
                                [ text "Login" ]
                        _ -> empty
                    ]
                ]
            ]
        , div [ class("container") ]
            [ div [ class("row") ]
                [ div [ class("col-3") ]
                    [ gameListElement SelectActiveGame model.gameList ]
                , div [ class("col mx-auto") ]
                    [ case cells of
                        Just cells_ ->
                            boardElement Place playerToken cells_
                        Nothing -> empty
                    ]
                ,  div [ class("col-3") ]
                    [ model.profile
                        |> Maybe.andThen (profileElement >> Just)
                        |> Maybe.withDefault empty
                    ]
                ]
            ]
        ]
