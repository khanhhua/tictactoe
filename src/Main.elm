port module Main exposing (main)

import Browser exposing (Document)
import Json.Decode as D exposing (Decoder, Error, Value)
import Elements exposing (boardElement, empty, gameListElement, profileElement)
import Html exposing (Html, button, div, h1, li, nav, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Models exposing (Game, GameOverview, Profile, TaggedValue, decodeCells, decodeGame, decodeGameOverview, decodeProfile, decodeTaggedValue)

port fbLogin : () -> Cmd msg
port fbSelectActiveGame : String -> Cmd msg
port fbUpdateCells : String -> Cmd msg

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

playerToken : String -> Html msg
playerToken name =
    span [ class("player-token") ] [ text (String.slice 0 1 name) ]

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
                key = y * 3 + x
                cellString = Maybe.map
                    (\game -> game.cells
                        |> List.indexedMap (\index s ->
                            if index == key then "X"
                            else s
                        )
                        |> String.join ""
                    ) model.activeGame
            in
            case cellString of
                Just value -> ( model, fbUpdateCells value )
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
                        Just profile ->
                            profileElement profile
                    ]
                ]
            ]
        , div [ class("container") ]
            [ div [ class("row") ]
                [ div [ class("col mx-auto") ]
                    [ case cells of
                        Just cells_ ->
                            boardElement Place playerToken cells_
                        Nothing -> empty
                    ]
                , div [ class("col-3") ]
                    [ gameListElement SelectActiveGame model.gameList ]
                ]
            ]
        ]
