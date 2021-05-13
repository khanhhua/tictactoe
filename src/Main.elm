port module Main exposing (main)

import Browser exposing (Document)
import Json.Decode as D exposing (Decoder, Error, Value)
import Dict exposing (Dict)
import Elements exposing (boardElement, empty, profileElement)
import Html exposing (Html, button, div, h1, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Models exposing (Game, Profile, decodeCells, decodeGame, decodeProfile)

port fbLogin : () -> Cmd msg
port fbUpdateCells : String -> Cmd msg
port fbProfile : (Value -> msg) -> Sub msg
port fbCellsUpdated : (Value -> msg) -> Sub msg
port fbGameUpdated : (Value -> msg) -> Sub msg


main : Program () Model Msg
main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Player =
    { player : String
    }

type alias Model =
    { profile : Maybe Profile
    , busy : Bool
    , game : Maybe Game
    }

type Msg
    = NoOp
    | Login
    | LoginComplete (Result Error Profile)
    | Place Int Int
    | CellsUpdated (Result Error (List String))
    | GameUpdated (Result Error Game)

playerToken : String -> Html msg
playerToken name =
    span [ class("player-token") ] [ text (String.slice 0 1 name) ]

init : () -> ( Model , Cmd Msg )
init _ = (
    { profile = Nothing
    , busy = False
    , game = Nothing
    }, Cmd.none)

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
                    ) model.game
            in
            case cellString of
                Just value -> ( model, fbUpdateCells value )
                Nothing -> ( model, Cmd.none)
        GameUpdated result ->
            case result of
                Ok game ->
                    ( { model | game = Just game }
                    , Cmd.none )
                Err err ->
                    Debug.log (Debug.toString err)
                    ( model, Cmd.none )
        _ ->
            ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fbProfile (D.decodeValue decodeProfile >> LoginComplete)
        , fbCellsUpdated (D.decodeValue decodeCells >> CellsUpdated)
        , fbGameUpdated (D.decodeValue decodeGame >> GameUpdated)
        ]

view : Model -> Html Msg
view model =
    let
        cells = model.game |> Maybe.map .cells
    in
    div [ class("container") ]
        [ div [ class("row") ]
            [ h1 [ class("col text-center") ] [ text "Tic-Tac-Toe" ]
            ]
        , div [ class("row") ]
            [ case model.profile of
                Nothing ->
                    button
                       [ class("btn btn-primary")
                       , onClick Login
                       ]
                       [ text "Login" ]
                Just profile ->
                    div [ class("row") ]
                        [ div [ class("col") ]
                            [ profileElement profile
                            ]
                        , case cells of
                            Just cells_ ->
                                div [ class("col") ]
                                    [ boardElement Place playerToken cells_
                                    ]
                            Nothing -> empty
                        ]
            ]
        ]
