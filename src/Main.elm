port module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation exposing (Key, replaceUrl)
import Dict
import Json.Encode as E
import Json.Decode as D exposing (Decoder, Error, Value)
import Elements exposing (boardElement, empty, gameListElement, gameoverModalElement, maybeElement, profileElement
    , requestedOpponentsElement, requesterToastElement, acknowledgeResponseToastElement)
import Html exposing (Html, button, div, h1, h5, li, nav, p, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Logic exposing (checkWinner, makeNewGame)
import Models exposing (Game, GameOverview, JoinResponse(..), Profile, decodeGame, decodeGameOverview, decodeProfile)
import Process
import Task

import Port.FirebaseApp as F exposing (FirebaseMsg(..), firebaseUpdate)
import Url exposing (Url, toString)

port bootstrap : String -> Cmd msg


main : Program () Model Msg
main = Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlRequest = (\_ -> NoOp)
    , onUrlChange = UrlChanged
    }

type alias Model =
    { fbModel : F.Model Msg -- F.Model is only aware of Msg defined actions (serialization - deserialization)
    , channels : List String
    , url : Url.Url
    , key : Key
    , initialized : Bool
    , busy : Bool
    , modalMessage : Maybe String
    , profile : Maybe Profile
    , activeGameId : Maybe String
    , activeGame : Maybe Game
    , gameList : List GameOverview
    , requesterList : List String
    , requestedList : List String
    , responseList : List JoinResponse
    , shareLinkCopied : Bool
    }

type Msg
    = NoOp
    | UrlChanged Url.Url
    | Login
    | LoginComplete Profile
    | GotMyGameOverview (Maybe GameOverview)
    | DidOpenChannel String
    | DidCloseChannel String
    | GotRequestToJoinGame String
    | SelectActiveGame String
    | GameListUpdated (List GameOverview)
    | Place Int Int
    | PlaceComplete Bool
    | GameUpdated Game
    | ShowModal String
    | StartNewGame Int
    | StartNewGameComplete Bool
    | RequestToJoinGame String
    | RequestToJoinGameComplete String
    | Player2AllowedToJoin String String
    | AcknowledgeGameJoinResponse JoinResponse
    | AllowRequestToJoinGame String String
    | AllowRequestToJoinGameComplete String String
    | DenyGame String String
    | DenyGameComplete String String
    | LeaveGame String
    | LeaveGameComplete Bool
    | FB (F.FirebaseMsg Msg)
    | FirebaseInitialized ( Maybe String )
    | ShareLink String
    | ShareLinkCopied Bool
    | Steps ( List Msg )

playerToken : String -> Html msg
playerToken token =
    span [ class("player-token player-token-" ++ token) ] [ text " " ]


firebaseInstance =
    F.firebaseApp FB
    |> F.config
        [ ( "apiKey", "AIzaSyBB6mJ8zC4HeqpnznHVnrqvu7vq3i05HQU" )
        , ( "authDomain", "tictactoe-7d2d5.firebaseapp.com" )
        , ( "projectId", "tictactoe-7d2d5" )
        , ( "storageBucket", "tictactoe-7d2d5.appspot.com")
        , ( "messagingSenderId", "1060266018339" )
        , ( "appId", "1:1060266018339:web:c3a29cb4f0361ab8435396" )
        , ( "databaseURL", "https://tictactoe-7d2d5-default-rtdb.europe-west1.firebasedatabase.app/" )
        ]

parseGame : String -> Maybe String
parseGame path =
    case String.split "/" path of
        [ _, "g", gameId ] -> Just gameId
        _ -> Nothing

init : () -> Url -> Key -> (Model, Cmd Msg )
init _ url key =
    let
        cmd = F.initialize firebaseInstance ( url.path |> parseGame >> FirebaseInitialized >> F.expectEmpty )
    in
    ( { fbModel = F.init firebaseInstance
        , channels = []
        , url = url
        , key = key
        , busy = False
        , initialized = False
        , profile = Nothing
        , activeGameId = Nothing
        , activeGame = Nothing
        , gameList = []
        , requesterList = []
        , requestedList = []
        , responseList = []
        , modalMessage = Nothing
        , shareLinkCopied = False
        }
    , cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FirebaseInitialized defaultGameId ->
            let
                decodeDictToList = D.dict decodeGameOverview
                    |> D.andThen ( Dict.values >> D.succeed )

                updated = defaultGameId
                    |> Maybe.map (\defaultGameId_ ->
                        ( { model | activeGameId = Just defaultGameId_ }
                        , Cmd.batch
                             [ replaceUrl model.key "/"
                             , F.listenOn firebaseInstance ( "games/" ++ defaultGameId_ ) "value"
                                 ( F.expect DidOpenChannel D.string )
                                 ( F.expect GameUpdated decodeGame )
                             , F.listenOn firebaseInstance "gameList" "value"
                                 ( F.expect DidOpenChannel D.string )
                                 ( F.expect GameListUpdated decodeDictToList )
                             , F.call firebaseInstance "fbIsLoggedIn"
                                 ( F.expect (\profile ->
                                     case profile of
                                        Just profile_ -> LoginComplete profile_
                                        Nothing -> NoOp
                                     ) ( D.maybe decodeProfile )
                                 )
                             ] )
                        )
                    |> Maybe.withDefault ( model
                        , Cmd.batch
                            [ F.call firebaseInstance "fbIsLoggedIn"
                                ( F.expect (\profile ->
                                    case profile of
                                        Just profile_ -> LoginComplete profile_
                                        Nothing -> NoOp
                                    ) ( D.maybe decodeProfile )
                                )
                            , F.listenOn firebaseInstance "gameList" "value"
                                ( F.expect DidOpenChannel D.string )
                                ( F.expect GameListUpdated decodeDictToList )
                            ]
                        )
            in
                updated
        Login ->
            ( { model | busy = True },
            F.call firebaseInstance "fbLogin" ( F.expect LoginComplete decodeProfile )
            )
        LoginComplete profile ->
            let
                refPath = E.string ( "gameList/" ++ profile.uid )
            in
            ( { model
            | profile = Just profile },
            F.call1 firebaseInstance "fbGetValueAt" refPath ( F.expect GotMyGameOverview ( D.maybe decodeGameOverview ) )
            )
        GotMyGameOverview Nothing ->
            let
                gameListItem = model.profile
                    |> Maybe.map (\profile ->
                        E.object
                            [ ( "id", E.string profile.uid )
                            , ( "title", profile.uid
                                |> String.slice 0 9
                                |> \uid -> String.append uid "'s game"
                                |> E.string
                                )
                            ]
                        )
                game = model.profile
                    |> Maybe.map (\profile ->
                        makeNewGame 5 profile.uid profile.uid
                    )

                cmd = ( Maybe.map3 (\uid gameListItem_ game_ ->
                            E.object
                                [ ( "gameList/" ++ uid, gameListItem_ )
                                , ( "games/" ++ uid, game_ )
                                ] )
                        ( model.profile |> Maybe.map .uid )
                        gameListItem
                        game
                    )
                    |> Maybe.map (\value -> F.call1 firebaseInstance "fbMultiUpdate" value
                        ( F.expectEmpty NoOp )
                    )
                    |> Maybe.withDefault Cmd.none
            in
            ( model, cmd )
        GotMyGameOverview (Just _) ->
            let
                cmd = model.profile
                    |> Maybe.map (\profile ->
                        let
                            refPath = String.join "" ["players/", profile.uid, "/requests"]
                            eventType = "child_added"
                        in
                        ( refPath, eventType )
                    )
                    |> Maybe.map (\( refPath, eventType ) ->
                        F.listenOn firebaseInstance refPath eventType
                            ( F.expect DidOpenChannel D.string )
                            ( F.expect GotRequestToJoinGame D.string )
                    )
                    |> Maybe.withDefault Cmd.none
            in
            ( model, cmd )
        DidOpenChannel channel ->
            ( { model | channels = channel :: model.channels }, Cmd.none )
        DidCloseChannel channel ->
            ( { model | channels = model.channels |> List.filter ( (/=) channel ) }, Cmd.none )
        SelectActiveGame gameId ->
            let
                cmdDeselect = Maybe.map (\activeGameId ->
                        let
                            channel = ( "games/" ++ activeGameId ++ ":value" )
                        in
                            F.listenOff firebaseInstance channel
                                ( F.expect (\bool ->
                                  if bool then DidCloseChannel channel
                                  else NoOp
                                  ) D.bool
                                )
                    ) model.activeGameId
                    |> Maybe.withDefault Cmd.none

                cmdSelect = F.listenOn firebaseInstance ( "games/" ++ gameId ) "value"
                    ( F.expect DidOpenChannel D.string )
                    ( F.expect GameUpdated decodeGame )
            in
            ( { model | activeGameId = Just gameId }, Cmd.batch
                [ cmdSelect
                , cmdDeselect
                ]
            )
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
                        , let key = y * game.size + x in
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
                        winner = Maybe.andThen (\(player1, player2, strideSize) ->
                            case checkWinner strideSize cells (x, y) of
                                Just "1" -> Just player1
                                Just "2" -> Just player2
                                _ -> Nothing
                            )
                                ( Maybe.map3 (\p1 p2 strideSize -> ( p1, p2, strideSize ) )
                                    ( model.activeGame |> Maybe.map .player1 )
                                    ( model.activeGame |> Maybe.andThen .player2 )
                                    ( model.activeGame |> Maybe.map .size )
                                )
                        refPath = E.string ( "games/" ++ gameId )
                        value = E.object
                            ( [ ( "activePlayer", E.string activePlayer )
                            , ( "cells", E.string (cells |> String.join "") )
                            ] ++
                            ( case winner of
                                Just winner_ ->
                                    [ ( "winner", winner_ |> E.string )
                                    ]
                                _ -> []
                            ) )
                        cmd = F.call2 firebaseInstance "fbUpdateValueAt" refPath value
                            ( F.expect PlaceComplete D.bool )
                    in
                    ( model, cmd )
                Nothing -> ( model, Cmd.none)
        PlaceComplete bool ->
            ( model, Cmd.none )
        GameListUpdated gameList ->
            ( { model | gameList = gameList }, Cmd.none )
        GameUpdated game ->
            let
                ( updatedModel, cmd ) = if (game.winner == Nothing) && (model.modalMessage /= Nothing)
                    then
                        ( { model
                        | activeGame = Just game
                        , modalMessage = Nothing
                        }, bootstrap "modal.hide" )
                    else
                        ( { model | activeGame = case model.activeGameId of
                            Nothing -> Nothing
                            Just activeGameId -> if game.id == activeGameId
                                then Just game
                                else Nothing
                        }
                        , ( Maybe.map3
                                (\_ winner profile ->
                                    Cmd.batch
                                        [ bootstrap "show.modal"
                                        , if winner == profile
                                            then Task.perform ShowModal (Task.succeed "You won!")
                                            else Task.perform ShowModal (Task.succeed "You lose!")
                                        ]
                                    )
                                game.player2
                                game.winner
                                (model.profile |> Maybe.map .uid)
                            )
                        |> Maybe.withDefault Cmd.none)
            in
            ( updatedModel, cmd )
        ShowModal message ->
            ( { model | modalMessage = Just message } , Cmd.none )
        StartNewGame strideSize ->
            let
                maybeCmd =
                    Maybe.map5
                        (\profile owner player2 gameId winner ->
                            if profile == owner then
                                let
                                    activePlayer = if player2 == Nothing then owner else winner
                                    refPath = E.string ( "games/" ++ gameId )
                                    value = makeNewGame strideSize gameId activePlayer
                                in
                                F.call2 firebaseInstance "fbUpdateValueAt" refPath value
                                    ( F.expect StartNewGameComplete D.bool )
                            else
                                Cmd.none
                        )
                        (model.profile |> Maybe.map .uid)
                        (model.activeGame |> Maybe.map .player1) -- player1 IS the owner
                        (model.activeGame |> Maybe.map .player2)
                        (model.activeGame |> Maybe.map .id)
                        (model.activeGame |> Maybe.andThen .winner)
            in
            case maybeCmd of
                Just cmd -> ( { model | modalMessage = Nothing }, cmd )
                Nothing -> ( model, Cmd.none )
        RequestToJoinGame gameId ->
            let
                cmd = Maybe.map (\player2 ->
                        let
                            refPath = E.string ( "players/" ++ gameId ++ "/requests" )
                            playerId = E.string player2
                        in
                        F.call2 firebaseInstance "fbPushValue" refPath playerId
                            ( F.expect (\bool ->
                                if bool
                                then RequestToJoinGameComplete gameId
                                else NoOp
                            ) D.bool )
                    ) (model.profile |> Maybe.map .uid)
                    |> Maybe.withDefault Cmd.none
            in
            ( { model
            | activeGameId = Just gameId
            }, cmd )
        RequestToJoinGameComplete gameId ->
            let
                refPath = "games/" ++ gameId ++ "/player2"
                eventType = "value"
                cmd = F.listenOn firebaseInstance refPath eventType
                    ( F.expectEmpty NoOp )
                    ( F.expect ( Player2AllowedToJoin gameId ) D.string )
            in
            ( { model | requestedList = gameId :: model.requestedList }
            , cmd )
        Player2AllowedToJoin gameId player2 -> -- gameId->player1
            let
                channel = "games/" ++ gameId ++ "/player2:value"
                cmd = case model.profile |> Maybe.map .uid of
                    Nothing -> Cmd.none
                    _ -> F.listenOff firebaseInstance channel ( F.expectEmpty NoOp )
            in
            ( { model
                | requestedList = model.requestedList |> List.filter ( (/=) gameId )
                , responseList = model.profile
                    |> Maybe.map .uid
                    |> Maybe.map (\profile ->
                            if profile == player2
                            then ( Accepted gameId ) :: model.responseList
                            else ( Rejected gameId ) :: model.responseList
                        )
                    |> Maybe.withDefault model.responseList
                }
            , cmd
            )
        AcknowledgeGameJoinResponse response ->
            ( { model
                | responseList = model.responseList |> List.filter ( (/=) response )
            }, Cmd.none )
        AllowRequestToJoinGame gameId player2 ->
            let
                updates = E.object
                    [ ( "players/" ++ gameId ++ "/requests", E.null )
                    , ( "games/" ++ gameId ++ "/player2", E.string player2)
                    ]

                cmd = F.call1 firebaseInstance "fbMultiUpdate" updates
                    ( F.expect (\bool ->
                            if bool then AllowRequestToJoinGameComplete gameId player2
                            else NoOp
                        ) D.bool )
            in
            ( { model | activeGameId = Just gameId }
            , cmd )
        AllowRequestToJoinGameComplete _ _ ->
            ( { model | requesterList = [] }, Cmd.none )
        GotRequestToJoinGame requesterUid ->
            ( { model
            | requesterList = requesterUid :: model.requesterList
            }, Cmd.none )
        DenyGame gameId requesterUid ->
            let
                refPath = E.string ( "players/" ++  gameId ++ "/requests" )
                value = E.string requesterUid
            in
                ( model
                , F.call2 firebaseInstance "fbRemoveValueFromList" refPath value
                    ( F.expect (\bool ->
                            if bool
                            then DenyGameComplete gameId requesterUid
                            else NoOp
                        ) D.bool
                    )
                )
        DenyGameComplete _ requesterUid ->
            ( { model | requesterList = model.requesterList |> List.filter ( (/=) requesterUid ) }, Cmd.none )
        LeaveGame gameId ->
            let
                cmd = Maybe.map2 (\profile player1 ->
                    let
                        refPath = E.string ( "games/" ++ gameId )
                        value = E.object
                            [ ( "activePlayer", E.string player1 )
                            , ( "player2", E.null )
                            ]
                        cmd2 = F.call2 firebaseInstance "fbUpdateValueAt" refPath value
                            ( F.expect LeaveGameComplete D.bool )
                    in
                    if profile /= player1
                    then
                        Cmd.batch
                            [ cmd2
                            , bootstrap "modal.hide"
                            ]
                    else
                        Cmd.none
                    )
                    (model.profile |> Maybe.map .uid)
                    (model.activeGame |> Maybe.map .player1 )
                    |> Maybe.withDefault Cmd.none
            in
            if cmd == Cmd.none then
                ( model, Cmd.none )
            else
                ( { model
                | activeGame = Nothing
                , modalMessage = Nothing
                }, cmd )
        LeaveGameComplete bool ->
            ( model, Cmd.none )
        UrlChanged url ->
            ( { model | url = url }, Cmd.none )
        ShareLink gameId ->
            let
                sharedLink = ( toString model.url ) ++ "g/" ++ gameId
            in
            ( model, F.call1 firebaseInstance "cbCopy"
                ( E.string sharedLink ) ( F.expect ShareLinkCopied D.bool )
            )
        ShareLinkCopied bool ->
            ( { model | shareLinkCopied = bool }
            , case bool of
                True -> Task.perform (\() -> ShareLinkCopied False) ( Process.sleep 1200 )
                False -> Cmd.none
            )
        Steps msgList ->
            let
                nextMsg = msgList |> List.head
                updatedMsgList = msgList |> List.tail

                cmd1 = Maybe.map (\nextMsg_ ->
                        Task.perform identity ( Task.succeed nextMsg_ )
                    ) nextMsg
                cmd2 = Maybe.map (\updated_ ->
                        Task.perform identity ( Task.succeed ( Steps updated_ ) )
                    ) updatedMsgList

                cmd = [ cmd1, cmd2 ]
                    |> List.filterMap identity
                    |> Cmd.batch
            in
            ( model, cmd )
        FB fbMsg ->
            let
                ( updatedModel, fbCmd ) = firebaseUpdate fbMsg model.fbModel
            in
            ( { model | fbModel = updatedModel }, fbCmd )
        _ ->
            ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map FB (firebaseInstance.subscriptions model.fbModel)

view : Model -> Document Msg
view model =
    { title = "Tic-Tac-Toe"
    , body =
         [ nav [class "navbar navbar-expand-lg navbar-light bg-light mb-3"]
             [ span [ class("navbar-brand") ] [ text "Tic-Tac-Toe" ]
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
                 [ div [ class("col-lg-3 col-md-12 mb-md-3") ]
                     [ gameListElement SelectActiveGame model.profile model.gameList ]
                 , div [ class("col-lg col-md-7") ]
                     [ if model.profile /= Nothing then
                         Maybe.map4 (\gameId profile player2 game ->
                             if 0 /= ( model.requestedList |> List.filter (gameId |> (==)) |> List.length )
                             then requestedOpponentsElement gameId
                             else if player2 == Nothing && gameId /= profile
                             then boardElement (Just (RequestToJoinGame gameId)) Nothing playerToken game
                             else boardElement Nothing ( Just Place ) playerToken game
                         )
                             (model.activeGame |> Maybe.map .id)
                             (model.profile |> Maybe.map .uid)
                             (model.activeGame |> Maybe.map .player2)
                             model.activeGame
                         |> Maybe.withDefault empty
                     else
                         Maybe.map (boardElement Nothing Nothing playerToken)
                             model.activeGame
                         |> Maybe.withDefault empty
                     ]
                 ,  div [ class("col-lg-3 col-md-5") ]
                     [ model.profile
                         |> Maybe.map ( profileElement SelectActiveGame ShareLink model.shareLinkCopied )
                         |> Maybe.withDefault empty
                     ]
                 ]
             ]
         , ( Maybe.map2 (\gameId requesterUid ->
                 requesterToastElement
                     (AllowRequestToJoinGame gameId requesterUid)
                     (DenyGame gameId requesterUid)
                 )
                 ( model.profile |> Maybe.map .uid )
                 ( model.requesterList |> List.head )
             )
             |> Maybe.withDefault empty
         , ( Maybe.map2 (\_ response ->
                acknowledgeResponseToastElement
                    AcknowledgeGameJoinResponse
                    (\gameId ->
                        Steps
                            [ AcknowledgeGameJoinResponse response
                            , SelectActiveGame gameId
                            ]
                    )
                    response
                )
                ( model.profile |> Maybe.map .uid )
                ( model.responseList |> List.head )
             )
             |> Maybe.withDefault empty
         , Maybe.map4 (\message gameId profile owner ->
             if profile == owner
             then
                 gameoverModalElement message (Just ( StartNewGame 5 ) ) Nothing
             else
                 gameoverModalElement message Nothing (Just (LeaveGame gameId))
             )
             model.modalMessage
             (model.activeGame |> Maybe.map .id)
             (model.profile |> Maybe.map .uid)
             (model.activeGame |> Maybe.map .player1)
             |> maybeElement
         ]
    }
