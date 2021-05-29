module Elements exposing (..)

import Html exposing (Html, button, div, h5, li, p, small, span, strong, text, ul)
import Html.Attributes exposing (class, style, tabindex)
import Html.Events exposing (onClick)
import Models exposing (Game, GameOverview, JoinResponse(..), Profile)

empty : Html msg
empty = text ""

maybeText : Maybe String -> Html msg
maybeText str =
    case str of
        Nothing -> empty
        Just value -> text value
maybeElement : Maybe (Html msg) -> Html msg
maybeElement e =
    e |> Maybe.withDefault empty

opponentsElement : Maybe msg -> String -> Maybe String -> Maybe String -> Html msg
opponentsElement onRequestToJoinGame player1 player2 activePlayer =
    ul [ class "list-group mb-3" ]
        ( onRequestToJoinGame
        |> Maybe.map (\onRequestToJoinGame_ ->
            [ li [ class "list-group-item" ]
                [ span [ class "font-weight-bold" ] [ text "Player 1: " ]
                , text player1
                ]
            , li [ class "list-group-item" ]
                [ button [ onClick onRequestToJoinGame_, class "btn btn-primary d-block mx-auto mt-2" ]
                    [ text "Join game!"
                    ]
                ]
            ] )
        |> Maybe.withDefault ( Maybe.map2 (\activePlayer_ player2_ ->
            [ li [ [ "list-group-item", ( if activePlayer_ == player1 then "active-player" else "" ) ]
                    |> String.join " "
                    |> class
                ]
                [ span [ class "font-weight-bold" ] [ text "Player 1: " ]
                , text player1
                ]
            , li [ [ "list-group-item", ( if activePlayer_ == player2_ then "active-player" else "" ) ]
                      |> String.join " "
                      |> class
                      ]
                [ span [ class "font-weight-bold" ] [ text "Player 2: " ]
                , text player2_
                ]
            ]
            ) activePlayer player2
            |> Maybe.withDefault []
        ) )

requestedOpponentsElement : String -> Html msg
requestedOpponentsElement player1 =
    ul [ class "list-group mb-3" ]
        [ li [ class "list-group-item" ]
            [ span [ class "font-weight-bold" ] [ text "Player 1: " ]
            , text player1
            ]
        , li [ class "list-group-item" ]
            [ text "You have already requested to join"
            ]
        ]

boardElement : Maybe msg -> (Int -> Int -> msg) -> (String -> Html msg) -> Game -> Html msg
boardElement onRequestToJoinGame onPlace cellRenderer game =
    div [ class("board") ]
        [ opponentsElement onRequestToJoinGame game.player1 game.player2 game.activePlayer
        , div []
            ( game.cells
            |> nPartition 3
            |> List.indexedMap (\y row ->
                div [ class("board-row flex-grow-0") ]
                    ( row |> List.indexedMap (\x name ->
                    div [ class("board-cell")
                        , onClick (onPlace x y)
                        ] [ cellRenderer name ] )
                    )
                )
            )
        ]

profileElement : (String -> msg) -> (String -> msg) -> Bool -> Profile -> Html msg
profileElement onSelectGame onShareGame shareLinkCopied p =
    div [ class("media profile") ]
        [ span [ class("media-object align-self-start flex-shrink-0 text-uppercase mr-3") ]
            [ text (String.slice 0 1 p.uid)
            ]
        , if p.anonymous
        then
            div [ class("media-body") ]
                [ h5 [ class("mt-0") ] [ text ("Anonymous (" ++ p.uid ++ ")") ]
                , button
                    [ class "ml-auto btn btn-sm btn-link"
                    , onClick (onSelectGame p.uid)
                    ] [ text "My Game" ]
                , if shareLinkCopied
                then span [ class "btn btn-sm btn-link muted" ] [ text "Share Link Copied" ]
                else button
                    [ class "ml-auto btn btn-sm btn-link"
                    , onClick (onShareGame p.uid)
                    ] [ text "Share Link" ]
                ]
        else
            div [ class("media-body") ]
                [ h5 [ class("mt-0") ] [maybeText p.displayName ]
                ]
        ]

playerListElement : List Profile -> Html msg
playerListElement playerProfiles =
    empty

gameListElement : (String -> msg) -> Maybe Profile -> List GameOverview -> Html msg
gameListElement onSelectGame playerProfile gameList =
    let
        predicate : String -> Bool
        predicate = playerProfile
                |> Maybe.map (\profile -> (\gameId -> profile.uid /= gameId ) )
                |> Maybe.withDefault (\a -> True)
    in
    ul [ class("list-group") ]
        ( gameList
        |> List.filter (.id >> predicate)
        |> List.map (\game ->
            li [ class("list-group-item list-group-item-action d-flex align-items-center")
               ]
                [ text game.title
                , button
                    [ class "ml-auto btn btn-sm btn-info"
                    , onClick (onSelectGame game.id)
                    ] [ text "View" ]
                ]
            )
        )

requesterToastElement : msg -> msg -> Html msg
requesterToastElement onAccept onCancel =
    div [ class "position-fixed top-0 right-0 p-3" ]
        [ div [ class "toast", style "opacity" "1" ]
            [ div [ class "toast-header" ]
                [ small [ class "mr-auto" ] [ text "Few seconds ago" ]
                , button [ class "btn btn-link btn-sm", onClick onCancel ]
                    [ span [] [text "Close" ]
                    ]
                ]
            , div [ class "toast-body" ]
                [ p [] [ text "Someone has requested to join your game" ]
                , button [ class "btn btn-primary btn-sm mx-auto", onClick onAccept ] [ text "Accept" ]
                ]
            ]
        ]


acknowledgeResponseToastElement : ( JoinResponse -> msg ) -> ( String -> msg ) -> JoinResponse -> Html msg
acknowledgeResponseToastElement onAcknowledge onSelectActiveGame response =
    div [ class "position-fixed top-0 right-0 p-3" ]
        [ div [ class "toast", style "opacity" "1" ]
            [ div [ class "toast-header" ]
                [ small [ class "mr-auto" ] [ text "Few seconds ago" ]
                , button [ class "btn btn-link btn-sm", onClick ( onAcknowledge response ) ]
                    [ text "Close" ]
                ]
            , div [ class "toast-body" ]
                ( case response of
                    Accepted gameId ->
                        [ p [] [ text "Your request has been accepted" ]
                        , button [ class "btn btn-link btn-sm", onClick ( onSelectActiveGame gameId ) ]
                            [ text "View Game" ]
                        ]
                    Rejected _ ->
                        [ p [] [ text "Your request has been rejected" ]
                        ]
                )
            ]
        ]


gameoverModalElement: String -> Maybe msg -> Maybe msg -> Html msg
gameoverModalElement message onRestart onLeave =
    div []
        [ div [ class "modal d-block", tabindex -1 ]
            [ div [ class "modal-dialog" ]
                [ div [ class "modal-content" ]
                    [ div [ class "modal-body"]
                        [ p [ class "display-4 text-center" ] [ text message ]
                        ]
                    , div [ class "modal-footer" ]
                        [ case onRestart of
                            Just onRestart_ ->
                                button
                                    [ class "btn btn-primary mx-auto"
                                    , onClick onRestart_
                                    ] [ text "NEW GAME" ]
                            Nothing -> empty
                        , case onLeave of
                            Just onLeave_ ->
                                button
                                    [ class "btn btn-primary mx-auto"
                                    , onClick onLeave_
                                    ] [ text "LEAVE GAME" ]
                            Nothing -> empty
                        ]
                    ]
                ]
            ]
        , div [ class "modal-backdrop fade show" ] []
        ]
{-| Partitions a flat list into n nested lists, retaining original ordering
-}
nPartition : Int-> List a -> List (List a)
nPartition n list =
    if n <= 0 then
        [list]
    else
        list
        |> List.indexedMap (\index item -> (((toFloat index) / toFloat(n)) |> floor, item))
        |> List.foldl (\(group, item) acc ->
            if (List.length acc) <= group then
                [item] :: acc
            else (
                case Maybe.map2 (\head_ tail_ ->
                    ( item :: head_, tail_ )
                    )
                    (List.head acc) (List.tail acc)
                of
                    Just (updatedHead, tail) -> updatedHead :: tail
                    _ -> []
            )
        ) []
        |> List.map List.reverse
        |> List.reverse
