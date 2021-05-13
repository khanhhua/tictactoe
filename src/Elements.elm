module Elements exposing (..)

import Html exposing (Html, div, li, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Models exposing (GameOverview, Profile)

empty : Html msg
empty = text ""

maybeText : Maybe String -> Html msg
maybeText str =
    case str of
        Nothing -> empty
        Just value -> text value

boardElement : (Int -> Int -> msg) -> (String -> Html msg) -> List String -> Html msg
boardElement onPlace cellRenderer cells =
    div [ class("board mx-auto") ]
        ( cells
        |> nPartition 3
        |> List.indexedMap (\y row ->
            div [ class("row flex-grow-0") ]
                ( row |> List.indexedMap (\x name ->
                div [ class("col cell")
                    , onClick (onPlace x y)
                    ] [ cellRenderer name ] )
                )
            )
        )

profileElement : Profile -> Html msg
profileElement p =
    div [ class("card profile") ]
        [ if p.anonymous
        then
            div [ class("card-body") ]
                [ text ("Anonymous (" ++ p.uid ++ ")")
                ]
        else
            div [ class("card-body") ]
                [ maybeText p.displayName
                ]
        ]

playerListElement : List Profile -> Html msg
playerListElement playerProfiles =
    empty

gameListElement : (String -> msg) -> List GameOverview -> Html msg
gameListElement onSelectGame gameList =
    ul [ class("list-group") ]
        ( gameList
        |> List.map (\game ->
            li [ class("list-group-item list-group-item-action")
                , onClick (onSelectGame game.id)
                ] [ text game.title ]
            )
        )

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
