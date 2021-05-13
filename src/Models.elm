module Models exposing (..)

import Json.Decode as D exposing (Decoder, Value)

-- MODEL

type alias Profile =
    { uid : String
    , anonymous: Bool
    , displayName : Maybe String
    }

type alias GameOverview =
    { id : String
    , title : String
    }

type alias Game =
    { id : String
    , player1 : String
    , player2 : Maybe String
    , cells : List String
    }

type alias TaggedValue =
    { tag : String
    , value : Value
    }

makeAnonProfile : String -> Profile
makeAnonProfile uid = Profile uid True Nothing

-- DECODERS
decodeProfile : Decoder Profile
decodeProfile =
    D.map3 Profile
        (D.field "uid" D.string)
        (D.field "isAnonymous" D.bool)
        (D.maybe (D.field "displayName" D.string))

decodeCells : Decoder (List String)
decodeCells =
    D.string |> D.andThen (String.split "" >> D.succeed)

decodeGame : Decoder Game
decodeGame =
    D.map4 Game
        (D.field "id" D.string)
        (D.field "player1" D.string)
        (D.maybe (D.field "player2" D.string))
        (D.field "cells" decodeCells)

decodeGameOverview : Decoder GameOverview
decodeGameOverview =
    D.map2 GameOverview
        (D.field "id" D.string)
        (D.field "title" D.string)

{-| Decode external JSON VALUE passed on from port in form of [tag String, value]
-}
decodeTaggedValue : Decoder TaggedValue
decodeTaggedValue =
    D.map2 TaggedValue
        (D.at ["0"] D.string)
        (D.at ["1"] D.value)
