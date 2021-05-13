module Models exposing (..)

import Json.Decode as D exposing (Decoder)

-- MODEL

type alias Profile =
    { uid : String
    , anonymous: Bool
    , displayName : Maybe String
    }

type alias Game =
    { player1 : String
    , player2 : Maybe String
    , cells : List String
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
    D.map3 Game
        (D.field "player1" D.string)
        (D.maybe (D.field "player2" D.string))
        (D.field "cells" decodeCells)
