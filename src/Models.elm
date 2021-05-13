module Models exposing (..)

import Json.Decode as D exposing (Decoder)

-- MODEL

type alias Profile =
    { uid : String
    , anonymous: Bool
    , displayName : Maybe String
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
