module Model exposing (..)

import Dict
import Json.Decode
import Json.Schema.Definitions as Defs
import Object


type alias Swagger =
    { definitions : Dict.Dict String Defs.Schema }


swaggerDecoder =
    Json.Decode.map Swagger
        (Json.Decode.field "definitions" (Json.Decode.dict Defs.decoder))


type alias RunningModel =
    { swagger : Swagger
    , object : Maybe Object.Object
    , note : Maybe String
    }


type Model
    = Failure String
    | Success RunningModel


type Msg
    = AddElement String
    | CopyYaml (Maybe Object.Object)
    | NoOp
    | RemoveElement String
    | Toggle String
    | ToggleAll Bool
    | UpdateName String String
    | UpdateNote (Maybe String)
    | UpdateSelectedType String
    | UpdateValue String String
