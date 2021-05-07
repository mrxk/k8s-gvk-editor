module Main exposing (..)

import Browser
import Json.Decode
import Model
import Update
import View


main =
    Browser.element
        { init = init
        , update = Update.update
        , subscriptions = subscriptions
        , view = View.view
        }


init : Maybe String -> ( Model.Model, Cmd Model.Msg )
init maybeSwaggerString =
    case maybeSwaggerString of
        Nothing ->
            ( Model.Failure "swagger required", Cmd.none )

        Just swaggerString ->
            let
                swaggerDecodeResult =
                    Json.Decode.decodeString Model.swaggerDecoder swaggerString
            in
            case swaggerDecodeResult of
                Err err ->
                    ( Model.Failure (Json.Decode.errorToString err), Cmd.none )

                Ok swagger ->
                    ( Model.Success
                        { swagger = swagger
                        , object = Nothing
                        , note = Nothing
                        }
                    , Cmd.none
                    )


subscriptions : Model.Model -> Sub Model.Msg
subscriptions _ =
    Sub.none
