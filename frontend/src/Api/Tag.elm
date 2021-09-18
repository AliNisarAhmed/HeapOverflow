module Api.Tag exposing (..)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)


type alias Tag =
    { id : Int
    , title : String
    }


decoder : Decoder Tag
decoder =
    Decode.succeed Tag
        |> required "id" int
        |> required "title" string
