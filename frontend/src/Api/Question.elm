module Api.Question exposing (..)

import Api.Tag as Tag exposing (Tag)
import Iso8601
import Json.Decode as Decode exposing (Decoder, float, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Time


type alias Question =
    { id : Int
    , title : String
    , content : String
    , userId : Int
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , tags : List Tag
    }


decoder : Decoder Question
decoder =
    Decode.succeed Question
        |> required "id" int
        |> required "title" string
        |> required "content" string
        |> required "userId" int
        |> required "createdAt" Iso8601.decoder
        |> required "updatedAt" Iso8601.decoder
        |> required "tags" (Decode.list Tag.decoder)


listDecoder : Decoder (List Question)
listDecoder =
    Decode.list decoder
