module Tide exposing (Tide(..), parseTide, tideDecoder)

import Json.Decode as JD exposing (Decoder)


type Tide
    = Low
    | Mid
    | High


tideDecoder : Decoder Tide
tideDecoder =
    JD.string |> JD.andThen (fromResult << parseTide)


fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok a ->
            JD.succeed a

        Err errorMessage ->
            JD.fail errorMessage


parseTide : String -> Result String Tide
parseTide string =
    case string of
        "Low" ->
            Ok Low

        "Mid" ->
            Ok Mid

        "High" ->
            Ok High

        _ ->
            Err ("Invalid tide: " ++ string)
