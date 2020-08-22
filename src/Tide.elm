module Tide exposing (Tide(..), parseTide, tideDataDecoder, tideDecoder)

import Json.Decode as JD exposing (Decoder, at, field, float, index, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)


type Tide
    = Low
    | Mid
    | High
    | Normal --



-- type alias TideDataPoint =
--     { timestamp : Int
--     , type_ : String
--     , height : Float
--     }
-- tideDataDecoder : Decoder TideDataPoint
-- tideDataDecoder =
--     succeed TideDataPoint
--         |> required "timestamp" int
--         |> required "type" string
--         |> required "height" float


tideDataDecoder : Decoder Tide
tideDataDecoder =
    at [ "data", "tides" ] <| index 0 <| field "type" tideDecoder


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

        "NORMAL" ->
            Ok Mid

        "High" ->
            Ok High

        _ ->
            Err ("Invalid tide: " ++ string)
