module Tide exposing (Tide(..), TideDataPoint, listTideDecoder, tideDecoder, tideFromTides)

import Json.Decode as JD exposing (Decoder, at, field, float, index, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)


type Tide
    = Low
    | Mid
    | High


type alias TideDataPoint =
    { timestamp : Int
    , type_ : String
    , height : Float
    }


tideDataDecoder : Decoder TideDataPoint
tideDataDecoder =
    succeed TideDataPoint
        |> required "timestamp" int
        |> required "type" string
        |> required "height" float


listTideDecoder : Decoder (List TideDataPoint)
listTideDecoder =
    at [ "data", "tides" ] <| list tideDataDecoder


tideDecoder : Decoder Tide
tideDecoder =
    -- this is to parse the string coming from the DB
    JD.string |> JD.andThen (fromResult << parseTide)


normalize : Maybe Float -> Maybe Float -> Maybe Float -> Float
normalize value min max =
    -- code smell, it shouldnt return a 0 here, responsability is somewhre else
    case ( value, min, max ) of
        ( Just v, Just mn, Just mx ) ->
            (v - mn) / (mx - mn)

        _ ->
            0


tideFromTides : List TideDataPoint -> Tide
tideFromTides xs =
    let
        maxHeight =
            List.maximum (List.map .height xs)

        minHeight =
            List.minimum (List.map .height xs)

        value =
            normalize (Maybe.map .height <| List.head <| List.reverse <| xs) minHeight maxHeight
    in
    if value < 0.33 then
        Low

    else if value < 0.66 then
        Mid

    else
        High



-- heightToTideDecoder : Decoder Tide
-- heightToTideDecoder =
--     JD.float |> JD.andThen (fromResult << parseHeightToTide)
-- parseDegreeToDirection : Float -> Result String Tide
-- parseDegreeToDirection degree =
--     if degree < 22.5 then
--         Ok N
--     else if degree < 45.0 then
--         Ok NNE
--     else if degree < 67.5 then
--         Ok NE
--     else if degree < 90 then
--         Ok ENE
--     else if degree < 112.5 then
--         Ok E
--     else if degree < 135.0 then
--         Ok ESE
--     else if degree < 157.5 then
--         Ok SE
--     else if degree < 180.0 then
--         Ok SSE
--     else if degree < 202.5 then
--         Ok S
--     else if degree < 225.0 then
--         Ok SSW
--     else if degree < 247.5 then
--         Ok SW
--     else if degree < 270.0 then
--         Ok WSW
--     else if degree < 292.5 then
--         Ok W
--     else if degree < 315.0 then
--         Ok WNW
--     else if degree < 337.5 then
--         Ok NW
--     else
--         Ok NNW


fromResult :
    Result String a
    -> Decoder a -- here Decoder Tide
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
