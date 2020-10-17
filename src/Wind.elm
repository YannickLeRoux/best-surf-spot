module Wind exposing (windDirectionDecoder)

import Direction exposing (Direction, degreeToDirectionDecoder, parseDegreeToDirection)
import Json.Decode as JD exposing (Decoder, andThen, at, fail, field, float, index, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required, requiredAt)


type alias WindDataPoint =
    { timestamp : Int
    , speed : Float
    , direction : Float
    , gust : Float
    , optimalScore : Int
    }


windDataPointDecoder : Decoder WindDataPoint
windDataPointDecoder =
    succeed WindDataPoint
        |> required "timestamp" int
        |> required "speed" float
        |> required "direction" float
        |> required "gust" float
        |> required "optimalScore" int


windListDecoder : Decoder (List WindDataPoint)
windListDecoder =
    at [ "data", "wind" ] <| list windDataPointDecoder


filterNextHour : Int -> List WindDataPoint -> Decoder WindDataPoint
filterNextHour now datapoints =
    case List.head <| List.filter (\x -> .timestamp x == now) datapoints of
        Nothing ->
            fail "Couldnt decode the wind direction"

        Just direction ->
            succeed direction


windNextHourDecoder : Int -> Decoder WindDataPoint
windNextHourDecoder now =
    windListDecoder |> JD.andThen (filterNextHour now)


windDirectionDecoder : Int -> Decoder Direction
windDirectionDecoder now =
    windNextHourDecoder now |> JD.map (.direction >> parseDegreeToDirection)
