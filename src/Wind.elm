module Wind exposing (windDirectionDecoder)

import Direction exposing (Direction, degreeToDirectionDecoder)
import Json.Decode as JD exposing (Decoder, andThen, at, fail, field, float, index, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required, requiredAt)


type alias WindDataPoint =
    { timestamp : Int
    , speed : Float
    , direction : Int
    , gust : Float
    , optimalScore : Int
    }


windDataPointDecoder : Decoder WindDataPoint
windDataPointDecoder =
    succeed WindDataPoint
        |> required "timestamp" int
        |> required "speed" float
        |> required "direction" int
        |> required "gust" float
        |> required "optimalScore" int


windListDecoder : Decoder Int
windListDecoder =
    (at [ "data", "wind" ] <| list windDataPointDecoder) |> andThen (filterNextHour 123) |> andThen (succeed .direction)


filterNextHour : Int -> List WindDataPoint -> Decoder WindDataPoint
filterNextHour now datapoints =
    case List.head <| List.filter (\x -> .timestamp x == now) datapoints of
        Nothing ->
            fail "Couldnt decode the wind direction"

        Just direction ->
            succeed direction


windNextHourDecoder : Int -> Decoder WindDataPoint
windNextHourDecoder now =
    JD.map (filterNextHour now) windListDecoder


windDirectionDecoder : Int -> Decoder Direction
windDirectionDecoder now =
    JD.map (windNextHourDecoder now) degreeToDirectionDecoder
