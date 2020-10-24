module Wind exposing (windDirectionDecoder)

import Direction exposing (Direction, parseDegreeToDirection)
import Json.Decode as JD exposing (Decoder, at, fail, float, int, list, succeed)
import Json.Decode.Pipeline exposing (required)


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



-- 1/ decoder the whole wind object


windListDecoder : Decoder (List WindDataPoint)
windListDecoder =
    at [ "data", "wind" ] <| list windDataPointDecoder



-- Now focus on the element with the current timestamp


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
