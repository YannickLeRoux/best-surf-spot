module Swell exposing (Swell, swellToDirection, swellsDirectionsDecoder)

import Direction exposing (Direction, parseDegreeToDirection)
import Json.Decode as JD exposing (Decoder, at, fail, float, int, list, succeed)
import Json.Decode.Pipeline exposing (required)


type alias Swell =
    { height : Float
    , period : Int
    , direction : Float
    , directionMin : Float
    , optimalScore : Int
    }


type alias Surf =
    { min : Float
    , max : Float
    , optimalScore : Int
    }


type alias SwellDataPoint =
    { timestamp : Int
    , surf : Surf
    , swells : List Swell
    }


swellDecoder : Decoder Swell
swellDecoder =
    succeed Swell
        |> required "height" float
        |> required "period" int
        |> required "direction" float
        |> required "directionMin" float
        |> required "optimalScore" int


surfDecoder : Decoder Surf
surfDecoder =
    succeed Surf
        |> required "min" float
        |> required "max" float
        |> required "optimalScore" int


swellDataPointDecoder : Decoder SwellDataPoint
swellDataPointDecoder =
    succeed SwellDataPoint
        |> required "timestamp" int
        |> required "surf" surfDecoder
        |> required "swells" (list swellDecoder)



-- 1/ Decode the whole data object of the json as a list of element


swellsDecoder : Decoder (List SwellDataPoint)
swellsDecoder =
    at [ "data", "wave" ] <| list swellDataPointDecoder



-- 2/ take only the element matching the current timestamp


swellNextHourDecoder : Int -> Decoder SwellDataPoint
swellNextHourDecoder now =
    swellsDecoder |> JD.andThen (filterNextHour now)


filterNextHour : Int -> List SwellDataPoint -> Decoder SwellDataPoint
filterNextHour now datapoints =
    case List.head <| List.filter (\x -> .timestamp x == now) datapoints of
        Nothing ->
            fail "Couldnt decode the wind direction"

        Just swell ->
            succeed swell



-- extract the float of the direction property and convert to a direction


swellToDirection : List Swell -> List Direction
swellToDirection listSwells =
    List.map (parseDegreeToDirection << .direction) <| List.reverse <| List.sortBy .height listSwells


swellsDirectionsDecoder : Int -> Decoder (List Direction)
swellsDirectionsDecoder now =
    swellNextHourDecoder now |> JD.map (.swells >> swellToDirection)
