module Swell exposing (Swell, swellsDirectionsDecoder)

import Direction exposing (Direction, degreeToDirectionDecoder, parseDegreeToDirection)
import Json.Decode as JD exposing (Decoder, at, fail, field, float, index, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required, requiredAt)


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


swellsDecoder : Decoder (List SwellDataPoint)
swellsDecoder =
    at [ "data", "wave" ] <| list swellDataPointDecoder


swellToDirection : List Swell -> List Direction
swellToDirection listSwells =
    List.map (parseDegreeToDirection << .direction) <| List.reverse <| List.sortBy .height listSwells


swellNextHourDecoder : Int -> Decoder SwellDataPoint
swellNextHourDecoder now =
    swellsDecoder |> JD.andThen (filterNextHour now)


swellsDirectionsDecoder : Int -> Decoder (List Direction)
swellsDirectionsDecoder now =
    swellNextHourDecoder now |> JD.map (.swells >> swellToDirection)


filterNextHour : Int -> List SwellDataPoint -> Decoder SwellDataPoint
filterNextHour now datapoints =
    case List.head <| List.filter (\x -> .timestamp x == now) datapoints of
        Nothing ->
            fail "Couldnt decode the wind direction"

        Just swell ->
            succeed swell
