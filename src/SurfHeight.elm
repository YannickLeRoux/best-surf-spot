module SurfHeight exposing (SurfHeight, heightDecoder)

import Json.Decode as JD exposing (Decoder, at, fail, field, float, index, int, list, map2, succeed)
import Json.Decode.Pipeline exposing (required)



-- DATA


type SurfHeight
    = SurfHeight Float Float


type alias Surf =
    { min : Float
    , max : Float
    , optimalScore : Int
    }


type alias Swell =
    { height : Float
    , period : Int
    , direction : Float
    , directionMin : Float
    , optimalScore : Int
    }


type alias SwellDataPoint =
    { timestamp : Int
    , surf : Surf
    , swells : List Swell
    }



-- DECODERS


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



-- 1/ decode the whole data object, its a list of elements with a timestamp each


swellsDecoder : Decoder (List SwellDataPoint)
swellsDecoder =
    at [ "data", "wave" ] <| list swellDataPointDecoder



-- 2/ Decode only the element corresponding to the current timestamp


filterNextHour : Int -> List SwellDataPoint -> Decoder SwellDataPoint
filterNextHour now datapoints =
    case List.head <| List.filter (\x -> .timestamp x == now) datapoints of
        Nothing ->
            fail "Couldnt decode the surf height"

        Just swell ->
            succeed swell


swellNextHourDecoder : Int -> Decoder SwellDataPoint
swellNextHourDecoder now =
    swellsDecoder |> JD.andThen (filterNextHour now)



-- 3/ extract the surf object
-- TODO Problem around these 2 functions


heightDecoder : Int -> Decoder SurfHeight
heightDecoder now =
    swellNextHourDecoder now |> JD.map (.surf >> surfToHeight)


surfToHeight : Surf -> SurfHeight
surfToHeight surf =
    SurfHeight surf.min surf.max
