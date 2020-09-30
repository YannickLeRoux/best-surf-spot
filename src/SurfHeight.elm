module SurfHeight exposing (SurfHeight, heightDecoder)

import Json.Decode as JD exposing (Decoder, at, field, float, index, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (required)



-- MODEL


type alias Wave =
    List WaveElement


type alias WaveElement =
    { timestamp : Int
    , surf : Surf
    , swells : List Swell
    }


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


type alias SurfHeight =
    ( Float, Float )


type alias MinMax =
    { min : Float
    , max : Float
    }



-- DECODER


heightDecoder : Decoder SurfHeight
heightDecoder =
    at [ "data", "wave" ] <| index 0 <| field "surf" minMaxDecoder


minMaxDecoder : Decoder SurfHeight
minMaxDecoder =
    map2 MinMax (field "min" float) (field "max" float) |> JD.andThen (fromResult << parseHeight)


fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok a ->
            JD.succeed a

        Err errorMessage ->
            JD.fail errorMessage


fromMinMax : MinMax -> ( Float, Float )
fromMinMax { min, max } =
    ( min, max )


parseHeight : MinMax -> Result String SurfHeight
parseHeight { min, max } =
    Ok ( min, max )


wave : Decoder Wave
wave =
    list waveElement


waveElement : Decoder WaveElement
waveElement =
    succeed WaveElement
        |> required "timestamp" int
        |> required "surf" surf
        |> required "swells" (list swell)


surf : Decoder Surf
surf =
    succeed Surf
        |> required "min" float
        |> required "max" float
        |> required "optimalScore" int


swell : Decoder Swell
swell =
    succeed Swell
        |> required "height" float
        |> required "period" int
        |> required "direction" float
        |> required "directionMin" float
        |> required "optimalScore" int
