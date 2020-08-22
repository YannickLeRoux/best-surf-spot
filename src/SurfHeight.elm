module SurfHeight exposing (SurfHeight, heightDecoder)

import Json.Decode as JD exposing (Decoder, at, field, float, index, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (required)


type alias SurfHeight =
    ( Float, Float )


type alias MinMax =
    { min : Float
    , max : Float
    }


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
