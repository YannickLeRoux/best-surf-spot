module Swell exposing (Swell, swellsDecoder)

import Direction exposing (Direction, degreeToDirectionDecoder)
import Json.Decode exposing (Decoder, at, field, float, index, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required, requiredAt)


type alias Swell =
    { height : Float
    , period : Int
    , direction : Float
    , directionMin : Float
    , optimalScore : Int
    }


swellDecoder : Decoder Swell
swellDecoder =
    succeed Swell
        |> required "height" float
        |> required "period" int
        |> required "direction" float
        |> required "directionMin" float
        |> required "optimalScore" int


swellsDecoder : Decoder (List Swell)
swellsDecoder =
    at [ "data", "wave" ] <| index 0 <| field "swells" <| list swellDecoder
