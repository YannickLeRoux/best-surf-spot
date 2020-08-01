module Wind exposing (windDirectionDecoder)

import Direction exposing (Direction, degreeToDirectionDecoder)
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required, requiredAt)


windDirectionDecoder : Decoder Direction
windDirectionDecoder =
    at [ "data", "wind", "direction" ] degreeToDirectionDecoder
