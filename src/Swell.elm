module Swell exposing (Swell, swellsDirectionsDecoder)

import Direction exposing (Direction, degreeToDirectionDecoder, parseDegreeToDirection)
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


swellsDirectionsDecoder : Decoder (List Direction)
swellsDirectionsDecoder =
    Json.Decode.map mapLists swellsDecoder


mapLists : List Swell -> List Direction
mapLists listSwells =
    List.map (parseDegreeToDirection << .direction) listSwells



-- function produce a directon but not a list of direcion
