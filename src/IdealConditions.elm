module IdealConditions exposing (IdealConditions, idealConditionsDecoder)

import Direction exposing (Direction, stringToDirectionDecoder)
import Json.Decode exposing (Decoder, andThen, index, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Tide exposing (Tide, tideDecoder)


type alias IdealConditions =
    { swellDirection : List Direction
    , windDirection : List Direction
    , surfHeight : ( Int, Int )
    , tide : Tide
    }


idealConditionsDecoder : Decoder IdealConditions
idealConditionsDecoder =
    succeed IdealConditions
        |> required "swellDirection" (list stringToDirectionDecoder)
        |> required "windDirection" (list stringToDirectionDecoder)
        |> required "surfHeight" (arrayAsTuple2 int int)
        |> required "tide" tideDecoder


arrayAsTuple2 : Decoder a -> Decoder b -> Decoder ( a, b )
arrayAsTuple2 a b =
    index 0 a
        |> andThen
            (\aVal ->
                index 1 b
                    |> andThen (\bVal -> Json.Decode.succeed ( aVal, bVal ))
            )
