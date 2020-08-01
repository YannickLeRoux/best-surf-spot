module CurrentConditions exposing (..)

import Direction exposing (Direction, degreeToDirectionDecoder)
import Json.Decode exposing (Decoder, andThen, index, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Tide exposing (Tide, tideDecoder)


type alias CurrentConditions =
    { swellDirection : List Direction
    , windDirection : Direction
    , surfHeight : ( Int, Int )
    , tide : Tide
    }


idealConditionsDecoder : Decoder CurrentConditions
idealConditionsDecoder =
    succeed CurrentConditions
        |> required "swellDirection" (list degreeToDirectionDecoder)
        |> required "windDirection" degreeToDirectionDecoder
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
