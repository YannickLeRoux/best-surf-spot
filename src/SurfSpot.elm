module SurfSpot exposing (SurfSpot, spotDecoder)

import IdealConditions exposing (IdealConditions, idealConditionsDecoder)
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)


type alias SurfSpot =
    { id : Int
    , name : String
    , idealConditions : IdealConditions
    , score : Int
    }



-- DECODER


spotDecoder : Decoder SurfSpot
spotDecoder =
    succeed SurfSpot
        |> required "id" int
        |> required "name" string
        |> required "idealConditions" idealConditionsDecoder
        |> hardcoded 0



-- condtionsDecoder: Decoder Conditions
