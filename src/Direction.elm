module Direction exposing (Direction, degreeToDirectionDecoder, fromResultDirection, parseDegreeToDirection, parseStringToDirection, stringToDirectionDecoder)

import Json.Decode as JD exposing (Decoder)


type Direction
    = N
    | E
    | S
    | W
    | NE
    | SE
    | SW
    | NW
    | NNE
    | ENE
    | ESE
    | SSE
    | SSW
    | WSW
    | WNW
    | NNW


stringToDirectionDecoder : Decoder Direction
stringToDirectionDecoder =
    JD.string |> JD.andThen (fromResultDirection << parseStringToDirection)


degreeToDirectionDecoder : Decoder Direction
degreeToDirectionDecoder =
    JD.float |> JD.map parseDegreeToDirection


fromResultDirection : Result String a -> Decoder a
fromResultDirection result =
    case result of
        Ok a ->
            JD.succeed a

        Err errorMessage ->
            JD.fail errorMessage


parseStringToDirection : String -> Result String Direction
parseStringToDirection string =
    case string of
        "N" ->
            Ok N

        "S" ->
            Ok S

        "E" ->
            Ok E

        "W" ->
            Ok W

        "NE" ->
            Ok NE

        "SE" ->
            Ok SE

        "SW" ->
            Ok SW

        "NW" ->
            Ok NW

        "NNE" ->
            Ok NNE

        "ENE" ->
            Ok ENE

        "ESE" ->
            Ok ESE

        "SSE" ->
            Ok SSE

        "SSW" ->
            Ok SSW

        "WSW" ->
            Ok WSW

        "WNW" ->
            Ok WNW

        "NNW" ->
            Ok NNW

        _ ->
            Err ("Invalid direction: " ++ string)


parseDegreeToDirection : Float -> Direction
parseDegreeToDirection degree =
    if degree < 22.5 then
        N

    else if degree < 45.0 then
        NNE

    else if degree < 67.5 then
        NE

    else if degree < 90 then
        ENE

    else if degree < 112.5 then
        E

    else if degree < 135.0 then
        ESE

    else if degree < 157.5 then
        SE

    else if degree < 180.0 then
        SSE

    else if degree < 202.5 then
        S

    else if degree < 225.0 then
        SSW

    else if degree < 247.5 then
        SW

    else if degree < 270.0 then
        WSW

    else if degree < 292.5 then
        W

    else if degree < 315.0 then
        WNW

    else if degree < 337.5 then
        NW

    else
        NNW
