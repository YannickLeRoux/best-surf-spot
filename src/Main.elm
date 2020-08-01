module Main exposing (..)

import Browser
import CurrentConditions exposing (CurrentConditions)
import Direction exposing (Direction(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import IdealConditions exposing (IdealConditions)
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import SurfSpot exposing (SurfSpot, spotDecoder)
import Tide exposing (Tide(..), parseTide)
import Wind exposing (windDirectionDecoder)



-- MODEL


type Model
    = Loading
        { surfSpots : Maybe (List SurfSpot)
        , currentConditions : Maybe CurrentConditions
        }
    | Success
        { surfSpots : List SurfSpot
        , currentConditions : CurrentConditions
        }
    | Error String


initialModel : Model
initialModel =
    Loading
        { surfSpots = Nothing
        , currentConditions = Nothing
        }


initialCmd : Cmd Msg
initialCmd =
    let
        getSpotsFromDBUrl =
            "http://localhost:3000/spots"

        getWindUrl =
            "https://services.surfline.com/kbyg/spots/forecasts/wind?spotId=5842041f4e65fad6a770883b&days=1&intervalHours=6"
    in
    Cmd.batch
        [ Http.get { url = getSpotsFromDBUrl, expect = Http.expectJson GotSurfSpots (list spotDecoder) }
        , Http.get { url = getWindUrl, expect = Http.expectJson GotWindDirection windDirectionDecoder }
        ]


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadUrl url ->
            "Malformed url: " ++ url

        Http.BadBody message ->
            "Bad Body: " ++ message

        Http.BadStatus status ->
            "Returning " ++ String.fromInt status ++ " status"



-- UPDATE


type Msg
    = GotSurfSpots (Result Http.Error (List SurfSpot))
    | GotWindDirection (Result Http.Error Direction)


updateWindDirection : Direction -> Model -> Model
updateWindDirection direction ({ currentConditions } as model) =
    case model of
        Loading { currentConditions } ->
            case direction of
                Just windDir ->
                    Loading {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatedModel =
            case model of
                Loading loadingData ->
                    case msg of
                        GotSurfSpots (Ok surfSpots) ->
                            checkIfLoaded { loadingData | surfSpots = Just surfSpots }

                        GotSurfSpots (Err errorMsg) ->
                            Error ("Error fetching surf spots:" ++ errorToString errorMsg)

                        GotWindDirection (Ok windDirection) ->
                            checkIfLoaded { loadingData | currentConditions = updateWindDirection windDirection model }

                Success loadedModel ->
                    case msg of
                        GotSurfSpots _ ->
                            model

                Error _ ->
                    model
    in
    ( updatedModel, Cmd.none )


checkIfLoaded :
    { surfSpots : Maybe (List SurfSpot)
    , currentConditions : Maybe CurrentConditions
    }
    -> Model
checkIfLoaded data =
    case ( data.surfSpots, data.currentConditions ) of
        ( Just surfSpots, Just currentConditions ) ->
            Success
                { surfSpots = surfSpots
                , currentConditions = currentConditions
                }

        _ ->
            Loading data



-- VIEW


view : Model -> Html Msg
view model =
    layout [ padding 80 ] <|
        case model of
            Error message ->
                el [] (text ("Problem!: " ++ message))

            Loading _ ->
                el [] (text "Loading...")

            Success loadedData ->
                el [] (text "Success")



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
