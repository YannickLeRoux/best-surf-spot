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
        , swellDirection : Maybe (List Direction)
        , windDirection : Maybe Direction
        , surfHeight : Maybe ( Int, Int )
        , tide : Maybe Tide
        }
    | Success
        { surfSpots : List SurfSpot
        , swellDirection : List Direction
        , windDirection : Direction
        , surfHeight : ( Int, Int )
        , tide : Tide
        }
    | Error String


initialModel : Model
initialModel =
    Loading
        { surfSpots = Nothing
        , swellDirection = Nothing
        , windDirection = Nothing
        , surfHeight = Nothing
        , tide = Nothing
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


type FiveMaybes a b c d e
    = FiveMaybes (Maybe a) (Maybe b) (Maybe c) (Maybe d) (Maybe e)


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
                            checkIfLoaded { loadingData | windDirection = Just windDirection }

                        GotWindDirection (Err errorMsg) ->
                            Error ("Error fetching wind info:" ++ errorToString errorMsg)

                Success loadedModel ->
                    case msg of
                        GotSurfSpots _ ->
                            model

                        GotWindDirection _ ->
                            model

                Error _ ->
                    model
    in
    ( updatedModel, Cmd.none )



-- Check if all data is loaded, if so change model to Sucess


checkIfLoaded :
    { surfSpots : Maybe (List SurfSpot)
    , swellDirection : Maybe (List Direction)
    , windDirection : Maybe Direction
    , surfHeight : Maybe ( Int, Int )
    , tide : Maybe Tide
    }
    -> Model
checkIfLoaded data =
    case FiveMaybes data.surfSpots data.swellDirection data.windDirection data.surfHeight data.tide of
        FiveMaybes (Just surfSpots) (Just swellDirection) (Just windDirection) (Just surfHeight) (Just tide) ->
            Success
                { surfSpots = surfSpots
                , swellDirection = swellDirection
                , windDirection = windDirection
                , surfHeight = surfHeight
                , tide = tide
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
