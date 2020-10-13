module Main exposing (getNextHour, main)

import Browser
import CurrentConditions exposing (CurrentConditions)
import Direction exposing (Direction(..), fromResultDirection, parseDegreeToDirection)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http exposing (expectJson)
import IdealConditions exposing (IdealConditions)
import Json.Decode as JD exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Maybe.Extra exposing (combine)
import RemoteData exposing (RemoteData(..), WebData, andMap)
import RemoteData.Http
import SurfHeight exposing (SurfHeight, heightDecoder)
import SurfSpot exposing (SurfSpot, spotDecoder)
import Swell exposing (Swell, swellsDirectionsDecoder)
import Task
import Tide exposing (Tide(..), TideDataPoint, listTideDecoder, tideFromFloatDecoder, tideFromTides)
import Time exposing (Month(..), Zone, posixToMillis)
import Time.Extra exposing (Interval(..), ceiling)
import Wind exposing (windDirectionDecoder)



-- MODEL


type alias Model =
    { surfSpots : WebData (List SurfSpot)
    , swellDirections : WebData (List Direction)
    , windDirection : WebData Direction
    , surfHeight : WebData ( Float, Float )
    , tide : WebData Tide
    , zone : Time.Zone
    , time : Time.Posix
    }


initialModel : Model
initialModel =
    { surfSpots = Loading
    , swellDirections = Loading
    , windDirection = Loading
    , surfHeight = Loading
    , tide = Loading
    , zone = Time.utc
    , time = Time.millisToPosix 0
    }


windDecoder : Model -> Decoder Direction
windDecoder model =
    windDirectionDecoder (getNextHour model.zone model.time)


initialCmd : Cmd Msg
initialCmd =
    Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Task.perform GetTime Time.now
        ]


makeRequests : Time.Zone -> Time.Posix -> Cmd Msg
makeRequests zone time =
    let
        getSpotsFromDBUrl =
            "http://localhost:3000/spots"

        getWindUrl =
            "https://services.surfline.com/kbyg/spots/forecasts/wind?spotId=5842041f4e65fad6a770883b&days=1"

        getWaveUrl =
            "https://services.surfline.com/kbyg/spots/forecasts/wave?spotId=5842041f4e65fad6a770883b&days=1"

        getTideUrl =
            "https://services.surfline.com/kbyg/spots/forecasts/tides?spotId=5842041f4e65fad6a770883b&days=1"
    in
    Cmd.batch
        [ getWindDirection zone time
        , Http.get
            { url = getWaveUrl
            , expect = expectJson (RemoteData.fromResult >> GotSwells) swellsDirectionsDecoder
            }
        , Http.get
            { url = getWaveUrl
            , expect = expectJson (RemoteData.fromResult >> GotHeight) heightDecoder
            }
        , Http.get
            { url = getTideUrl
            , expect = expectJson (RemoteData.fromResult >> GotTide) tideFromFloatDecoder
            }
        , Http.get
            { url = getSpotsFromDBUrl
            , expect = expectJson (RemoteData.fromResult >> GotSurfSpots) (list spotDecoder)
            }
        ]


getWindDirection : Time.Zone -> Time.Posix -> Cmd Msg
getWindDirection zone time =
    Http.get
        { url = "https://services.surfline.com/kbyg/spots/forecasts/wind?spotId=5842041f4e65fad6a770883b&days=1"
        , expect = expectJson (RemoteData.fromResult >> GotWindDirection) <| windDirectionDecoder <| Debug.log "get next hour" (getNextHour zone time)
        }


type FiveData a b c d e
    = FiveData a b c d e



-- UPDATE


type Msg
    = GotSurfSpots (WebData (List SurfSpot))
    | GotWindDirection (WebData Direction)
    | GotSwells (WebData (List Direction))
    | GotTide (WebData Tide)
    | GotHeight (WebData ( Float, Float ))
    | AdjustTimeZone Time.Zone
    | GetTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTime time ->
            ( { model | time = time }, makeRequests model.zone time )

        -- change to california
        AdjustTimeZone tz ->
            ( { model | zone = tz }, Cmd.none )

        GotSurfSpots surfSpots ->
            ( { model | surfSpots = surfSpots }, Cmd.none )

        GotWindDirection windDirection ->
            ( { model | windDirection = windDirection }, Cmd.none )

        GotSwells swells ->
            ( { model | swellDirections = swells }, Cmd.none )

        GotTide tide ->
            ( { model | tide = tide }, Cmd.none )

        GotHeight surfHeight ->
            ( { model | surfHeight = surfHeight }, Cmd.none )


getNextHour : Time.Zone -> Time.Posix -> Int
getNextHour z t =
    Time.Extra.ceiling Hour z t |> posixToMillis |> (\x -> x // 1000)


checkIfLoaded :
    WebData (List SurfSpot)
    -> WebData (List Direction)
    -> WebData Direction
    -> WebData ( Float, Float )
    -> WebData Tide
    -> WebData (FiveData (List SurfSpot) (List Direction) Direction ( Float, Float ) Tide)
checkIfLoaded a b c d e =
    RemoteData.map (\f g h i j -> FiveData f g h i j) a |> andMap b |> andMap c |> andMap d |> andMap e


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



-- VIEW


view : Model -> Html Msg
view model =
    layout [ padding 80 ] <|
        case checkIfLoaded model.surfSpots model.swellDirections model.windDirection model.surfHeight model.tide of
            Failure message ->
                el [] (text ("Error: " ++ errorToString message))

            Loading ->
                el [] (text "Loading...")

            Success loadedData ->
                el [] (text "Success")

            NotAsked ->
                el [] (text "Not asked...")



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
