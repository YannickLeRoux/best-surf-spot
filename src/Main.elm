module Main exposing (main)

import Browser
import Direction exposing (Direction(..))
import Element exposing (..)
import Html exposing (Html)
import Http exposing (expectJson)
import Json.Decode exposing (list)
import RemoteData exposing (RemoteData(..), WebData, andMap)
import SurfHeight exposing (SurfHeight, heightDecoder)
import SurfSpot exposing (SurfSpot, spotDecoder)
import Swell exposing (swellsDirectionsDecoder)
import Task
import Tide exposing (Tide(..), tideFromFloatDecoder)
import Time exposing (Month(..), posixToMillis)
import Time.Extra exposing (Interval(..))
import Wind exposing (windDirectionDecoder)



-- MODEL


type alias Model =
    { surfSpots : WebData (List SurfSpot)
    , swellDirections : WebData (List Direction)
    , windDirection : WebData Direction
    , surfHeight : WebData SurfHeight
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
        [ getWindDirection getWindUrl zone time
        , getSwellDirections getWaveUrl zone time
        , getWavesHeight getWaveUrl zone time
        , Http.get
            { url = getTideUrl
            , expect = expectJson (RemoteData.fromResult >> GotTide) tideFromFloatDecoder
            }
        , Http.get
            { url = getSpotsFromDBUrl
            , expect = expectJson (RemoteData.fromResult >> GotSurfSpots) (list spotDecoder)
            }
        ]



-- HTTP REQUESTS


getWindDirection : String -> Time.Zone -> Time.Posix -> Cmd Msg
getWindDirection url zone time =
    Http.get
        { url = url
        , expect = expectJson (RemoteData.fromResult >> GotWindDirection) <| windDirectionDecoder <| getNextHour zone time
        }


getSwellDirections : String -> Time.Zone -> Time.Posix -> Cmd Msg
getSwellDirections url zone time =
    Http.get
        { url = url
        , expect = expectJson (RemoteData.fromResult >> GotSwells) <| swellsDirectionsDecoder <| getNextHour zone time
        }


getWavesHeight : String -> Time.Zone -> Time.Posix -> Cmd Msg
getWavesHeight url zone time =
    Http.get
        { url = url
        , expect = expectJson (RemoteData.fromResult >> GotHeight) <| heightDecoder <| getNextHour zone time
        }


type FiveData a b c d e
    = FiveData a b c d e



-- UPDATE


type Msg
    = GotSurfSpots (WebData (List SurfSpot))
    | GotWindDirection (WebData Direction)
    | GotSwells (WebData (List Direction))
    | GotTide (WebData Tide)
    | GotHeight (WebData SurfHeight)
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
            ( { model | surfHeight = Debug.log "sdfsf" surfHeight }, Cmd.none )


getNextHour : Time.Zone -> Time.Posix -> Int
getNextHour z t =
    Time.Extra.ceiling Hour z t |> posixToMillis |> (\x -> x // 1000)


checkIfLoaded :
    WebData (List SurfSpot)
    -> WebData (List Direction)
    -> WebData Direction
    -> WebData SurfHeight
    -> WebData Tide
    -> WebData (FiveData (List SurfSpot) (List Direction) Direction SurfHeight Tide)
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

            Success _ ->
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
