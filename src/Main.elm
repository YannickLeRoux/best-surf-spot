module Main exposing (main)

import Browser
import Direction exposing (Direction(..))
import Element exposing (..)
import Element.Background as Background
import Element.Input exposing (button)
import Html exposing (Html)
import Http exposing (expectJson)
import Json.Decode exposing (list)
import RemoteData exposing (RemoteData(..), WebData, andMap, toMaybe, unwrap)
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
    | FindBestSpot Model


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

        FindBestSpot mdl ->
            ( updateSpotsScores mdl, Cmd.none )


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


renderSpots : WebData (List SurfSpot) -> Element Msg
renderSpots spots =
    case toMaybe spots of
        Just s ->
            column [] (List.map (\spot -> row [] [ text ("Name: " ++ spot.name ++ " ,score: " ++ String.fromInt spot.score) ]) s)

        _ ->
            text "Nothing"


view : Model -> Html Msg
view model =
    layout [ padding 80 ] <|
        case checkIfLoaded model.surfSpots model.swellDirections model.windDirection model.surfHeight model.tide of
            Failure message ->
                el [] (text ("Error: " ++ errorToString message))

            Loading ->
                el [] (text "Loading...")

            Success _ ->
                let
                    blue =
                        Element.rgb255 238 238 238

                    purple =
                        Element.rgb255 250 218 218
                in
                column []
                    [ row []
                        [ button
                            [ Background.color blue
                            , Element.focused
                                [ Background.color purple ]
                            ]
                            { label = text "Find the best spot around", onPress = Just (FindBestSpot model) }
                        ]
                    , renderSpots model.surfSpots
                    ]

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



-- calculate score


updateSpotsScores : Model -> Model
updateSpotsScores model =
    let
        updatedSpots =
            RemoteData.map (List.map (updateSingleSpotScore model)) model.surfSpots
    in
    { model | surfSpots = updatedSpots }


updateSingleSpotScore : Model -> SurfSpot -> SurfSpot
updateSingleSpotScore model spot =
    { spot | score = updatedScore model spot }


updatedScore : Model -> SurfSpot -> Int
updatedScore model spot =
    let
        score =
            0
    in
    score |> scoreSwellDirection model spot |> scoreWindDirection model spot


scoreSwellDirection : Model -> SurfSpot -> Int -> Int
scoreSwellDirection model spot score =
    let
        currentSwell =
            unwrap [] identity model.swellDirections

        spotSwell =
            spot.idealConditions.swellDirection

        points =
            List.foldl
                (\swell acc ->
                    if List.member swell currentSwell then
                        acc + 1

                    else
                        acc
                )
                0
                spotSwell
    in
    score + points


scoreWindDirection : Model -> SurfSpot -> Int -> Int
scoreWindDirection model spot score =
    let
        currentWind =
            unwrap N identity model.windDirection

        spotWind =
            spot.idealConditions.windDirection

        points =
            if List.member currentWind spotWind then
                score + 1

            else
                score
    in
    score + points
