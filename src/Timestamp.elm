module Timestamp exposing (..)

import Date
import Task
import Time exposing (Time, Posix)


type alias Model =
    Posix


type Msg
    = OnTime Time


getTime =
    Time.now
        |> Task.perform OnTime


update : Msg -> Model -> Model
update (OnTime t) model = t


getNextHourTimeStamp : Model -> Int
getNextHourTimeStamp model =
