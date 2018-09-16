module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Svg
import Svg.Attributes as SA
import Task
import Time


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


type alias TimeInfo =
    { time : Time.Posix
    , zone : Time.Zone
    }


type alias Model =
    { value : Int
    , resetValue : String
    , myTime : TimeInfo
    , clockTicking : Bool
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.clockTicking then
        Time.every 1000 ClockTick

    else
        Sub.none


init : () -> ( Model, Cmd Msg )
init flags =
    ( { value = 1, resetValue = "0", myTime = { time = Time.millisToPosix 0, zone = Time.utc }, clockTicking = True }
    , Task.perform AdjustTimeZone Time.here
    )


type Msg
    = Increment
    | Decrement
    | SetResetValue String
    | Reset
    | NewValueGenerated Int
    | GenerateNewValue
    | AdjustTimeZone Time.Zone
    | ClockTick Time.Posix
    | ChangeToUTC
    | ChangeToLocal
    | ToggleClockTicking


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | value = model.value + 1 }, Cmd.none )

        Decrement ->
            ( { model | value = model.value - 1 }, Cmd.none )

        SetResetValue x ->
            ( { model | resetValue = x }, Cmd.none )

        Reset ->
            ( { model | value = String.toInt model.resetValue |> Maybe.withDefault 0 }, Cmd.none )

        GenerateNewValue ->
            ( model, Random.generate NewValueGenerated (Random.int -999 999) )

        NewValueGenerated v ->
            ( { model | value = v }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | myTime = { time = model.myTime.time, zone = zone } }, Cmd.none )

        ClockTick time ->
            ( { model | myTime = { time = time, zone = model.myTime.zone } }, Cmd.none )

        ChangeToUTC ->
            ( { model | myTime = { time = model.myTime.time, zone = Time.utc } }, Cmd.none )

        ChangeToLocal ->
            ( model, Task.perform AdjustTimeZone Time.here )

        ToggleClockTicking ->
            ( { model | clockTicking = not model.clockTicking }, Cmd.none )


displayTime : TimeInfo -> String
displayTime info =
    (Time.toDay info.zone info.time |> String.fromInt)
        ++ " of "
        ++ (Time.toMonth info.zone info.time |> Debug.toString)
        ++ ", "
        ++ (Time.toHour info.zone info.time |> String.fromInt)
        ++ "h "
        ++ (Time.toMinute info.zone info.time |> String.fromInt)
        ++ "m "
        ++ (Time.toSecond info.zone info.time |> String.fromInt)
        ++ "s"


type alias Coords =
    { x : Int
    , y : Int
    }


secondHandCoordinates : Coords -> Int -> TimeInfo -> Coords
secondHandCoordinates center radius timeInfo =
    let
        tickDegrees =
            pi / 60

        second =
            Time.toSecond timeInfo.zone timeInfo.time

        dx =
            toFloat radius * cos (tickDegrees * toFloat second)

        dy =
            toFloat radius * sin (tickDegrees * toFloat second)
    in
    Coords (center.x + round dx) (center.y + round dy)


viewClock : TimeInfo -> Html Msg
viewClock timeInfo =
    let
        r =
            50

        center =
            Coords 100 100

        centerX =
            String.fromInt center.x

        centerY =
            String.fromInt center.y

        radius =
            String.fromInt r

        handCoords =
            secondHandCoordinates center r timeInfo
    in
    Svg.svg [ SA.width "200", SA.height "200" ]
        [ Svg.circle [ SA.cx centerX, SA.cy centerY, SA.r radius, SA.fill "none", SA.stroke "gray", SA.strokeWidth "2" ] []
        , Svg.circle [ SA.cx centerX, SA.cy centerY, SA.r "2", SA.fill "gray" ] []
        , Svg.line [ SA.x1 centerX, SA.y1 centerY, SA.x2 (String.fromInt handCoords.x), SA.y2 (String.fromInt handCoords.y), SA.stroke "gray" ] []
        ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.value) ]
        , button [ onClick Increment ] [ text "+" ]
        , hr [] []
        , input [ placeholder "reset to ...", value model.resetValue, onInput SetResetValue ] []
        , button [ onClick Reset ] [ text "reset" ]
        , button [ onClick GenerateNewValue ] [ text "random" ]
        , hr [] []
        , div []
            [ text (displayTime model.myTime)
            , button [ onClick ChangeToUTC ] [ text "Show as UTC time" ]
            , button [ onClick ChangeToLocal ] [ text "Show as local time" ]
            , button [ onClick ToggleClockTicking ]
                [ text
                    (if model.clockTicking then
                        "pause"

                     else
                        "resume"
                    )
                ]
            ]
        , div [] [ viewClock model.myTime ]
        ]
