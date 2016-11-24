module Main exposing (main)

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Random
import String
import Task
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (style)
import Svg.Events exposing (onClick)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Window
import AnimationFrame
import Clock exposing (Clock)
import Ease


type Action
    = Consult Time
    | WindowSize Window.Size
    | Tick Time
    | NewHex (List Line)


type alias Model =
    { clock : Clock
    , window : Window.Size
    , phase : Phase
    }


type Phase
    = JustAButton Time
    | ButtonFadeOut
        { startTime : Time
        , elapsed : Time
        }
    | NeedHexagram
    | WaitingForHexagram
    | HexagramFadeIn
        { progress : Float
        , hex : List Line
        }
    | Hexagram (List Line)


type alias Line =
    ( Bool, Bool )


main : Program Never Model Action
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Action )
init =
    let
        defaultSize =
            { width = 0, height = 0 }
    in
        ( { window = defaultSize
          , clock = Clock.withPeriod (33 * Time.millisecond)
          , phase = JustAButton 0
          }
        , Task.perform WindowSize Window.size
        )


subscriptions : Model -> Sub Action
subscriptions _ =
    Sub.batch
        [ Window.resizes WindowSize
        , AnimationFrame.diffs Tick
        ]


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Consult time ->
            { model
                | phase =
                    ButtonFadeOut
                        { elapsed = 0
                        , startTime = time
                        }
            }
                ! []

        WindowSize window ->
            { model | window = window } ! []

        NewHex hex ->
            { model
                | phase =
                    HexagramFadeIn
                        { progress = 0
                        , hex = hex
                        }
            }
                ! []

        Tick dt ->
            timeUpdate dt model


timeUpdate : Time -> Model -> ( Model, Cmd Action )
timeUpdate dt model =
    case model.phase of
        NeedHexagram ->
            requestHexagram model

        _ ->
            let
                ( clock, phase ) =
                    Clock.update animationUpdate dt model.clock model.phase
            in
                { model | clock = clock, phase = phase } ! []


requestHexagram : Model -> ( Model, Cmd Action )
requestHexagram model =
    let
        cmd =
            Random.int 1 8
                |> Random.map toLine
                |> Random.list 6
                |> Random.generate NewHex
    in
        ( { model | phase = WaitingForHexagram }, cmd )


toLine : Int -> Line
toLine i =
    case i of
        1 ->
            ( False, False )

        2 ->
            ( False, False )

        3 ->
            ( True, True )

        4 ->
            ( False, False )

        5 ->
            ( True, True )

        6 ->
            ( True, True )

        7 ->
            ( False, True )

        8 ->
            ( True, False )

        _ ->
            Debug.crash "Out of range"


animationUpdate : Time -> Phase -> Phase
animationUpdate dt phase =
    case phase of
        JustAButton t ->
            JustAButton (t + dt)

        ButtonFadeOut m ->
            if m.elapsed > 2 * Time.second then
                NeedHexagram
            else
                ButtonFadeOut { m | elapsed = m.elapsed + dt }

        NeedHexagram ->
            NeedHexagram

        WaitingForHexagram ->
            WaitingForHexagram

        HexagramFadeIn m ->
            if m.progress > 1 then
                Hexagram m.hex
            else
                HexagramFadeIn { m | progress = m.progress + 0.03 }

        Hexagram _ ->
            phase


view : Model -> Html Action
view model =
    frame model.window <|
        case model.phase of
            JustAButton time ->
                stableWobbler time

            ButtonFadeOut { startTime, elapsed } ->
                fadingWobbler startTime elapsed

            NeedHexagram ->
                []

            WaitingForHexagram ->
                []

            HexagramFadeIn { progress, hex } ->
                drawFadeIn progress hex

            Hexagram hexagram ->
                drawHexagram model.window hexagram


frame : Window.Size -> List (Svg a) -> Html a
frame window contents =
    let
        slightlyLessThan x =
            toString (x * 24 // 25)
    in
        Html.div
            [ style
                [ ( "display", "flex" )
                , ( "justify-content", "center" )
                ]
            ]
            [ Svg.svg
                [ Attr.width <| slightlyLessThan window.width
                , Attr.height <| slightlyLessThan window.height
                , viewBox (window.height < window.width)
                ]
                contents
            ]


type alias Dimensions =
    { width : Int
    , height : Int
    , outerMargin : Int
    , innerMargin : Int
    }


dims : Dimensions
dims =
    { width = 120
    , height = 151
    , outerMargin = 5
    , innerMargin = 5
    }


viewBox : Bool -> Attribute a
viewBox landscape =
    let
        halfRadius =
            if landscape then
                { x = dims.outerMargin + dims.innerMargin + dims.width
                , y = dims.outerMargin + dims.height // 2
                }
            else
                { x = dims.outerMargin + dims.width // 2
                , y = dims.outerMargin + dims.innerMargin + dims.height
                }
    in
        [ -halfRadius.x
        , -halfRadius.y
        , 2 * halfRadius.x
        , 2 * halfRadius.y
        ]
            |> List.map toString
            |> String.join " "
            |> Attr.viewBox


firstPosition : Bool -> Attribute a
firstPosition landscape =
    if landscape then
        translate (-dims.width - dims.innerMargin) (-dims.height // 2)
    else
        translate (-dims.width // 2) (-dims.height - dims.innerMargin)


secondPosition : Bool -> Attribute a
secondPosition landscape =
    if landscape then
        translate dims.innerMargin (-dims.height // 2)
    else
        translate (-dims.width // 2) dims.innerMargin


centerPosition : Attribute a
centerPosition =
    translate (-dims.width // 2) (-dims.height // 2)


translate : Int -> Int -> Attribute a
translate x y =
    [ "translate(", toString x, ",", toString y, ")" ]
        |> String.concat
        |> Attr.transform


stableWobbler : Time -> List (Svg Action)
stableWobbler time =
    let
        consultButton =
            Svg.circle
                [ Attr.cx "0"
                , Attr.cy "0"
                , Attr.r "30"
                , Attr.fill "none"
                , Attr.pointerEvents "visible"
                , onClick (Consult time)
                ]
                []
    in
        wobbler (oscAmp time)
            ++ [ consultButton ]


fadingWobbler : Time -> Time -> List (Svg a)
fadingWobbler startTime elapsed =
    wobbler (fadeAmp startTime elapsed)


oscAmp : Time -> List Float
oscAmp t =
    Array.toList <|
        Array.initialize 13 <|
            \i ->
                let
                    angle =
                        t / 500 + 0.3 * toFloat i

                    baseRadius =
                        28 - 2 * toFloat i
                in
                    if i % 2 == 0 then
                        baseRadius + 0.5
                    else
                        baseRadius + sin angle


fadeAmp : Time -> Time -> List Float
fadeAmp startTime elapsed =
    Array.toList <|
        Array.initialize 13 <|
            \i ->
                let
                    angle =
                        (startTime + elapsed) / 500 + 0.3 * toFloat i

                    baseRadius =
                        28 - 2 * toFloat i

                    blackSweep =
                        0.02 * elapsed

                    whiteSweep =
                        0.018 * elapsed
                in
                    if i % 2 == 0 then
                        baseRadius + 0.5 + blackSweep
                    else
                        baseRadius + sin angle + whiteSweep


wobbler : List Float -> List (Svg a)
wobbler amplitudes =
    let
        color i =
            if i % 2 == 0 then
                "black"
            else
                "white"

        drawCircle i radius =
            Svg.circle
                [ Attr.cx "0"
                , Attr.cy "0"
                , Attr.fill (color i)
                , Attr.r (toString radius)
                ]
                []
    in
        List.indexedMap drawCircle amplitudes


drawFadeIn : Float -> List Line -> List (Svg a)
drawFadeIn progress lines =
    let
        percent =
            min 1 (Ease.inOutQuad progress)

        graphic =
            List.map Tuple.first lines
                |> drawHalfHexagram percent
    in
        [ Svg.g [ centerPosition ] [ graphic ] ]


drawHexagram : Window.Size -> List Line -> List (Svg a)
drawHexagram window lines =
    let
        ( before, changes, after ) =
            List.foldr splitLine ( [], [], [] ) lines

        landscape =
            window.height < window.width

        beforeGraphic =
            Svg.g
                [ firstPosition landscape ]
                [ drawHalfHexagram 1 before
                , drawText before
                ]

        afterGraphic =
            Svg.g
                [ secondPosition landscape ]
                [ drawHalfHexagram 1 after
                , drawText after
                ]
    in
        [ beforeGraphic
        , afterGraphic
        ]


type alias Triple a =
    ( a, a, a )


splitLine : Line -> Triple (List Bool) -> Triple (List Bool)
splitLine ( before, after ) ( befores, changes, afters ) =
    ( before :: befores
    , xor before after :: changes
    , after :: afters
    )


drawHalfHexagram : Float -> List Bool -> Svg a
drawHalfHexagram percent halfLines =
    List.indexedMap (drawHalfLine percent) halfLines
        |> Svg.g
            [ Attr.fill "white" ]


drawHalfLine : Float -> Int -> Bool -> Svg a
drawHalfLine percent index isYang =
    let
        fullHeight =
            9

        height =
            percent * 9

        yOffset =
            15 * toFloat index + 5

        y =
            yOffset + 0.5 * fullHeight * (1 - percent)

        rect x width =
            Svg.rect
                [ Attr.y <| toString y
                , Attr.height <| toString height
                , Attr.x <| toString x
                , Attr.width <| toString width
                ]
                []
    in
        if isYang then
            rect 20 60
        else
            Svg.g []
                [ rect 20 25
                , rect 55 25
                ]


drawText : List Bool -> Svg a
drawText halfLines =
    let
        fromBinary =
            List.map2
                (\isYang factor ->
                    if isYang then
                        0
                    else
                        factor
                )
                halfLines
                [ 32, 16, 8, 4, 2, 1 ]
                |> List.sum

        ( number, name ) =
            Dict.get fromBinary signDetails
                |> Maybe.withDefault ( -1, "" )

        numberLine =
            Svg.text_
                [ Attr.fontFamily "sans-serif"
                , Attr.fontSize "11"
                , Attr.fontWeight "bold"
                , Attr.fill "yellow"
                , Attr.x "5"
                , Attr.y "105"
                ]
                [ Svg.text (toString number) ]

        titleLine y text =
            Svg.text_
                [ Attr.fontFamily "serif"
                , Attr.fontSize "11"
                , Attr.fontStyle "italic"
                , Attr.fill "white"
                , Attr.x "5"
                , Attr.y y
                ]
                [ Svg.text text ]

        title =
            String.lines name
                |> List.map2 titleLine [ "125", "140" ]
    in
        Svg.g [] <| numberLine :: title


signDetails : Dict Int ( Int, String )
signDetails =
    Dict.fromList
        [ ( 0, ( 1, "The Creative" ) )
        , ( 1, ( 44, "Coming to Meet" ) )
        , ( 2, ( 13, "Fellowship\nwith Men" ) )
        , ( 3, ( 33, "Retreat" ) )
        , ( 4, ( 10, "Treading" ) )
        , ( 5, ( 6, "Conflict" ) )
        , ( 6, ( 25, "Innocence" ) )
        , ( 7, ( 12, "Standstill" ) )
        , ( 8, ( 9, "The Taming Power\nof the Small" ) )
        , ( 9, ( 57, "The Gentle" ) )
        , ( 10, ( 37, "The Family" ) )
        , ( 11, ( 53, "Development" ) )
        , ( 12, ( 61, "Inner Truth" ) )
        , ( 13, ( 59, "Dispersion" ) )
        , ( 14, ( 42, "Increase" ) )
        , ( 15, ( 20, "Contemplation" ) )
        , ( 16, ( 14, "Possession in\nGreat Measure" ) )
        , ( 17, ( 50, "The Caldron" ) )
        , ( 18, ( 30, "The Clinging" ) )
        , ( 19, ( 56, "The Wanderer" ) )
        , ( 20, ( 38, "Opposition" ) )
        , ( 21, ( 64, "Before Completion" ) )
        , ( 22, ( 21, "Biting Through" ) )
        , ( 23, ( 35, "Progress" ) )
        , ( 24, ( 26, "The Taming Power\nof the Great" ) )
        , ( 25, ( 18, "Work on What\nHas Been Spoiled" ) )
        , ( 26, ( 22, "Grace" ) )
        , ( 27, ( 52, "Keeping Still" ) )
        , ( 28, ( 41, "Decrease" ) )
        , ( 29, ( 4, "Youthful Folly" ) )
        , ( 30, ( 27, "The Corners\nof the Mouth" ) )
        , ( 31, ( 23, "Splitting Apart" ) )
        , ( 32, ( 43, "Breakthrough" ) )
        , ( 33, ( 28, "Preponderance\nof the Great" ) )
        , ( 34, ( 49, "Revolution" ) )
        , ( 35, ( 31, "Influence" ) )
        , ( 36, ( 58, "The Joyous" ) )
        , ( 37, ( 47, "Oppression" ) )
        , ( 38, ( 17, "Following" ) )
        , ( 39, ( 45, "Gathering\nTogether" ) )
        , ( 40, ( 5, "Waiting" ) )
        , ( 41, ( 48, "The Well" ) )
        , ( 42, ( 63, "Before Completion" ) )
        , ( 43, ( 39, "Obstruction" ) )
        , ( 44, ( 60, "Limitation" ) )
        , ( 45, ( 29, "The Abysmal" ) )
        , ( 46, ( 3, "Difficulty\nat the Beginning" ) )
        , ( 47, ( 8, "Holding Together" ) )
        , ( 48, ( 34, "The Power\nof the Great" ) )
        , ( 49, ( 32, "Duration" ) )
        , ( 50, ( 55, "Abundance" ) )
        , ( 51, ( 62, "Preponderance\nof the Small" ) )
        , ( 52, ( 54, "The Marrying\nMaiden" ) )
        , ( 53, ( 40, "Deliverance" ) )
        , ( 54, ( 51, "Shock, Thunder" ) )
        , ( 55, ( 16, "Enthusiasm" ) )
        , ( 56, ( 11, "Peace" ) )
        , ( 57, ( 46, "Pushing Upward" ) )
        , ( 58, ( 36, "Darkening\nof the Light" ) )
        , ( 59, ( 15, "Modesty" ) )
        , ( 60, ( 19, "Approach" ) )
        , ( 61, ( 7, "The Army" ) )
        , ( 62, ( 24, "Return" ) )
        , ( 63, ( 2, "The Receptive" ) )
        ]
