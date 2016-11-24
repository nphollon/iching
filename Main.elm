module Main exposing (main)

import Array.Hamt as Array exposing (Array)
import Color
import Color.Manipulate
import Color.Convert
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
    | HexagramFadeIn Animation
    | HexagramSplit Animation
    | HexagramMorph Animation
    | TextFadeIn Animation
    | NoSplitTextFadeIn Animation
    | DoubleHexagram (List Line)
    | SingleHexagram (List Line)


type alias Animation =
    { progress : Float
    , hex : List Line
    }


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
subscriptions model =
    case model.phase of
        DoubleHexagram _ ->
            Window.resizes WindowSize

        SingleHexagram _ ->
            Window.resizes WindowSize

        _ ->
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
            if m.progress < 1 then
                HexagramFadeIn { m | progress = m.progress + 0.03 }
            else if anyChanges m.hex then
                HexagramSplit { m | progress = 0 }
            else
                NoSplitTextFadeIn { m | progress = 0 }

        HexagramSplit m ->
            if m.progress < 1 then
                HexagramSplit { m | progress = m.progress + 0.03 }
            else
                HexagramMorph { m | progress = 0 }

        HexagramMorph m ->
            if m.progress < 1 then
                HexagramMorph { m | progress = m.progress + 0.06 }
            else
                TextFadeIn { m | progress = 0 }

        TextFadeIn m ->
            if m.progress < 1 then
                TextFadeIn { m | progress = m.progress + 0.03 }
            else
                DoubleHexagram m.hex

        DoubleHexagram _ ->
            phase

        NoSplitTextFadeIn m ->
            if m.progress < 1 then
                NoSplitTextFadeIn { m | progress = m.progress + 0.03 }
            else
                SingleHexagram m.hex

        SingleHexagram _ ->
            phase


anyChanges : List Line -> Bool
anyChanges lines =
    List.any (uncurry xor) lines


view : Model -> Html Action
view model =
    let
        landscape =
            model.window.height < model.window.width
    in
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

                HexagramSplit { progress, hex } ->
                    drawSplit landscape progress hex

                HexagramMorph { progress, hex } ->
                    drawMorph landscape progress hex

                TextFadeIn { progress, hex } ->
                    drawTextFadeIn landscape progress hex

                DoubleHexagram hexagram ->
                    drawDoubleHexagram landscape hexagram

                NoSplitTextFadeIn { progress, hex } ->
                    drawNoSplitTextFadeIn progress hex

                SingleHexagram hexagram ->
                    drawSingleHexagram hexagram


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
    in
        [ Svg.g [ translate centerPosition ]
            (drawLinesFadingIn percent lines)
        ]


drawSplit : Bool -> Float -> List Line -> List (Svg a)
drawSplit landscape progress lines =
    let
        percent =
            min 1 (Ease.inOutQuint progress)

        towardsFirstPosition =
            mix percent centerPosition (firstPosition landscape)

        towardsSecondPosition =
            mix percent centerPosition (secondPosition landscape)
    in
        [ Svg.g [ translate towardsSecondPosition ]
            (drawLinesSplitting lines)
        , Svg.g [ translate towardsFirstPosition ]
            (drawFirstLines lines)
        ]


drawMorph : Bool -> Float -> List Line -> List (Svg a)
drawMorph landscape progress lines =
    let
        percent =
            min 1 (Ease.outQuad progress)

        towardsFirstPosition =
            mix percent centerPosition (firstPosition landscape)

        towardsSecondPosition =
            mix percent centerPosition (secondPosition landscape)
    in
        [ Svg.g [ translate (secondPosition landscape) ]
            (drawLinesMorphing percent lines)
        , Svg.g [ translate (firstPosition landscape) ]
            (drawFirstLines lines)
        ]


drawTextFadeIn : Bool -> Float -> List Line -> List (Svg a)
drawTextFadeIn landscape progress lines =
    let
        opacity =
            Attr.opacity <| toString progress
    in
        [ Svg.g
            [ translate (firstPosition landscape) ]
            (drawFirstLines lines)
        , Svg.g
            [ translate (firstPosition landscape)
            , opacity
            ]
            (drawText (List.map Tuple.first lines))
        , Svg.g
            [ translate (secondPosition landscape) ]
            (drawSecondLinesLightening progress lines)
        , Svg.g
            [ translate (secondPosition landscape)
            , opacity
            ]
            (drawText (List.map Tuple.second lines))
        ]


drawDoubleHexagram : Bool -> List Line -> List (Svg a)
drawDoubleHexagram landscape lines =
    [ Svg.g
        [ translate (firstPosition landscape) ]
        (drawFirstLines lines)
    , Svg.g
        [ translate (firstPosition landscape) ]
        (drawText (List.map Tuple.first lines))
    , Svg.g
        [ translate (secondPosition landscape) ]
        (drawSecondLines lines)
    , Svg.g
        [ translate (secondPosition landscape) ]
        (drawText (List.map Tuple.second lines))
    ]


drawNoSplitTextFadeIn : Float -> List Line -> List (Svg a)
drawNoSplitTextFadeIn progress lines =
    [ Svg.g
        [ translate centerPosition ]
        (drawFirstLines lines)
    , Svg.g
        [ translate centerPosition
        , Attr.opacity <| toString progress
        ]
        (drawText (List.map Tuple.first lines))
    ]


drawSingleHexagram : List Line -> List (Svg a)
drawSingleHexagram lines =
    [ Svg.g
        [ translate centerPosition ]
        (drawFirstLines lines)
    , Svg.g
        [ translate centerPosition ]
        (drawText (List.map Tuple.first lines))
    ]


mix : Float -> ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
mix percent ( x1, y1 ) ( x2, y2 ) =
    let
        m u v =
            (1 - percent) * toFloat u + percent * toFloat v
    in
        ( m x1 x2, m y1 y2 )


translate : ( number, number_ ) -> Attribute a
translate ( x, y ) =
    [ "translate(", toString x, ",", toString y, ")" ]
        |> String.concat
        |> Attr.transform


firstPosition : Bool -> ( Int, Int )
firstPosition landscape =
    if landscape then
        ( -dims.width - dims.innerMargin, -dims.height // 2 )
    else
        ( -dims.width // 2, -dims.height - dims.innerMargin )


secondPosition : Bool -> ( Int, Int )
secondPosition landscape =
    if landscape then
        ( dims.innerMargin, -dims.height // 2 )
    else
        ( -dims.width // 2, dims.innerMargin )


centerPosition : ( Int, Int )
centerPosition =
    ( -dims.width // 2, -dims.height // 2 )


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


drawLinesFadingIn : Float -> List Line -> List (Svg a)
drawLinesFadingIn percent lines =
    lines
        |> List.indexedMap
            (\i ( first, _ ) ->
                drawHalfLine "white" percent i first
            )


drawLinesSplitting : List Line -> List (Svg a)
drawLinesSplitting lines =
    lines
        |> List.indexedMap
            (\i ( first, _ ) ->
                drawHalfLine "blue" 1 i first
            )


drawLinesMorphing : Float -> List Line -> List (Svg a)
drawLinesMorphing percent lines =
    lines
        |> List.indexedMap
            (\i ( first, second ) ->
                case ( first, second ) of
                    ( True, False ) ->
                        drawHalfLineMorphing (1 - percent) i

                    ( False, True ) ->
                        drawHalfLineMorphing percent i

                    _ ->
                        drawHalfLine "blue" 1 i first
            )


drawFirstLines : List Line -> List (Svg a)
drawFirstLines lines =
    lines
        |> List.indexedMap
            (\i ( first, _ ) ->
                drawHalfLine "white" 1 i first
            )


drawSecondLinesLightening : Float -> List Line -> List (Svg a)
drawSecondLinesLightening progress lines =
    let
        color =
            Color.Convert.colorToHex <|
                Color.Manipulate.weightedMix
                    Color.white
                    (Color.rgb 0 0 255)
                    progress
    in
        lines
            |> List.indexedMap
                (\i ( _, second ) ->
                    drawHalfLine color 1 i second
                )


drawSecondLines : List Line -> List (Svg a)
drawSecondLines lines =
    lines
        |> List.indexedMap
            (\i ( _, second ) ->
                drawHalfLine "white" 1 i second
            )


drawHalfLineMorphing : Float -> Int -> Svg a
drawHalfLineMorphing percent index =
    let
        height =
            9

        yOffset =
            15 * toFloat index + 5

        xOffset =
            5 * percent

        rect x width =
            Svg.rect
                [ Attr.y <| toString yOffset
                , Attr.height <| toString height
                , Attr.x <| toString x
                , Attr.width <| toString width
                , Attr.fill "blue"
                ]
                []
    in
        Svg.g []
            [ rect 20 (25 + xOffset)
            , rect (55 - xOffset) (25 + xOffset)
            ]


drawHalfLine : String -> Float -> Int -> Bool -> Svg a
drawHalfLine color percent index isYang =
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
                , Attr.fill color
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


drawText : List Bool -> List (Svg a)
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
        numberLine :: title


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
