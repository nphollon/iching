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


type Action
    = Consult
    | WindowSize Window.Size
    | Tick Float
    | NewHex (List Line)


type alias Model =
    { clock : Clock
    , time : Int
    , window : Window.Size
    , phase : Phase
    }


type Phase
    = Emptiness
    | Fade Int
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


period : Time
period =
    Time.millisecond * 33


init : ( Model, Cmd Action )
init =
    let
        defaultSize =
            { width = 0, height = 0 }
    in
        ( { window = defaultSize
          , clock = Clock.withPeriod period
          , time = 0
          , phase = Emptiness
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
        Consult ->
            { model | phase = Fade model.time } ! []

        WindowSize window ->
            { model | window = window } ! []

        NewHex hex ->
            { model | phase = Hexagram hex } ! []

        Tick dt ->
            let
                ( clock, time ) =
                    Clock.update always dt model.clock model.time

                cmd =
                    phaseTransition time model.phase
            in
                ( { model
                    | clock = clock
                    , time = time
                  }
                , cmd
                )


phaseTransition : Int -> Phase -> Cmd Action
phaseTransition currentTime phase =
    case phase of
        Fade startTime ->
            let
                elapsed =
                    toFloat (currentTime - startTime) * period
            in
                if elapsed > Time.second then
                    Random.generate NewHex generator
                else
                    Cmd.none

        _ ->
            Cmd.none


generator : Random.Generator (List Line)
generator =
    Random.int 1 8
        |> Random.map toLine
        |> Random.list 6


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


view : Model -> Html Action
view model =
    case model.phase of
        Emptiness ->
            stableWobbler model.window model.time

        Fade startTime ->
            fadingWobbler model.window startTime model.time

        Hexagram hexagram ->
            drawHexagram model.window hexagram


stableWobbler : Window.Size -> Int -> Html Action
stableWobbler window time =
    Html.div
        [ style
            [ ( "display", "flex" )
            , ( "justify-content", "center" )
            ]
        ]
        [ Svg.svg
            [ Attr.width <| toString <| window.width * 9 // 10
            , Attr.height <| toString <| window.height * 9 // 10
            , Attr.viewBox "0 0 120 120"
            ]
            (amplitudes time ++ [ consultButton ])
        ]


fadingWobbler : Window.Size -> Int -> Int -> Html Action
fadingWobbler window startTime currentTime =
    Html.div [] []


amplitudes : Int -> List (Svg a)
amplitudes t =
    Array.toList <|
        Array.initialize 13 <|
            \i ->
                let
                    baseRadius =
                        if i % 2 == 0 then
                            28.5 - 2 * toFloat i
                        else
                            28 - 2 * (toFloat i)

                    waveTime =
                        period * toFloat t / Time.second

                    phase =
                        0.3 * toFloat i

                    radius =
                        if i % 2 == 0 then
                            baseRadius
                        else
                            baseRadius
                                + sin (waveTime + phase)

                    color =
                        if i % 2 == 0 then
                            "black"
                        else
                            "white"
                in
                    Svg.circle
                        [ Attr.cx "60"
                        , Attr.cy "60"
                        , Attr.fill color
                        , Attr.r (toString radius)
                        ]
                        []


consultButton : Svg Action
consultButton =
    Svg.circle
        [ Attr.cx "60"
        , Attr.cy "60"
        , Attr.r "15"
        , Attr.fill "none"
        , Attr.pointerEvents "visible"
        , onClick Consult
        ]
        []


drawHexagram : Window.Size -> List Line -> Html a
drawHexagram window lines =
    let
        ( before, changes, after ) =
            List.foldr splitLine ( [], [], [] ) lines

        landscape =
            window.height < window.width

        positionAttrs =
            if landscape then
                [ Attr.height <| toString <| window.height * 9 // 10
                , Attr.width <| toString <| window.width * 4 // 10
                , Attr.viewBox "0 0 120 150"
                ]
            else
                [ Attr.width <| toString <| window.width * 9 // 10
                , Attr.height <| toString <| window.height * 5 // 11
                , Attr.viewBox "0 0 120 150"
                ]

        beforeGraphic =
            Svg.svg positionAttrs
                [ drawHalfHexagram before
                , drawText before
                ]

        afterGraphic =
            Svg.svg positionAttrs
                [ drawHalfHexagram after
                , drawText after
                ]

        flex =
            if landscape then
                [ ( "display", "flex" )
                , ( "justify-content", "space-around" )
                ]
            else
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "justify-content", "space-around" )
                , ( "align-items", "center" )
                ]
    in
        Html.div [ style flex ]
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


drawHalfHexagram : List Bool -> Svg a
drawHalfHexagram halfLines =
    List.indexedMap drawHalfLine halfLines
        |> Svg.g [ Attr.fill "white" ]


drawHalfLine : Int -> Bool -> Svg a
drawHalfLine index isYang =
    let
        rect x width =
            Svg.rect
                [ Attr.y <| toString <| 15 * index + 5
                , Attr.height "9"
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
