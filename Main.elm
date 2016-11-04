module Main exposing (main)

import Dict exposing (Dict)
import Color exposing (Color)
import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Element exposing (Element)
import Text
import Random.Pcg as Rand


type Action
    = Refresh


type alias Model =
    { seed : Rand.Seed
    , hexagram : List Line
    }


type alias Line =
    ( Bool, Bool )


main : Program ( Int, Int )
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Int, Int ) -> ( Model, Cmd a )
init randomSeed =
    generateHexagram (uncurry Rand.initialSeed2 randomSeed)
        ! []


update : a -> Model -> ( Model, Cmd a )
update _ model =
    generateHexagram model.seed ! []


generateHexagram : Rand.Seed -> Model
generateHexagram seed =
    let
        ( hexagram, nextSeed ) =
            Rand.step generator seed
    in
        { seed = nextSeed
        , hexagram = hexagram
        }


generator : Rand.Generator (List Line)
generator =
    Rand.int 1 8
        |> Rand.map toLine
        |> Rand.list 6


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
    div []
        [ drawHexagram model.hexagram
        , button [ onClick Refresh ] [ text "Again" ]
        ]


drawHexagram : List Line -> Html a
drawHexagram lines =
    let
        ( before, changes, after ) =
            List.foldr splitLine ( [], [], [] ) lines

        beforeGraphic =
            [ Element.flow Element.right
                [ Element.spacer 30 1
                , drawHalfHexagram before
                , Element.spacer 20 1
                , drawChanges changes
                ]
            , Element.spacer 1 20
            , drawText before
            ]
                |> Element.flow Element.down
                |> Element.container 180 250 Element.topLeft

        afterGraphic =
            [ Element.flow Element.right
                [ Element.spacer 30 1
                , drawHalfHexagram after
                ]
            , Element.spacer 1 20
            , drawText after
            ]
                |> Element.flow Element.down
                |> Element.container 180 250 Element.topLeft
    in
        [ beforeGraphic
        , Element.spacer 60 1
        , afterGraphic
        ]
            |> Element.flow Element.right
            |> Element.container 500 300 Element.bottomRight
            |> Element.color backColor
            |> Element.toHtml


type alias Triple a =
    ( a, a, a )


splitLine : Line -> Triple (List Bool) -> Triple (List Bool)
splitLine ( before, after ) ( befores, changes, afters ) =
    ( before :: befores
    , xor before after :: changes
    , after :: afters
    )


drawHalfHexagram : List Bool -> Element
drawHalfHexagram halfLines =
    List.map drawHalfLine halfLines
        |> List.intersperse (Element.spacer 1 10)
        |> Element.flow Element.down


drawHalfLine : Bool -> Element
drawHalfLine isYang =
    if isYang then
        Element.color foreColor (Element.spacer 70 10)
    else
        let
            halfBar =
                Element.color foreColor (Element.spacer 25 10)
        in
            Element.flow Element.left
                [ halfBar
                , Element.spacer 20 1
                , halfBar
                ]


drawChanges : List Bool -> Element
drawChanges changes =
    let
        box =
            Element.spacer 12 12

        dot isChanging =
            if isChanging then
                Element.color Color.yellow box
            else
                box
    in
        List.map dot changes
            |> List.intersperse (Element.spacer 1 8)
            |> Element.flow Element.down


backColor : Color
backColor =
    Color.black


foreColor : Color
foreColor =
    Color.white


drawText : List Bool -> Element
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
    in
        [ toString number
            |> Text.fromString
            |> Text.style numberStyle
            |> Element.leftAligned
        , Element.spacer 1 10
        , name
            |> Text.fromString
            |> Text.style nameStyle
            |> Element.leftAligned
        ]
            |> Element.flow Element.down


numberStyle : Text.Style
numberStyle =
    { typeface = [ "sans-serif" ]
    , height = Just 20
    , color = Color.yellow
    , bold = True
    , italic = False
    , line = Nothing
    }


nameStyle : Text.Style
nameStyle =
    { typeface = [ "serif" ]
    , height = Just 18
    , color = Color.white
    , bold = False
    , italic = True
    , line = Nothing
    }


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
