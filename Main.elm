module Main exposing (main)

import Color exposing (Color)
import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Element exposing (Element)
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
    uncurry Rand.initialSeed2 randomSeed
        |> generateHexagram
        |> flip (!) []


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

        gap =
            Element.spacer 20 1
    in
        [ drawHalfHexagram before
        , gap
        , drawChanges changes
        , gap
        , drawHalfHexagram after
        ]
            |> Element.flow Element.right
            |> Element.container 400 300 Element.middle
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
        Element.color foreColor (Element.spacer 65 10)
    else
        let
            halfBar =
                Element.color foreColor (Element.spacer 25 10)
        in
            Element.flow Element.left
                [ halfBar
                , Element.spacer 15 10
                , halfBar
                ]


drawChanges : List Bool -> Element
drawChanges changes =
    let
        box =
            Element.spacer 12 12

        dot isChanging =
            if isChanging then
                Element.color Color.red box
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
