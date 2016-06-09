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


type Line
    = YoungYin
    | YoungYang
    | OldYin
    | OldYang


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
            YoungYin

        2 ->
            YoungYin

        3 ->
            YoungYang

        4 ->
            YoungYin

        5 ->
            YoungYang

        6 ->
            YoungYang

        7 ->
            OldYin

        8 ->
            OldYang

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
        before =
            List.map beforeChange lines
                |> List.intersperse (Element.spacer 1 10)
                |> Element.flow Element.down

        after =
            List.map afterChange lines
                |> List.intersperse (Element.spacer 1 10)
                |> Element.flow Element.down

        gap =
            Element.spacer 50 1
    in
        [ before, gap, after ]
            |> Element.flow Element.left
            |> Element.container 400 300 Element.middle
            |> Element.color backColor
            |> Element.toHtml


beforeChange : Line -> Element
beforeChange line =
    case line of
        YoungYin ->
            yinElement

        YoungYang ->
            yangElement

        OldYin ->
            yinElement

        OldYang ->
            yangElement


afterChange : Line -> Element
afterChange line =
    case line of
        YoungYin ->
            yinElement

        YoungYang ->
            yangElement

        OldYin ->
            yangElement

        OldYang ->
            yinElement


yinElement : Element
yinElement =
    let
        halfBar =
            Element.color foreColor (Element.spacer 25 10)
    in
        Element.flow Element.left
            [ halfBar
            , Element.spacer 15 10
            , halfBar
            ]


yangElement : Element
yangElement =
    Element.color foreColor (Element.spacer 65 10)


backColor : Color
backColor =
    Color.black


foreColor : Color
foreColor =
    Color.white
