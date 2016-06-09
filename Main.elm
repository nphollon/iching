module Main exposing (main)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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
        [ div
            [ style
                [ ( "font-family", "monospace" )
                , ( "font-size", "50px" )
                , ( "margin", "0.5em" )
                ]
            ]
            (List.map toHtml model.hexagram)
        , button [ onClick Refresh ] [ text "Refresh" ]
        ]


toHtml : Line -> Html a
toHtml line =
    let
        lineString =
            case line of
                YoungYin ->
                    "─ ─"

                YoungYang ->
                    "╌ ╌"

                OldYin ->
                    "─ ╌"

                OldYang ->
                    "╌ ─"
    in
        div
            [ style [ ( "margin-top", "-0.3em" ) ]
            ]
            [ text lineString ]
