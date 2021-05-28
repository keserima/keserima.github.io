module Main exposing (Model, Msg, init, main, view)

import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        () ->
            model


type alias Msg =
    ()


boardBackgroundColor : Coordinate -> String
boardBackgroundColor coord =
    case ( coord.x, coord.y ) of
        ( 1, 2 ) ->
            "rgb(94, 147, 184)"

        ( 2, 1 ) ->
            "rgb(94, 147, 184)"

        ( 2, 2 ) ->
            "rgb(94, 147, 184)"

        ( 2, 3 ) ->
            "rgb(94, 147, 184)"

        ( 3, 2 ) ->
            "rgb(94, 147, 184)"

        _ ->
            "#ccc"


board : Svg Msg
board =
    g
        []
        ([ 0, 1, 2, 3, 4 ]
            |> List.concatMap
                (\y_ind ->
                    [ 0, 1, 2, 3, 4 ]
                        |> List.map
                            (\x_ind ->
                                rect
                                    [ x (String.fromInt (x_ind * 100 + 2))
                                    , y (String.fromInt (y_ind * 100 + 2))
                                    , width "100"
                                    , height "100"
                                    , fill (boardBackgroundColor { y = y_ind, x = x_ind })
                                    , stroke "#000"
                                    , strokeWidth "4"
                                    ]
                                    []
                            )
                )
        )


type alias Coordinate =
    { x : Int, y : Int }


type PieceColor
    = Kese
    | Rima
    | Ship


backgroundColor pieceColor =
    case pieceColor of
        Rima ->
            "rgb(200, 190, 183)"

        Kese ->
            "rgb(72, 62, 55)"

        Ship ->
            "rgb(96, 133, 157)"


foregroundColor pieceColor =
    case pieceColor of
        Kese ->
            "rgb(200, 190, 183)"

        Rima ->
            "rgb(72, 62, 55)"

        Ship ->
            "rgb(34, 44, 47)"


type Profession
    = HorizontalVertical
    | Diagonal
    | Circle
    | All


glyph profession color =
    case profession of
        HorizontalVertical ->
            [ Svg.path [ d "M 21 52 h 62", fill "transparent", stroke color, strokeWidth "6", strokeLinecap "round" ] []
            , Svg.path [ d "M 52 21 v 62", fill "transparent", stroke color, strokeWidth "6", strokeLinecap "round" ] []
            ]

        Diagonal ->
            [ Svg.path [ d "M 24 24 l 56 56", fill "transparent", stroke color, strokeWidth "6", strokeLinecap "round" ] []
            , Svg.path [ d "M 80 24 l -56 56", fill "transparent", stroke color, strokeWidth "6", strokeLinecap "round" ] []
            ]

        Circle ->
            [ circle
                [ cx "52"
                , cy "52"
                , r "27"
                , fill "transparent"
                , stroke color
                , strokeWidth "6"
                ]
                []
            ]

        All ->
            glyph HorizontalVertical color ++ glyph Diagonal color ++ glyph Circle color


type alias Piece =
    { prof : Profession, pieceColor : PieceColor, coord : Coordinate }


pieceSvg : Piece -> Svg msg
pieceSvg p =
    g [ transform ("translate(" ++ String.fromInt (p.coord.x * 100) ++ " " ++ String.fromInt (p.coord.y * 100) ++ ")") ]
        (rect [ x "12", y "12", width "80", height "80", fill (backgroundColor p.pieceColor) ] []
            :: glyph p.prof (foregroundColor p.pieceColor)
        )


view : Model -> Html Msg
view model =
    svg
        [ viewBox "0 0 504 504"
        , width "504"
        , height "504"
        ]
        [ board
        , pieceSvg { coord = { x = 0, y = 0 }, pieceColor = Rima, prof = HorizontalVertical }
        , pieceSvg { coord = { x = 1, y = 0 }, pieceColor = Rima, prof = Circle }
        , pieceSvg { coord = { x = 2, y = 0 }, pieceColor = Rima, prof = All }
        , pieceSvg { coord = { x = 3, y = 0 }, pieceColor = Rima, prof = Circle }
        , pieceSvg { coord = { x = 4, y = 0 }, pieceColor = Rima, prof = Diagonal }
        , pieceSvg { coord = { x = 0, y = 4 }, pieceColor = Kese, prof = HorizontalVertical }
        , pieceSvg { coord = { x = 1, y = 4 }, pieceColor = Kese, prof = Circle }
        , pieceSvg { coord = { x = 2, y = 4 }, pieceColor = Kese, prof = All }
        , pieceSvg { coord = { x = 3, y = 4 }, pieceColor = Kese, prof = Circle }
        , pieceSvg { coord = { x = 4, y = 4 }, pieceColor = Kese, prof = Diagonal }
        , pieceSvg { coord = { x = 1, y = 2 }, pieceColor = Ship, prof = HorizontalVertical }
        , pieceSvg { coord = { x = 3, y = 2 }, pieceColor = Ship, prof = Diagonal }
        ]


type alias Model =
    List Piece


init : Model
init =
    [ { coord = { x = 0, y = 0 }, pieceColor = Rima, prof = HorizontalVertical }
    , { coord = { x = 1, y = 0 }, pieceColor = Rima, prof = Circle }
    , { coord = { x = 2, y = 0 }, pieceColor = Rima, prof = All }
    , { coord = { x = 3, y = 0 }, pieceColor = Rima, prof = Circle }
    , { coord = { x = 4, y = 0 }, pieceColor = Rima, prof = Diagonal }
    , { coord = { x = 0, y = 4 }, pieceColor = Kese, prof = HorizontalVertical }
    , { coord = { x = 1, y = 4 }, pieceColor = Kese, prof = Circle }
    , { coord = { x = 2, y = 4 }, pieceColor = Kese, prof = All }
    , { coord = { x = 3, y = 4 }, pieceColor = Kese, prof = Circle }
    , { coord = { x = 4, y = 4 }, pieceColor = Kese, prof = Diagonal }
    , { coord = { x = 1, y = 2 }, pieceColor = Ship, prof = HorizontalVertical }
    , { coord = { x = 3, y = 2 }, pieceColor = Ship, prof = Diagonal }
    ]
