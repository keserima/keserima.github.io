module Main exposing (Model, Msg, init, main, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    { model | msg = msg }


type alias Msg =
    Maybe String


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


board : List (Svg Msg)
board =
    List.concatMap
        (\y_ind ->
            List.map
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
                [ 0, 1, 2, 3, 4 ]
        )
        [ 0, 1, 2, 3, 4 ]


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
    let
        style =
            [ fill "transparent", stroke color, strokeWidth "6", strokeLinecap "round" ]
    in
    case profession of
        HorizontalVertical ->
            [ Svg.path (d "M 21 52 h 62" :: style) []
            , Svg.path (d "M 52 21 v 62" :: style) []
            ]

        Diagonal ->
            [ Svg.path (d "M 24 24 l  56 56" :: style) []
            , Svg.path (d "M 80 24 l -56 56" :: style) []
            ]

        Circle ->
            [ circle ([ cx "52", cy "52", r "27" ] ++ style) []
            ]

        All ->
            glyph HorizontalVertical color ++ glyph Diagonal color ++ glyph Circle color


type alias Piece =
    { prof : Profession, pieceColor : PieceColor, coord : Coordinate }


pieceSvg : Msg -> Piece -> Svg Msg
pieceSvg msg p =
    g
        [ transform ("translate(" ++ String.fromInt (p.coord.x * 100) ++ " " ++ String.fromInt (p.coord.y * 100) ++ ")")
        , Html.Attributes.style "cursor" "pointer"
        , Svg.Events.onClick msg
        ]
        (rect
            [ x "12"
            , y "12"
            , width "80"
            , height "80"
            , fill (backgroundColor p.pieceColor)
            ]
            []
            :: glyph p.prof (foregroundColor p.pieceColor)
        )


view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "padding" "0 0 0 20px" ]
        [ svg
            [ viewBox "0 -100 504 704"
            , width "378"
            ]
            (board
                ++ List.map
                    (\piece ->
                        piece
                            |> pieceSvg (Just ("piece on board, location " ++ String.fromInt piece.coord.x ++ " " ++ String.fromInt piece.coord.y))
                    )
                    model.board
                ++ List.indexedMap
                    (\i prof -> pieceSvg (Just ("piece on keseCaptured, index " ++ String.fromInt i)) { coord = { x = i, y = 5 }, prof = prof, pieceColor = Rima })
                    model.keseCaptured
                ++ List.indexedMap
                    (\i prof -> pieceSvg (Just ("piece on rimaCaptured, index " ++ String.fromInt i)) { coord = { x = 4 - i, y = -1 }, prof = prof, pieceColor = Kese })
                    model.rimaCaptured
            )
        , Html.text
            (case model.msg of
                Nothing ->
                    ""

                Just str ->
                    "clicked: " ++ str
            )
        ]


type alias Model =
    { focus : Maybe Piece
    , board : List Piece
    , keseCaptured : List Profession
    , rimaCaptured : List Profession
    , msg : Msg
    }


coin =
    Random.map (\n -> n == 0) (Random.int 0 1)


init : Model
init =
    let
        keseDice =
            False

        rimaDice =
            True

        shipDice =
            True
    in
    { focus = Nothing
    , board =
        [ { coord = { x = 0, y = 0 }
          , pieceColor = Rima
          , prof =
                if rimaDice then
                    HorizontalVertical

                else
                    Diagonal
          }
        , { coord = { x = 1, y = 0 }, pieceColor = Rima, prof = Circle }
        , { coord = { x = 2, y = 0 }, pieceColor = Rima, prof = All }
        , { coord = { x = 3, y = 0 }, pieceColor = Rima, prof = Circle }
        , { coord = { x = 4, y = 0 }
          , pieceColor = Rima
          , prof =
                if not rimaDice then
                    HorizontalVertical

                else
                    Diagonal
          }
        , { coord = { x = 0, y = 4 }
          , pieceColor = Kese
          , prof =
                if keseDice then
                    HorizontalVertical

                else
                    Diagonal
          }
        , { coord = { x = 1, y = 4 }, pieceColor = Kese, prof = Circle }
        , { coord = { x = 2, y = 4 }, pieceColor = Kese, prof = All }
        , { coord = { x = 3, y = 4 }, pieceColor = Kese, prof = Circle }
        , { coord = { x = 4, y = 4 }
          , pieceColor = Kese
          , prof =
                if not keseDice then
                    HorizontalVertical

                else
                    Diagonal
          }
        , { coord = { x = 1, y = 2 }
          , pieceColor = Ship
          , prof =
                if shipDice then
                    HorizontalVertical

                else
                    Diagonal
          }
        , { coord = { x = 3, y = 2 }
          , pieceColor = Ship
          , prof =
                if not shipDice then
                    HorizontalVertical

                else
                    Diagonal
          }
        ]
    , keseCaptured = [ Diagonal, Circle, Circle ]
    , rimaCaptured = [ HorizontalVertical, Diagonal ]
    , msg = Nothing
    }
