module Main exposing (Model, Msg, init, main, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import List exposing (length)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( { model | msg = msg }, Cmd.none )


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


type alias CoordinateFloat =
    { x : Float, y : Float }


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


type alias PieceOnBoard =
    { prof : Profession, pieceColor : PieceColor, coord : Coordinate }


type alias PieceWithFloatPosition =
    { prof : Profession, pieceColor : PieceColor, coord : CoordinateFloat }


pieceSvg : Msg -> PieceWithFloatPosition -> Svg Msg
pieceSvg msg p =
    g
        [ transform ("translate(" ++ String.fromFloat (p.coord.x * 100.0) ++ " " ++ String.fromFloat (p.coord.y * 100.0) ++ ")")
        , Html.Attributes.style "cursor"
            (case msg of
                Nothing ->
                    "default"

                Just _ ->
                    "pointer"
            )
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


drawUpToThree : List a -> ( List a, List a )
drawUpToThree xs =
    case xs of
        a :: b :: c :: ys ->
            ( [ a, b, c ], ys )

        _ ->
            ( xs, [] )


type alias Msg =
    Maybe String


type ClickPosition
    = PieceOnTheBoard Coordinate


view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "padding" "0 0 0 20px" ]
        [ svg
            [ viewBox "0 -200 800 900"
            , width "600"
            ]
            (board
                ++ List.map
                    (\piece ->
                        { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                            |> pieceSvg (Just ("piece on board, location " ++ String.fromInt piece.coord.x ++ " " ++ String.fromInt piece.coord.y))
                    )
                    model.board
                ++ List.indexedMap
                    (\i prof -> pieceSvg Nothing { coord = { x = toFloat i * 0.85, y = 6.0 }, prof = prof, pieceColor = Rima })
                    model.capturedByKese
                ++ List.indexedMap
                    (\i prof -> pieceSvg (Just ("piece in keseHand, index " ++ String.fromInt i)) { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese })
                    model.keseHand
                ++ List.indexedMap
                    (\i prof -> pieceSvg Nothing { coord = { x = 4.0 - toFloat i * 0.85, y = -2.0 }, prof = prof, pieceColor = Kese })
                    model.capturedByRima
                ++ List.indexedMap
                    (\i prof -> pieceSvg (Just ("piece in rimaHand, index " ++ String.fromInt i)) { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima })
                    model.rimaHand
                ++ List.indexedMap
                    (\i _ ->
                        rect
                            [ x (String.fromInt (532 + 10 * i))
                            , y (String.fromInt (12 + 3 * i))
                            , width "80"
                            , height "80"
                            , fill (backgroundColor Rima)
                            , strokeWidth "1"
                            , stroke "#000"
                            ]
                            []
                    )
                    model.rimaDeck
                ++ List.indexedMap
                    (\i _ ->
                        rect
                            [ x (String.fromInt (532 + 10 * i))
                            , y (String.fromInt (412 - 3 * i))
                            , width "80"
                            , height "80"
                            , fill (backgroundColor Kese)
                            , strokeWidth "1"
                            , stroke "#eee"
                            ]
                            []
                    )
                    model.keseDeck
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
    { focus : Maybe PieceOnBoard
    , board : List PieceOnBoard
    , capturedByKese : List Profession
    , capturedByRima : List Profession
    , msg : Msg
    , keseDeck : List Profession
    , rimaDeck : List Profession
    , keseHand : List Profession
    , rimaHand : List Profession
    }


type alias Flags =
    { keseDice : Bool, rimaDice : Bool, shipDice : Bool, keseDeck : List Int, rimaDeck : List Int }


numToProf n =
    case n of
        0 ->
            Circle

        1 ->
            HorizontalVertical

        _ ->
            Diagonal


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( keseHand, keseDeck ) =
            drawUpToThree (List.map numToProf flags.keseDeck)

        ( rimaHand, rimaDeck ) =
            drawUpToThree (List.map numToProf flags.rimaDeck)
    in
    ( { keseDeck = keseDeck
      , rimaDeck = rimaDeck
      , keseHand = keseHand
      , rimaHand = rimaHand
      , focus = Nothing
      , board =
            [ { coord = { x = 0, y = 0 }
              , pieceColor = Rima
              , prof =
                    if flags.rimaDice then
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
                    if not flags.rimaDice then
                        HorizontalVertical

                    else
                        Diagonal
              }
            , { coord = { x = 0, y = 4 }
              , pieceColor = Kese
              , prof =
                    if flags.keseDice then
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
                    if not flags.keseDice then
                        HorizontalVertical

                    else
                        Diagonal
              }
            , { coord = { x = 1, y = 2 }
              , pieceColor = Ship
              , prof =
                    if flags.shipDice then
                        HorizontalVertical

                    else
                        Diagonal
              }
            , { coord = { x = 3, y = 2 }
              , pieceColor = Ship
              , prof =
                    if not flags.shipDice then
                        HorizontalVertical

                    else
                        Diagonal
              }
            ]
      , capturedByKese = [ Diagonal, Circle, Circle ]
      , capturedByRima = [ HorizontalVertical, Diagonal ]
      , msg = Nothing
      }
    , Cmd.none
    )
