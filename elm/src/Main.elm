module Main exposing (Model, Msg, init, main, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type alias Coordinate =
    { x : Int, y : Int }


type alias CoordinateFloat =
    { x : Float, y : Float }


type PieceColor
    = Kese
    | Rima
    | Ship


type Profession
    = HorizontalVertical
    | Diagonal
    | Circle
    | All


type alias PieceOnBoard =
    { prof : Profession, pieceColor : PieceColor, coord : Coordinate }


type alias PieceWithFloatPosition =
    { prof : Profession, pieceColor : PieceColor, coord : CoordinateFloat }


type Model
    = NoMoverSelected
        { board : List PieceOnBoard
        , capturedByKese : List Profession
        , capturedByRima : List Profession
        , keseDeck : List Profession
        , rimaDeck : List Profession
        , keseHand : List Profession
        , rimaHand : List Profession
        }
    | MoverIsSelected
        { board : List PieceOnBoard
        , capturedByKese : List Profession
        , capturedByRima : List Profession
        , msg : Msg_
        , keseDeck : List Profession
        , rimaDeck : List Profession
        , keseHand : List Profession
        , rimaHand : List Profession
        }


type alias Flags =
    { keseDice : Bool, rimaDice : Bool, shipDice : Bool, keseDeck : List Int, rimaDeck : List Int }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg modl =
    case ( modl, msg ) of
        ( NoMoverSelected model, Just msg_ ) ->
            ( MoverIsSelected
                { board = model.board
                , msg = msg_
                , keseHand = model.keseHand
                , keseDeck = model.keseDeck
                , rimaHand = model.rimaHand
                , rimaDeck = model.rimaDeck
                , capturedByRima = model.capturedByRima
                , capturedByKese = model.capturedByKese
                }
            , Cmd.none
            )

        _ ->
            ( modl, Cmd.none )


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


backgroundColor : PieceColor -> String
backgroundColor pieceColor =
    case pieceColor of
        Rima ->
            "rgb(200, 190, 183)"

        Kese ->
            "rgb(72, 62, 55)"

        Ship ->
            "rgb(96, 133, 157)"


foregroundColor : PieceColor -> String
foregroundColor pieceColor =
    case pieceColor of
        Kese ->
            "rgb(200, 190, 183)"

        Rima ->
            "rgb(72, 62, 55)"

        Ship ->
            "rgb(34, 44, 47)"


glyph : Profession -> String -> List (Svg msg)
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


pieceSvg : Bool -> Msg -> PieceWithFloatPosition -> Svg Msg
pieceSvg focused msg p =
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
            , stroke
                (if focused then
                    borderColor p.pieceColor

                 else
                    "none"
                )
            , strokeWidth
                (if focused then
                    "10"

                 else
                    "none"
                )
            ]
            []
            :: glyph p.prof (foregroundColor p.pieceColor)
        )


borderColor : PieceColor -> String
borderColor c =
    case c of
        Rima ->
            "#005242"

        Kese ->
            "#00b592"

        Ship ->
            "#005242"


drawUpToThree : List a -> ( List a, List a )
drawUpToThree xs =
    case xs of
        a :: b :: c :: ys ->
            ( [ a, b, c ], ys )

        _ ->
            ( xs, [] )


type alias Msg =
    Maybe Msg_


type Msg_
    = PieceOnTheBoard Coordinate
    | PieceInKeseHand Int
    | PieceInRimaHand Int


serializeMsg : Msg_ -> String
serializeMsg msg =
    case msg of
        PieceOnTheBoard coord ->
            "piece on board, location " ++ String.fromInt coord.x ++ " " ++ String.fromInt coord.y

        PieceInKeseHand i ->
            "piece in keseHand, index " ++ String.fromInt i

        PieceInRimaHand i ->
            "piece in rimaHand, index " ++ String.fromInt i


view : Model -> Html Msg
view modl =
    case modl of
        MoverIsSelected model ->
            div [ Html.Attributes.style "padding" "0 0 0 20px" ]
                [ svg
                    [ viewBox "0 -200 800 900"
                    , width "600"
                    ]
                    (board
                        ++ List.map
                            (\piece ->
                                case model.msg of
                                    PieceOnTheBoard focus_coord ->
                                        { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                                            |> pieceSvg (piece.coord == focus_coord) Nothing

                                    _ ->
                                        { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                                            |> pieceSvg False Nothing
                            )
                            model.board
                        ++ List.indexedMap
                            (\i prof -> pieceSvg False Nothing { coord = { x = toFloat i * 0.85, y = 6.0 }, prof = prof, pieceColor = Rima })
                            model.capturedByKese
                        ++ List.indexedMap
                            (\i prof ->
                                case model.msg of
                                    PieceInKeseHand ind ->
                                        pieceSvg (ind == i) Nothing { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese }

                                    _ ->
                                        pieceSvg False Nothing { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese }
                            )
                            model.keseHand
                        ++ List.indexedMap
                            (\i prof -> pieceSvg False Nothing { coord = { x = 4.0 - toFloat i * 0.85, y = -2.0 }, prof = prof, pieceColor = Kese })
                            model.capturedByRima
                        ++ List.indexedMap
                            (\i prof ->
                                case model.msg of
                                    PieceInRimaHand ind ->
                                        pieceSvg (ind == i) Nothing { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima }

                                    _ ->
                                        pieceSvg False Nothing { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima }
                            )
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
                    ("clicked: " ++ serializeMsg model.msg)
                ]

        NoMoverSelected model ->
            div [ Html.Attributes.style "padding" "0 0 0 20px" ]
                [ svg
                    [ viewBox "0 -200 800 900"
                    , width "600"
                    ]
                    (board
                        ++ List.map
                            (\piece ->
                                { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                                    |> pieceSvg False (Just (PieceOnTheBoard piece.coord))
                            )
                            model.board
                        ++ List.indexedMap
                            (\i prof -> pieceSvg False Nothing { coord = { x = toFloat i * 0.85, y = 6.0 }, prof = prof, pieceColor = Rima })
                            model.capturedByKese
                        ++ List.indexedMap
                            (\i prof -> pieceSvg False (Just (PieceInKeseHand i)) { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese })
                            model.keseHand
                        ++ List.indexedMap
                            (\i prof -> pieceSvg False Nothing { coord = { x = 4.0 - toFloat i * 0.85, y = -2.0 }, prof = prof, pieceColor = Kese })
                            model.capturedByRima
                        ++ List.indexedMap
                            (\i prof -> pieceSvg False (Just (PieceInRimaHand i)) { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima })
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
                ]


numToProf : Int -> Profession
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
    ( NoMoverSelected
        { keseDeck = keseDeck
        , rimaDeck = rimaDeck
        , keseHand = keseHand
        , rimaHand = rimaHand
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
        }
    , Cmd.none
    )
