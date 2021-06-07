module Main exposing (Model, Msg, init, main, view)

import Browser
import Html exposing (Html)
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


type WhoseTurn
    = KeseTurn
    | RimaTurn


toColor : WhoseTurn -> PieceColor
toColor w =
    case w of
        KeseTurn ->
            Kese

        RimaTurn ->
            Rima


type alias PieceOnBoard =
    { prof : Profession, pieceColor : PieceColor, coord : Coordinate }


type alias PieceWithFloatPosition =
    { prof : Profession, pieceColor : PieceColor, coord : CoordinateFloat }


type Model
    = NothingSelected StateOfCards
    | MoverIsSelected Focus StateOfCards


type alias StateOfCards =
    { board : List PieceOnBoard
    , capturedByKese : List Profession
    , capturedByRima : List Profession
    , keseDeck : List Profession
    , rimaDeck : List Profession
    , keseHand : List Profession
    , rimaHand : List Profession
    }


type alias Flags =
    { keseDice : Bool, rimaDice : Bool, shipDice : Bool, keseDeck : List Int, rimaDeck : List Int }


type Msg
    = None
    | Cancel
    | Focused Focus
    | FirstMove { to : Coordinate }


type Focus
    = PieceOnTheBoard Coordinate
    | PieceInKeseHand Int
    | PieceInRimaHand Int


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
        ( NothingSelected cardState, Focused focus ) ->
            ( MoverIsSelected focus cardState, Cmd.none )

        ( MoverIsSelected _ cardState, Cancel ) ->
            ( NothingSelected cardState, Cmd.none )

        ( MoverIsSelected from cardState, FirstMove { to } ) ->
            case from of
                {- FIXME -}
                PieceOnTheBoard coord ->
                    ( modl, Cmd.none )

                PieceInKeseHand ind ->
                    let
                        newKeseHand =
                            List.take ind cardState.keseHand ++ List.drop (ind + 1) cardState.keseHand

                        newBoard =
                            case List.drop ind cardState.keseHand of
                                profession :: _ ->
                                    { pieceColor = Kese, coord = to, prof = profession } :: cardState.board

                                {- This path is never taken -}
                                [] ->
                                    cardState.board
                    in
                    ( NothingSelected { cardState | board = newBoard, keseHand = newKeseHand }, Cmd.none )

                PieceInRimaHand ind ->
                    let
                        newRimaHand =
                            List.take ind cardState.rimaHand ++ List.drop (ind + 1) cardState.rimaHand

                        newBoard =
                            case List.drop ind cardState.rimaHand of
                                profession :: _ ->
                                    { pieceColor = Rima, coord = to, prof = profession } :: cardState.board

                                {- This path is never taken -}
                                [] ->
                                    cardState.board
                    in
                    ( NothingSelected { cardState | board = newBoard, rimaHand = newRimaHand }, Cmd.none )

        _ ->
            ( modl, Cmd.none )


isWater : Coordinate -> Bool
isWater coord =
    case ( coord.x, coord.y ) of
        ( 1, 2 ) ->
            True

        ( 2, 1 ) ->
            True

        ( 2, 2 ) ->
            True

        ( 2, 3 ) ->
            True

        ( 3, 2 ) ->
            True

        _ ->
            False


boardBackgroundColor : Coordinate -> String
boardBackgroundColor coord =
    if isWater coord then
        "rgb(94, 147, 184)"

    else
        "#ccc"


boardSvg : List (Svg Msg)
boardSvg =
    List.map
        (\coord ->
            rect
                [ x (String.fromInt (coord.x * 100 + 2))
                , y (String.fromInt (coord.y * 100 + 2))
                , width "100"
                , height "100"
                , fill (boardBackgroundColor coord)
                , stroke "#000"
                , strokeWidth "4"
                ]
                []
        )
        all_coord


all_coord : List Coordinate
all_coord =
    List.concatMap
        (\y_ind ->
            List.map
                (\x_ind ->
                    { y = y_ind, x = x_ind }
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


goalCandidateSvg : Msg -> Coordinate -> Svg Msg
goalCandidateSvg msgToBeSent coord =
    g
        [ transform ("translate(" ++ String.fromInt (coord.x * 100) ++ " " ++ String.fromInt (coord.y * 100) ++ ")")
        , Svg.Events.onClick msgToBeSent
        ]
        [ circle [ cx "52", cy "52", r "16", fill "#ffff00" ] [] ]


pieceSvg : Bool -> Msg -> PieceWithFloatPosition -> Svg Msg
pieceSvg focused msgToBeSent p =
    g
        [ transform ("translate(" ++ String.fromFloat (p.coord.x * 100.0) ++ " " ++ String.fromFloat (p.coord.y * 100.0) ++ ")")
        , Html.Attributes.style "cursor"
            (case msgToBeSent of
                None ->
                    "default"

                _ ->
                    "pointer"
            )
        , Svg.Events.onClick msgToBeSent
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


displayCapturedCardsAndTwoDecks : StateOfCards -> List (Svg Msg)
displayCapturedCardsAndTwoDecks model =
    List.indexedMap
        (\i _ ->
            rect
                [ x "535.7"
                , y (String.fromInt (-10 + 10 * i))
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
                    [ x "535.7"
                    , y (String.fromInt (410 - 10 * i))
                    , width "80"
                    , height "80"
                    , fill (backgroundColor Kese)
                    , strokeWidth "1"
                    , stroke "#eee"
                    ]
                    []
            )
            model.keseDeck
        ++ List.indexedMap
            (\i prof -> pieceSvg False None { coord = { x = toFloat i * 0.85, y = 6.0 }, prof = prof, pieceColor = Rima })
            model.capturedByKese
        ++ List.indexedMap
            (\i prof -> pieceSvg False None { coord = { x = 4.0 - toFloat i * 0.85, y = -2.0 }, prof = prof, pieceColor = Kese })
            model.capturedByRima


stationaryPart : StateOfCards -> List (Svg Msg)
stationaryPart cardState =
    defs []
        [ Svg.filter [ Svg.Attributes.style "color-interpolation-filters:sRGB", id "blur" ]
            [ feGaussianBlur [ stdDeviation "1.5 1.5", result "blur" ] []
            ]
        ]
        :: boardSvg
        ++ displayCapturedCardsAndTwoDecks cardState
        ++ [ playerSvg True RimaTurn
           , playerSvg False KeseTurn
           ]


playerSvg : Bool -> WhoseTurn -> Svg msg
playerSvg focused turn =
    let
        translateY =
            case turn of
                KeseTurn ->
                    442.0

                RimaTurn ->
                    56.75

        color =
            toColor turn

        scale =
            if focused then
                5.5

            else
                4.0

        transf =
            "translate(727," ++ String.fromFloat translateY ++ ") scale(" ++ String.fromFloat scale ++ ")"

        person =
            [ circle [ cx "0", cy "0", r "12", fill (backgroundColor color) ] []
            , circle [ cx "0", cy "-5.5", r "4", fill (foregroundColor color) ] []
            , Svg.path [ fill (foregroundColor color), d "m 0,0.5 c -3,0 -5.8,1 -8,3 v 3 h 16 v -3 c -2.2,-2 -5,-3 -8,-3 z" ] []
            ]

        blur =
            circle [ cx "0", cy "0", r "12", fill (backgroundColor color), Svg.Attributes.style "fill:#483e37;fill-opacity:1;filter:url(#blur)" ] []
    in
    if focused then
        g [ transform transf ] (blur :: person)

    else
        g [ transform transf ] person


neitherOccupiedNorWater : List PieceOnBoard -> List Coordinate
neitherOccupiedNorWater board =
    all_coord
        |> List.filter (\coord -> not (List.member coord (List.map .coord board)))
        |> List.filter (\coord -> not (isWater coord))


addDelta : ( Int, Int ) -> Coordinate -> List Coordinate
addDelta ( deltaX, deltaY ) coord =
    let
        x =
            coord.x + deltaX

        y =
            coord.y + deltaY
    in
    if 0 <= x && x <= 4 && 0 <= y && y <= 4 then
        [ { x = x, y = y } ]

    else
        []


robFocusedPieceFromBoard : Coordinate -> List PieceOnBoard -> Maybe ( PieceOnBoard, List PieceOnBoard )
robFocusedPieceFromBoard coord board =
    case List.filter (\p -> p.coord == coord) board of
        [ piece ] ->
            Just ( piece, List.filter (\p -> p.coord /= coord) board )

        {- This branch is not taken -}
        _ ->
            Nothing


getCandidates : Bool -> PieceOnBoard -> List PieceOnBoard -> List Coordinate
getCandidates hasCircleInHand piece robbedBoard =
    let
        ship_positions =
            robbedBoard |> List.filter (\p -> p.pieceColor == Ship) |> List.map (\p -> p.coord)

        raw_candidates =
            case piece.prof of
                Circle ->
                    [ piece.coord ]

                HorizontalVertical ->
                    List.concatMap (\delta -> addDelta delta piece.coord) [ ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ]

                Diagonal ->
                    List.concatMap (\delta -> addDelta delta piece.coord) [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ) ]

                All ->
                    List.concatMap (\delta -> addDelta delta piece.coord)
                        [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ), ( 0, 0 ) ]
    in
    case piece.pieceColor of
        {- If ship, cannot leave water -}
        Ship ->
            if hasCircleInHand then
                {- Allowed location: water OR ships -}
                List.filter isWater raw_candidates
                    ++ List.filter (\coord -> List.member coord ship_positions) raw_candidates

            else
                {- Allowed location: water -}
                List.filter isWater raw_candidates

        {- If not ship, then restriction on water -}
        _ ->
            if hasCircleInHand then
                {- Allowed location: non-water OR ships -}
                List.filter (\coord -> not (isWater coord)) raw_candidates
                    ++ List.filter (\coord -> List.member coord ship_positions) raw_candidates

            else
                {- Allowed location: (non-water AND unoccupied) OR ships -}
                List.filter (\coord -> List.member coord (neitherOccupiedNorWater robbedBoard)) raw_candidates
                    ++ List.filter (\coord -> List.member coord ship_positions) raw_candidates


view : Model -> Html Msg
view modl =
    case modl of
        NothingSelected model ->
            Html.div [ Html.Attributes.style "padding" "0 0 0 20px" ]
                [ svg
                    [ viewBox "0 -200 900 900"
                    , width "600"
                    ]
                    (stationaryPart model
                        ++ List.map
                            (\piece ->
                                { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                                    |> pieceSvg False (Focused (PieceOnTheBoard piece.coord))
                            )
                            model.board
                        ++ List.indexedMap
                            (\i prof -> pieceSvg False (Focused (PieceInKeseHand i)) { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese })
                            model.keseHand
                        ++ List.indexedMap
                            (\i prof -> pieceSvg False (Focused (PieceInRimaHand i)) { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima })
                            model.rimaHand
                    )
                ]

        MoverIsSelected focus cardState ->
            let
                dynamicPart =
                    case focus of
                        PieceOnTheBoard focus_coord ->
                            case robFocusedPieceFromBoard focus_coord cardState.board of
                                {- This branch is not taken -}
                                Nothing ->
                                    []

                                Just ( focused_piece, robbedBoard ) ->
                                    let
                                        candidates =
                                            getCandidates True {- Fixme -} focused_piece robbedBoard
                                    in
                                    List.map
                                        (\piece ->
                                            { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                                                |> pieceSvg (piece.coord == focus_coord) None
                                        )
                                        cardState.board
                                        ++ (candidates
                                                |> List.map (\coord -> goalCandidateSvg (FirstMove { to = coord }) coord)
                                           )
                                        ++ List.indexedMap
                                            (\i prof ->
                                                pieceSvg False None { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese }
                                            )
                                            cardState.keseHand
                                        ++ List.indexedMap
                                            (\i prof ->
                                                pieceSvg False None { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima }
                                            )
                                            cardState.rimaHand

                        _ ->
                            List.map
                                (\piece ->
                                    { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                                        |> pieceSvg False None
                                )
                                cardState.board
                                ++ (neitherOccupiedNorWater cardState.board
                                        |> List.map (\coord -> goalCandidateSvg (FirstMove { to = coord }) coord)
                                   )
                                ++ List.indexedMap
                                    (\i prof ->
                                        case focus of
                                            PieceInKeseHand ind ->
                                                pieceSvg (ind == i) None { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese }

                                            _ ->
                                                pieceSvg False None { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese }
                                    )
                                    cardState.keseHand
                                ++ List.indexedMap
                                    (\i prof ->
                                        case focus of
                                            PieceInRimaHand ind ->
                                                pieceSvg (ind == i) None { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima }

                                            _ ->
                                                pieceSvg False None { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima }
                                    )
                                    cardState.rimaHand
            in
            Html.div [ Html.Attributes.style "padding" "0 0 0 20px" ]
                [ svg
                    [ viewBox "0 -200 900 900", width "600" ]
                    (stationaryPart cardState ++ dynamicPart)
                , Html.button [ onClick Cancel ] [ text "キャンセル" ]
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
    ( NothingSelected
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
