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


type alias PieceOnBoard =
    { prof : Profession, pieceColor : PieceColor, coord : Coordinate }


type alias PieceWithFloatPosition =
    { prof : Profession, pieceColor : PieceColor, coord : CoordinateFloat }


type Model
    = NothingSelected StateOfCards
    | MoverIsSelected Focus StateOfCards
    | {- Sacrifice is necessary if currently stepping; otherwise not necessary -} NowWaitingForAdditionalSacrifice FloatingMover
    | WaitForTrashBinClick { mover : PieceOnBoard, remaining : StateOfCards, whoseHand : WhoseTurn, index : Int }
    | AfterSacrifice MoveCommand FloatingMover
    | AfterCircleSacrifice FloatingMover


type alias FloatingMover =
    { mover : PieceOnBoard, remaining : StateOfCards }


type MoveCommand
    = HorizVert
    | Diag


type alias StateOfCards =
    { board : List PieceOnBoard
    , capturedByKese : List Profession
    , capturedByRima : List Profession
    , keseDeck : List Profession
    , rimaDeck : List Profession
    , keseHand : List Profession
    , rimaHand : List Profession
    , whoseTurn : WhoseTurn
    }


type alias Flags =
    { keseGoesFirst : Bool, keseDice : Bool, rimaDice : Bool, shipDice : Bool, keseDeck : List Int, rimaDeck : List Int }


type Msg
    = None
    | Cancel
    | TurnEnd
    | GiveFocusTo Focus
    | SendToTrashBinPart1 { whoseHand : WhoseTurn, index : Int }
    | SendToTrashBinPart2
    | MovementToward Coordinate


type Focus
    = PieceOnTheBoard Coordinate
    | PieceInKeseHand Int
    | PieceInRimaHand Int


toColor : WhoseTurn -> PieceColor
toColor w =
    case w of
        KeseTurn ->
            Kese

        RimaTurn ->
            Rima


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
    ( update_ msg modl, Cmd.none )


update_ : Msg -> Model -> Model
update_ msg modl =
    case ( modl, msg ) of
        ( NothingSelected cardState, GiveFocusTo focus ) ->
            MoverIsSelected focus cardState

        ( MoverIsSelected _ cardState, Cancel ) ->
            NothingSelected cardState

        ( MoverIsSelected from cardState, MovementToward to ) ->
            case from of
                PieceOnTheBoard coord ->
                    case robFocusedPieceFromBoard coord cardState.board of
                        {- This branch is not taken -}
                        Nothing ->
                            modl

                        Just ( piece, robbedBoard ) ->
                            NowWaitingForAdditionalSacrifice
                                { {- Updates the position here -} mover = { piece | coord = to }
                                , remaining = { cardState | board = robbedBoard }
                                }

                {- Parachuting from KeseHand -}
                PieceInKeseHand ind ->
                    let
                        ( profs {- always a singleton -}, newKeseHand ) =
                            robIth ind cardState.keseHand

                        newBoard =
                            List.map (\prof -> { pieceColor = Kese, coord = to, prof = prof }) profs
                                ++ cardState.board
                    in
                    NothingSelected { cardState | board = newBoard, keseHand = newKeseHand, whoseTurn = RimaTurn }

                {- Parachuting from RimaHand -}
                PieceInRimaHand ind ->
                    let
                        ( profs {- always a singleton -}, newRimaHand ) =
                            robIth ind cardState.rimaHand

                        newBoard =
                            List.map (\prof -> { pieceColor = Rima, coord = to, prof = prof }) profs
                                ++ cardState.board
                    in
                    NothingSelected { cardState | board = newBoard, rimaHand = newRimaHand, whoseTurn = KeseTurn }

        ( AfterSacrifice command { mover, remaining }, MovementToward to ) ->
            NowWaitingForAdditionalSacrifice { mover = { mover | coord = to }, remaining = remaining }

        ( NowWaitingForAdditionalSacrifice { mover, remaining }, SendToTrashBinPart1 { whoseHand, index } ) ->
            WaitForTrashBinClick { mover = mover, remaining = remaining, whoseHand = whoseHand, index = index }

        ( NowWaitingForAdditionalSacrifice { mover, remaining }, TurnEnd ) ->
            NothingSelected
                { remaining
                    | whoseTurn =
                        case remaining.whoseTurn of
                            KeseTurn ->
                                RimaTurn

                            RimaTurn ->
                                KeseTurn
                    , board = mover :: remaining.board
                }

        ( WaitForTrashBinClick { mover, remaining, whoseHand, index }, SendToTrashBinPart2 ) ->
            case whoseHand of
                KeseTurn ->
                    let
                        ( sacrifices {- always a singleton -}, newKeseHand ) =
                            robIth index remaining.keseHand

                        new =
                            { mover = mover, remaining = { remaining | keseHand = newKeseHand } }
                    in
                    case sacrifices of
                        [ Circle ] ->
                            AfterCircleSacrifice new

                        [ HorizontalVertical ] ->
                            AfterSacrifice HorizVert new

                        [ Diagonal ] ->
                            AfterSacrifice Diag new

                        {- this path shall not be taken -}
                        _ ->
                            modl

                RimaTurn ->
                    let
                        ( sacrifices {- always a singleton -}, newRimaHand ) =
                            robIth index remaining.rimaHand

                        new =
                            { mover = mover, remaining = { remaining | rimaHand = newRimaHand } }
                    in
                    case sacrifices of
                        {- FIXME -}
                        [ Circle ] ->
                            AfterCircleSacrifice new

                        [ HorizontalVertical ] ->
                            AfterSacrifice HorizVert new

                        [ Diagonal ] ->
                            AfterSacrifice Diag new

                        {- this path shall not be taken -}
                        _ ->
                            modl

        ( AfterCircleSacrifice { mover, remaining }, SendToTrashBinPart1 { whoseHand, index } ) ->
            WaitForTrashBinClick { mover = mover, remaining = remaining, whoseHand = whoseHand, index = index }

        _ ->
            modl


robIth : Int -> List a -> ( List a, List a )
robIth ind list =
    let
        newList =
            List.take ind list ++ List.drop (ind + 1) list

        xs =
            case List.drop ind list of
                x :: _ ->
                    [ x ]

                {- This path is never taken -}
                [] ->
                    []
    in
    ( xs, newList )


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
    [ g [ id "board" ]
        (List.map
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
        )
    ]


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
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ circle [ cx "52", cy "52", r "16", fill "#ffff00" ] [] ]


clickableButtonOnTrashBinSvg : WhoseTurn -> Msg -> Svg Msg
clickableButtonOnTrashBinSvg whoseTurn msgToBeSent =
    g
        [ transform
            (case whoseTurn of
                KeseTurn ->
                    "translate(575 615)"

                RimaTurn ->
                    "translate(575 -95)"
            )
        , Svg.Events.onClick msgToBeSent
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ circle [ cx "0", cy "0", r "16", fill "#ffff00" ] [] ]


pieceSvg : Bool -> Msg -> PieceWithFloatPosition -> Svg Msg
pieceSvg focused msgToBeSent p =
    g
        [ transform ("translate(" ++ String.fromFloat (p.coord.x * 100.0) ++ " " ++ String.fromFloat (p.coord.y * 100.0) ++ ")")
        , Html.Attributes.style "cursor"
            (case msgToBeSent of
                None ->
                    "not-allowed"

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


pieceWaitingForAdditionalCommandSvg : PieceWithFloatPosition -> Svg Msg
pieceWaitingForAdditionalCommandSvg p =
    g
        [ transform ("translate(" ++ String.fromFloat (p.coord.x * 100.0 - 5.0) ++ " " ++ String.fromFloat (p.coord.y * 100.0 + 5.0) ++ ")")
        , Html.Attributes.style "cursor" "not-allowed"
        ]
        (rect
            [ x "12"
            , y "12"
            , width "80"
            , height "80"
            , fill (backgroundColor p.pieceColor)
            , stroke "#ffff00"
            , strokeWidth "2"
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
    [ g [ id "rimaDeck" ]
        (List.indexedMap
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
        )
    , g [ id "keseDeck" ]
        (List.indexedMap
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
        )
    , g [ id "capturedByKese" ]
        (List.indexedMap
            (\i prof -> pieceSvg False None { coord = { x = toFloat i * 0.85, y = 6.0 }, prof = prof, pieceColor = Rima })
            model.capturedByKese
        )
    , g [ id "capturedByRima" ]
        (List.indexedMap
            (\i prof -> pieceSvg False None { coord = { x = 4.0 - toFloat i * 0.85, y = -2.0 }, prof = prof, pieceColor = Kese })
            model.capturedByRima
        )
    ]


stationaryPart : Maybe WhoseTurn -> StateOfCards -> List (Svg Msg)
stationaryPart trashBinFocus cardState =
    defs []
        [ Svg.filter [ Svg.Attributes.style "color-interpolation-filters:sRGB", id "blur" ]
            [ feGaussianBlur [ stdDeviation "1.5 1.5", result "blur" ] []
            ]
        ]
        :: boardSvg
        ++ displayCapturedCardsAndTwoDecks cardState
        ++ [ playerSvg "rimaPlayer" (RimaTurn == cardState.whoseTurn) RimaTurn
           , playerSvg "kesePlayer" (KeseTurn == cardState.whoseTurn) KeseTurn
           , trashBinSvg
                { transform = "translate(530 560) scale(0.2)"
                , color =
                    case trashBinFocus of
                        Just KeseTurn ->
                            "#555"

                        _ ->
                            "#eee"
                }
           , trashBinSvg
                { transform = "translate(530 -150) scale(0.2)"
                , color =
                    case trashBinFocus of
                        Just RimaTurn ->
                            "#555"

                        _ ->
                            "#eee"
                }
           ]


trashBinSvg : { a | transform : String, color : String } -> Svg msg
trashBinSvg o =
    g [ transform o.transform ]
        {- trash bin -}
        [ Svg.path [ fill o.color, d "M 4 112 l 59 337 c 5 22 25 37 47 37 c 0 0 0 0 0 0 h 227 c 22 0 41 -16 47 -37 v 0 l 59 -337 z m 219 58 c 8 0 13 6 13 13 v 218 c 0 7 -5 13 -13 13 c -7 0 -13 -6 -13 -13 v -218 c 0 -7 6 -13 13 -13 z m -105 0 c 7 0 13 6 13 12 l 19 218 c 1 7 -4 13 -12 14 c -7 0 -13 -5 -14 -12 l -19 -217 c -1 -8 5 -14 12 -15 c 1 0 1 0 1 0 z m 210 0 c 0 0 0 0 1 0 c 7 1 13 7 12 15 l -19 217 c -1 7 -7 12 -14 12 c -8 -1 -13 -7 -12 -14 l 19 -218 c 0 -6 6 -12 13 -12 z" ] []
        , Svg.path [ fill o.color, d "m 200,0 c -7,0 -13,6 -13,13 V 30 L 13,45 A 15,15 0 0 0 0,60 v 0 29 H 446 v -29 0 a 15,15 0 0 0 -13,-15 l -173,-15 V 13 c 0,-7 -5,-13 -12,-13 z" ] []
        ]


playerSvg : String -> Bool -> WhoseTurn -> Svg msg
playerSvg id_ isOwnTurn turn =
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
            if isOwnTurn then
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
    if isOwnTurn then
        g [ id id_, transform transf ] (blur :: person)

    else
        g [ id id_, transform transf ] person


neitherOccupiedNorWater : List PieceOnBoard -> List Coordinate
neitherOccupiedNorWater board =
    all_coord
        |> List.filter (\coord -> not (List.member coord (List.map .coord board)))
        |> List.filter (isWater >> not)


addDelta : Coordinate -> ( Int, Int ) -> List Coordinate
addDelta coord ( deltaX, deltaY ) =
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
    getCandidates_ piece
        hasCircleInHand
        robbedBoard
        (case piece.prof of
            Circle ->
                [ piece.coord ]

            HorizontalVertical ->
                List.concatMap (addDelta piece.coord) [ ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ]

            Diagonal ->
                List.concatMap (addDelta piece.coord) [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ) ]

            All ->
                List.concatMap (addDelta piece.coord)
                    [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ), ( 0, 0 ) ]
        )


getCandidates_ : PieceOnBoard -> Bool -> List PieceOnBoard -> List Coordinate -> List Coordinate
getCandidates_ piece hasCircleInHand robbedBoard raw_candidates =
    let
        ship_positions =
            robbedBoard |> List.filter (\p -> p.pieceColor == Ship) |> List.map .coord
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
                List.filter (isWater >> not) raw_candidates
                    ++ List.filter (\coord -> List.member coord ship_positions) raw_candidates

            else
                {- Allowed location: (non-water AND unoccupied) OR ships -}
                List.filter (\coord -> List.member coord (neitherOccupiedNorWater robbedBoard)) raw_candidates
                    ++ List.filter (\coord -> List.member coord ship_positions) raw_candidates


getCandidatesWithCommand : MoveCommand -> Bool -> PieceOnBoard -> List PieceOnBoard -> List Coordinate
getCandidatesWithCommand moveCommand hasCircleInHand piece robbedBoard =
    getCandidates_ piece
        hasCircleInHand
        robbedBoard
        (case moveCommand of
            HorizVert ->
                List.concatMap (addDelta piece.coord) [ ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ]

            Diag ->
                List.concatMap (addDelta piece.coord) [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ) ]
        )


view : Model -> Html Msg
view modl =
    case modl of
        NothingSelected cardState ->
            Html.div [ Html.Attributes.style "padding" "0 0 0 20px" ]
                [ svg
                    [ viewBox "0 -200 900 900"
                    , width "600"
                    ]
                    (stationaryPart Nothing cardState
                        ++ List.map
                            (\{ coord, prof, pieceColor } ->
                                { coord = { x = toFloat coord.x, y = toFloat coord.y }, prof = prof, pieceColor = pieceColor }
                                    |> pieceSvg False
                                        {- You can move the piece if it is a ship or if it belongs to you. -}
                                        (if pieceColor == Ship || pieceColor == toColor cardState.whoseTurn then
                                            GiveFocusTo (PieceOnTheBoard coord)

                                         else
                                            None
                                        )
                            )
                            cardState.board
                        ++ List.indexedMap
                            (\i prof ->
                                pieceSvg False
                                    (if cardState.whoseTurn == KeseTurn then
                                        GiveFocusTo (PieceInKeseHand i)

                                     else
                                        None
                                    )
                                    (keseHandPos i prof)
                            )
                            cardState.keseHand
                        ++ List.indexedMap
                            (\i prof ->
                                pieceSvg False
                                    (if cardState.whoseTurn == RimaTurn then
                                        GiveFocusTo (PieceInRimaHand i)

                                     else
                                        None
                                    )
                                    (rimaHandPos i prof)
                            )
                            cardState.rimaHand
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
                                        hasCircleInHand =
                                            List.any ((==) Circle)
                                                (case cardState.whoseTurn of
                                                    KeseTurn ->
                                                        cardState.keseHand

                                                    RimaTurn ->
                                                        cardState.rimaHand
                                                )

                                        candidates =
                                            getCandidates hasCircleInHand focused_piece robbedBoard
                                    in
                                    List.map
                                        (\{ coord, prof, pieceColor } ->
                                            { coord = { x = toFloat coord.x, y = toFloat coord.y }, prof = prof, pieceColor = pieceColor }
                                                |> pieceSvg (coord == focus_coord) None
                                        )
                                        cardState.board
                                        ++ (candidates
                                                |> List.map (\coord -> goalCandidateSvg (MovementToward coord) coord)
                                           )
                                        ++ List.indexedMap
                                            (\i prof ->
                                                pieceSvg False None (keseHandPos i prof)
                                            )
                                            cardState.keseHand
                                        ++ List.indexedMap
                                            (\i prof ->
                                                pieceSvg False None (rimaHandPos i prof)
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
                                        |> List.map (\coord -> goalCandidateSvg (MovementToward coord) coord)
                                   )
                                ++ List.indexedMap
                                    (\i prof ->
                                        case focus of
                                            PieceInKeseHand ind ->
                                                pieceSvg (ind == i) None (keseHandPos i prof)

                                            _ ->
                                                pieceSvg False None (keseHandPos i prof)
                                    )
                                    cardState.keseHand
                                ++ List.indexedMap
                                    (\i prof ->
                                        case focus of
                                            PieceInRimaHand ind ->
                                                pieceSvg (ind == i) None (rimaHandPos i prof)

                                            _ ->
                                                pieceSvg False None (rimaHandPos i prof)
                                    )
                                    cardState.rimaHand
            in
            Html.div [ Html.Attributes.style "padding" "0 0 0 20px" ]
                [ svg
                    [ viewBox "0 -200 900 900", width "600" ]
                    (stationaryPart Nothing cardState ++ dynamicPart)
                , Html.button [ onClick Cancel ] [ text "キャンセル" ]
                ]

        NowWaitingForAdditionalSacrifice { mover, remaining } ->
            let
                isSacrificingCircleRequired =
                    case List.filter (\c -> c.coord == mover.coord) remaining.board of
                        {- If there is no stepping, circle is never required -}
                        [] ->
                            False

                        steppedOn :: _ ->
                            case ( mover.pieceColor, steppedOn.pieceColor ) of
                                ( Ship, Ship ) ->
                                    True

                                ( _, Ship ) ->
                                    False

                                ( _, _ ) ->
                                    True
            in
            Html.div [ Html.Attributes.style "padding" "0 0 0 20px" ]
                [ svg
                    [ viewBox "0 -200 900 900", width "600" ]
                    (stationaryPart Nothing remaining
                        ++ List.map
                            (\piece ->
                                { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                                    |> pieceSvg False None
                             {- You cannot click any piece on the board while waiting for additional sacrifices. -}
                            )
                            remaining.board
                        ++ List.indexedMap
                            (\i prof ->
                                pieceSvg False
                                    (if remaining.whoseTurn == KeseTurn && (isSacrificingCircleRequired == (prof == Circle)) then
                                        SendToTrashBinPart1 { whoseHand = KeseTurn, index = i }

                                     else
                                        None
                                    )
                                    (keseHandPos i prof)
                            )
                            remaining.keseHand
                        ++ List.indexedMap
                            (\i prof ->
                                pieceSvg False
                                    (if remaining.whoseTurn == RimaTurn && (isSacrificingCircleRequired == (prof == Circle)) then
                                        SendToTrashBinPart1 { whoseHand = RimaTurn, index = i }

                                     else
                                        None
                                    )
                                    (rimaHandPos i prof)
                            )
                            remaining.rimaHand
                        ++ [ pieceWaitingForAdditionalCommandSvg { coord = { x = toFloat mover.coord.x, y = toFloat mover.coord.y }, prof = mover.prof, pieceColor = mover.pieceColor } ]
                    )
                , Html.button [ onClick TurnEnd ] [ text "ターンエンド" ]
                ]

        WaitForTrashBinClick { mover, remaining, whoseHand, index } ->
            Html.div [ Html.Attributes.style "padding" "0 0 0 20px" ]
                [ svg
                    [ viewBox "0 -200 900 900", width "600" ]
                    (stationaryPart (Just whoseHand) remaining
                        ++ List.map
                            (\piece ->
                                { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                                    |> pieceSvg False None
                             {- You cannot click any piece on the board while waiting for additional sacrifices. -}
                            )
                            remaining.board
                        ++ List.indexedMap
                            (\i prof ->
                                pieceSvg (whoseHand == KeseTurn && i == index)
                                    None
                                    (keseHandPos i prof)
                            )
                            remaining.keseHand
                        ++ List.indexedMap
                            (\i prof ->
                                pieceSvg (whoseHand == RimaTurn && i == index)
                                    None
                                    (rimaHandPos i prof)
                            )
                            remaining.rimaHand
                        ++ [ clickableButtonOnTrashBinSvg whoseHand SendToTrashBinPart2
                           , pieceWaitingForAdditionalCommandSvg { coord = { x = toFloat mover.coord.x, y = toFloat mover.coord.y }, prof = mover.prof, pieceColor = mover.pieceColor }
                           ]
                    )
                ]

        AfterSacrifice command { mover, remaining } ->
            let
                hasCircleInHand =
                    List.any ((==) Circle)
                        (case remaining.whoseTurn of
                            KeseTurn ->
                                remaining.keseHand

                            RimaTurn ->
                                remaining.rimaHand
                        )

                candidates =
                    getCandidatesWithCommand command hasCircleInHand mover remaining.board

                dynamicPart =
                    List.map
                        (\piece ->
                            { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                                |> pieceSvg False None
                        )
                        remaining.board
                        ++ (candidates
                                |> List.map (\coord -> goalCandidateSvg (MovementToward coord) coord)
                           )
                        ++ List.indexedMap (\i prof -> pieceSvg False None (keseHandPos i prof)) remaining.keseHand
                        ++ List.indexedMap (\i prof -> pieceSvg False None (rimaHandPos i prof)) remaining.rimaHand
                        ++ [ pieceWaitingForAdditionalCommandSvg { coord = { x = toFloat mover.coord.x, y = toFloat mover.coord.y }, prof = mover.prof, pieceColor = mover.pieceColor } ]
            in
            Html.div [ Html.Attributes.style "padding" "0 0 0 20px" ]
                [ svg
                    [ viewBox "0 -200 900 900", width "600" ]
                    (stationaryPart Nothing remaining ++ dynamicPart)
                ]

        AfterCircleSacrifice { mover, remaining } ->
            Html.div [ Html.Attributes.style "padding" "0 0 0 20px" ]
                [ svg
                    [ viewBox "0 -200 900 900", width "600" ]
                    (stationaryPart Nothing remaining
                        ++ List.map
                            (\piece ->
                                { coord = { x = toFloat piece.coord.x, y = toFloat piece.coord.y }, prof = piece.prof, pieceColor = piece.pieceColor }
                                    |> pieceSvg False None
                             {- You cannot click any piece on the board while waiting for additional sacrifices. -}
                            )
                            remaining.board
                        ++ List.indexedMap
                            (\i prof ->
                                pieceSvg False
                                    (if remaining.whoseTurn == KeseTurn && prof /= Circle then
                                        SendToTrashBinPart1 { whoseHand = KeseTurn, index = i }

                                     else
                                        None
                                    )
                                    (keseHandPos i prof)
                            )
                            remaining.keseHand
                        ++ List.indexedMap
                            (\i prof ->
                                pieceSvg False
                                    (if remaining.whoseTurn == RimaTurn && prof /= Circle then
                                        SendToTrashBinPart1 { whoseHand = RimaTurn, index = i }

                                     else
                                        None
                                    )
                                    (rimaHandPos i prof)
                            )
                            remaining.rimaHand
                        ++ [ pieceWaitingForAdditionalCommandSvg { coord = { x = toFloat mover.coord.x, y = toFloat mover.coord.y }, prof = mover.prof, pieceColor = mover.pieceColor } ]
                    )
                ]


keseHandPos : Int -> Profession -> PieceWithFloatPosition
keseHandPos i prof =
    { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese }


rimaHandPos : Int -> Profession -> PieceWithFloatPosition
rimaHandPos i prof =
    { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima }


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
        { whoseTurn =
            if flags.keseGoesFirst then
                KeseTurn

            else
                RimaTurn
        , keseDeck = keseDeck
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
        , capturedByKese = []
        , capturedByRima = []
        }
    , Cmd.none
    )
