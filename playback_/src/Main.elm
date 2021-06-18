module Main exposing (init, main, view)

import Browser
import Debug
import Html exposing (Html)
import Html.Attributes exposing (href)
import KeseRimaTypes exposing (..)
import List.Extra exposing (filterNot)
import Regex
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import SvgColor exposing (..)
import Url.Builder exposing (..)


type alias Flags =
    { historyFirst : String
    , historySecond : String
    }


type CurrentStatus
    = NothingSelected StateOfCards
    | GameTerminated
        { board : List PieceOnBoard
        , capturedByKese : List Profession
        , capturedByRima : List Profession
        , keseDeck : List ()
        , rimaDeck : List ()
        , keseHand : List Profession
        , rimaHand : List Profession
        , whoseVictory : PieceColor
        }
    | MoverIsSelected Focus StateOfCards
    | {- Sacrifice is necessary if currently stepping; otherwise not necessary -} NowWaitingForAdditionalSacrifice FloatingMover
    | WaitForTrashBinClick { mover : PieceOnBoard, remaining : StateOfCards, whoseHand : WhoseTurn, index : Int }
    | AfterSacrifice MoveCommand FloatingMover
    | AfterCircleSacrifice FloatingMover


type Msg
    = None
    | Cancel
    | TurnEnd {- whether it is a capture or not is determined by whether there is an overlap -}
    | GiveFocusTo Focus
    | SendToTrashBinPart1 { whoseHand : WhoseTurn, index : Int }
    | SendToTrashBinPart2
    | MovementToward Coordinate


type Model
    = Model
        { saved : CurrentStatus -- Reverts to here when canceled
        , historyFirst : HistoryString
        , historySecond : HistoryString
        , currentStatus : CurrentStatus
        }


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


profToHistoryStr : Profession -> String
profToHistoryStr prof =
    case prof of
        Circle ->
            "o"

        HorizontalVertical ->
            "+"

        Diagonal ->
            "x"

        All ->
            "*"


whoseTurnToHistoryStr : WhoseTurn -> String
whoseTurnToHistoryStr w =
    case w of
        KeseTurn ->
            "K"

        RimaTurn ->
            "R"


invertWhoseTurn : WhoseTurn -> WhoseTurn
invertWhoseTurn w =
    case w of
        KeseTurn ->
            RimaTurn

        RimaTurn ->
            KeseTurn


coordToHistoryStr : Coordinate -> String
coordToHistoryStr coord =
    String.fromInt (coord.x + 1) ++ String.fromInt (coord.y + 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model { historyFirst, currentStatus, saved }) =
    let
        newHist =
            historyFirst ++ newHistory msg currentStatus

        newStat =
            updateStatus msg currentStatus saved
    in
    if Regex.contains twoConsecutivePasses newHist then
        case newStat of
            NothingSelected cardState ->
                let
                    gameEnd =
                        GameTerminated
                            { whoseVictory = Ship
                            , board = cardState.board
                            , capturedByKese = cardState.capturedByKese
                            , capturedByRima = cardState.capturedByRima
                            , keseDeck = cardState.keseDeck
                            , rimaDeck = cardState.rimaDeck
                            , keseHand = cardState.keseHand
                            , rimaHand = cardState.rimaHand
                            }
                in
                ( Model
                    { historyFirst = String.dropRight 1 newHist ++ "--------------------------------\nKeseRima"
                    , historySecond = "FIXME"
                    , currentStatus =
                        gameEnd
                    , saved = gameEnd
                    }
                , Cmd.none
                )

            _ ->
                ( Model { historyFirst = newHist, historySecond = "FIXME", currentStatus = newStat, saved = saved }, Cmd.none )

    else
        case newStat of
            NothingSelected cardState ->
                ( Model
                    { historyFirst = newHist
                    , historySecond = "FIXME"
                    , currentStatus = newStat
                    , saved = NothingSelected cardState -- update `saved`
                    }
                , Cmd.none
                )

            _ ->
                ( Model { historyFirst = newHist, historySecond = "FIXME", currentStatus = newStat, saved = saved }, Cmd.none )


twoConsecutivePasses : Regex.Regex
twoConsecutivePasses =
    {- Unforgivable dark magic -}
    Maybe.withDefault Regex.never <|
        Regex.fromString "([RK]o[1-5][1-5]-[1-5][1-5]\\.\\n){2}"


getWhoseTurn : CurrentStatus -> Maybe WhoseTurn
getWhoseTurn modl =
    case modl of
        NothingSelected { whoseTurn } ->
            Just whoseTurn

        MoverIsSelected _ { whoseTurn } ->
            Just whoseTurn

        NowWaitingForAdditionalSacrifice { remaining } ->
            Just remaining.whoseTurn

        AfterSacrifice _ { remaining } ->
            Just remaining.whoseTurn

        AfterCircleSacrifice { remaining } ->
            Just remaining.whoseTurn

        WaitForTrashBinClick { remaining } ->
            Just remaining.whoseTurn

        GameTerminated _ ->
            Nothing


newHistory : Msg -> CurrentStatus -> String
newHistory msg modl =
    let
        unwrap =
            Maybe.withDefault "ERROR!!!!!!!!!!!!!!!!!!!!"
    in
    case ( modl, msg ) of
        ( _, Cancel ) ->
            "~~~ " ++ unwrap (Maybe.map whoseTurnToHistoryStr (getWhoseTurn modl))

        ( NothingSelected cardState, GiveFocusTo (PieceInKeseHand index) ) ->
            List.Extra.getAt index cardState.keseHand |> Maybe.map profToHistoryStr |> unwrap

        ( NothingSelected cardState, GiveFocusTo (PieceInRimaHand index) ) ->
            List.Extra.getAt index cardState.rimaHand |> Maybe.map profToHistoryStr |> unwrap

        ( NothingSelected cardState, GiveFocusTo (PieceOnTheBoard coord) ) ->
            case List.filter (\p -> p.coord == coord) cardState.board of
                [ p ] ->
                    (if p.pieceColor == Ship then
                        "S"

                     else
                        ""
                    )
                        ++ profToHistoryStr p.prof
                        ++ coordToHistoryStr p.coord

                _ ->
                    "ERROR!!!!!!!!"

        ( MoverIsSelected from cardState, MovementToward to ) ->
            case from of
                PieceOnTheBoard _ ->
                    "-" ++ coordToHistoryStr to

                {- Parachuting from KeseHand -}
                PieceInKeseHand _ ->
                    case ( cardState.keseHand, cardState.keseDeck ) of
                        ( [ _ ], x :: y :: z :: _ ) ->
                            coordToHistoryStr to ++ "{" ++ String.join "" (List.map profToHistoryStr [ Circle, Circle, Circle ]) ++ "}.\nR"

                        {- FIXME -}
                        _ ->
                            coordToHistoryStr to ++ ".\nR"

                {- Parachuting from RimaHand -}
                PieceInRimaHand _ ->
                    case ( cardState.rimaHand, cardState.rimaDeck ) of
                        ( [ _ ], x :: y :: z :: _ ) ->
                            coordToHistoryStr to ++ "{" ++ String.join "" (List.map profToHistoryStr [ Circle, Circle, Circle ]) ++ "}.\nK"

                        {- FIXME -}
                        _ ->
                            coordToHistoryStr to ++ ".\nK"

        ( AfterSacrifice _ _, MovementToward to ) ->
            coordToHistoryStr to

        ( NowWaitingForAdditionalSacrifice { remaining }, SendToTrashBinPart1 { whoseHand, index } ) ->
            case whoseHand of
                KeseTurn ->
                    List.Extra.getAt index remaining.keseHand |> Maybe.map profToHistoryStr |> unwrap

                RimaTurn ->
                    List.Extra.getAt index remaining.rimaHand |> Maybe.map profToHistoryStr |> unwrap

        ( NowWaitingForAdditionalSacrifice { mover, remaining }, TurnEnd ) ->
            let
                cardDrawn =
                    if List.isEmpty remaining.keseHand then
                        let
                            ( _, _ ) =
                                drawUpToThree remaining.keseDeck
                        in
                        "{" ++ String.join "" (List.map profToHistoryStr [ Circle, Circle, Circle ]) ++ "}"
                        {- FIXME -}

                    else if List.isEmpty remaining.rimaHand then
                        let
                            ( _, _ ) =
                                drawUpToThree remaining.rimaDeck
                        in
                        "{" ++ String.join "" (List.map profToHistoryStr [ Circle, Circle, Circle ]) ++ "}"
                        {- FIXME -}

                    else
                        ""
            in
            case List.filter (\p -> p.coord == mover.coord) remaining.board of
                [] ->
                    cardDrawn ++ ".\n" ++ whoseTurnToHistoryStr (invertWhoseTurn remaining.whoseTurn)

                captured :: _ ->
                    {- capture -}
                    case remaining.whoseTurn of
                        KeseTurn ->
                            let
                                newCapturedByKese =
                                    captured.prof :: remaining.capturedByKese
                            in
                            if isVictorious newCapturedByKese then
                                "[" ++ profToHistoryStr captured.prof ++ "]" ++ cardDrawn ++ ".\n--------------------------------\nKese"

                            else
                                "[" ++ profToHistoryStr captured.prof ++ "]" ++ cardDrawn ++ ".\n" ++ whoseTurnToHistoryStr (invertWhoseTurn remaining.whoseTurn)

                        RimaTurn ->
                            let
                                newCapturedByRima =
                                    captured.prof :: remaining.capturedByRima
                            in
                            if isVictorious newCapturedByRima then
                                "[" ++ profToHistoryStr captured.prof ++ "]" ++ cardDrawn ++ ".\n--------------------------------\nRima"

                            else
                                "[" ++ profToHistoryStr captured.prof ++ "]" ++ cardDrawn ++ ".\n" ++ whoseTurnToHistoryStr (invertWhoseTurn remaining.whoseTurn)

        ( WaitForTrashBinClick _, SendToTrashBinPart2 ) ->
            ""

        ( AfterCircleSacrifice { remaining }, SendToTrashBinPart1 { whoseHand, index } ) ->
            case whoseHand of
                KeseTurn ->
                    List.Extra.getAt index remaining.keseHand |> Maybe.map profToHistoryStr |> unwrap

                RimaTurn ->
                    List.Extra.getAt index remaining.rimaHand |> Maybe.map profToHistoryStr |> unwrap

        _ ->
            {- Do nothing -}
            ""


isVictorious : List Profession -> Bool
isVictorious list =
    List.member All list || List.all (\p -> List.member p list) [ Diagonal, HorizontalVertical, Circle ]


updateStatus : Msg -> CurrentStatus -> CurrentStatus -> CurrentStatus
updateStatus msg modl saved =
    case ( modl, msg ) of
        ( _, Cancel ) ->
            -- no matter what the state is, abort it and revert to what was saved last
            saved

        ( NothingSelected cardState, GiveFocusTo focus ) ->
            MoverIsSelected focus cardState

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
                    case ( newKeseHand, cardState.keseDeck ) of
                        ( [], x :: y :: z :: zs ) ->
                            NothingSelected
                                { cardState
                                    | board = newBoard
                                    , keseHand = [ Circle, Circle, Circle ]

                                    {- FIXME -}
                                    , whoseTurn = RimaTurn
                                    , keseDeck = zs
                                }

                        _ ->
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
                    case ( newRimaHand, cardState.rimaDeck ) of
                        ( [], x :: y :: z :: zs ) ->
                            NothingSelected
                                { cardState
                                    | board = newBoard
                                    , rimaHand = [ Circle, Circle, Circle ]

                                    {- FIXME -}
                                    , whoseTurn = KeseTurn
                                    , rimaDeck = zs
                                }

                        _ ->
                            NothingSelected { cardState | board = newBoard, rimaHand = newRimaHand, whoseTurn = KeseTurn }

        ( AfterSacrifice _ { mover, remaining }, MovementToward to ) ->
            NowWaitingForAdditionalSacrifice { mover = { mover | coord = to }, remaining = remaining }

        ( NowWaitingForAdditionalSacrifice { mover, remaining }, SendToTrashBinPart1 { whoseHand, index } ) ->
            WaitForTrashBinClick { mover = mover, remaining = remaining, whoseHand = whoseHand, index = index }

        ( NowWaitingForAdditionalSacrifice { mover, remaining }, TurnEnd ) ->
            let
                cardDrawn =
                    if List.isEmpty remaining.keseHand then
                        let
                            ( _, keseDeck ) =
                                drawUpToThree remaining.keseDeck

                            keseHand =
                                [ Circle, Circle, Circle ]

                            {- FIXME -}
                        in
                        { remaining
                            | keseHand = keseHand
                            , keseDeck = keseDeck
                        }

                    else if List.isEmpty remaining.rimaHand then
                        let
                            ( _, rimaDeck ) =
                                drawUpToThree remaining.rimaDeck

                            rimaHand =
                                [ Circle, Circle, Circle ]

                            {- FIXME -}
                        in
                        { remaining
                            | rimaHand = rimaHand
                            , rimaDeck = rimaDeck
                        }

                    else
                        remaining
            in
            case List.filter (\p -> p.coord == mover.coord) remaining.board of
                [] ->
                    NothingSelected
                        { cardDrawn
                            | whoseTurn =
                                case cardDrawn.whoseTurn of
                                    KeseTurn ->
                                        RimaTurn

                                    RimaTurn ->
                                        KeseTurn
                            , board = mover :: cardDrawn.board
                        }

                captured :: _ ->
                    {- capture -}
                    let
                        newBoard =
                            mover :: List.Extra.remove captured remaining.board
                    in
                    case remaining.whoseTurn of
                        KeseTurn ->
                            let
                                newCapturedByKese =
                                    captured.prof :: remaining.capturedByKese
                            in
                            if isVictorious newCapturedByKese then
                                {- end the game without drawing cards -}
                                GameTerminated
                                    { whoseVictory = Kese
                                    , board = newBoard
                                    , capturedByKese = newCapturedByKese
                                    , capturedByRima = remaining.capturedByRima
                                    , keseDeck = remaining.keseDeck
                                    , rimaDeck = remaining.rimaDeck
                                    , keseHand = remaining.keseHand
                                    , rimaHand = remaining.rimaHand
                                    }

                            else
                                NothingSelected
                                    { cardDrawn
                                        | whoseTurn = RimaTurn
                                        , board = newBoard
                                        , capturedByKese = newCapturedByKese
                                    }

                        RimaTurn ->
                            let
                                newCapturedByRima =
                                    captured.prof :: remaining.capturedByRima
                            in
                            if isVictorious newCapturedByRima then
                                {- end the game without drawing cards -}
                                GameTerminated
                                    { whoseVictory = Rima
                                    , board = newBoard
                                    , capturedByRima = newCapturedByRima
                                    , capturedByKese = remaining.capturedByKese
                                    , keseDeck = remaining.keseDeck
                                    , rimaDeck = remaining.rimaDeck
                                    , keseHand = remaining.keseHand
                                    , rimaHand = remaining.rimaHand
                                    }

                            else
                                NothingSelected
                                    { cardDrawn
                                        | whoseTurn = KeseTurn
                                        , board = newBoard
                                        , capturedByRima = newCapturedByRima
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


filterWhetherMemberOf : List a -> List a -> List a
filterWhetherMemberOf judges =
    List.filter (\c -> List.member c judges)


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
                    , stroke boardBorderColor
                    , strokeWidth "4"
                    ]
                    []
            )
            allCoord
        )
    ]


allCoord : List Coordinate
allCoord =
    List.concatMap
        (\y_ind ->
            List.map
                (\x_ind ->
                    { y = y_ind, x = x_ind }
                )
                [ 0, 1, 2, 3, 4 ]
        )
        [ 0, 1, 2, 3, 4 ]


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


goalCandidateYellowSvg : Msg -> Coordinate -> Svg Msg
goalCandidateYellowSvg msgToBeSent coord =
    g
        [ transform ("translate(" ++ String.fromInt (coord.x * 100) ++ " " ++ String.fromInt (coord.y * 100) ++ ")")
        , Svg.Events.onClick msgToBeSent
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ circle [ cx "52", cy "52", r "16", fill yellowCandidateColor ] [] ]


goalCandidateRedSvg : Msg -> Coordinate -> Svg Msg
goalCandidateRedSvg msgToBeSent coord =
    g
        [ transform ("translate(" ++ String.fromInt (coord.x * 100) ++ " " ++ String.fromInt (coord.y * 100) ++ ")")
        , Svg.Events.onClick msgToBeSent
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ rect [ x "36", y "36", width "32", height "32", fill redCandidateColor ] [] ]


pieceSvgOnGrid : Bool -> Msg -> PieceOnBoard -> Svg Msg
pieceSvgOnGrid focused msg { coord, prof, pieceColor } =
    pieceSvg focused msg { coord = { x = toFloat coord.x, y = toFloat coord.y }, prof = prof, pieceColor = pieceColor }


pieceSvg : Bool -> Msg -> PieceWithFloatPosition -> Svg Msg
pieceSvg focused msgToBeSent p =
    let
        strok =
            if focused then
                { color = borderColor p.pieceColor
                , width = "10"
                }

            else
                { color = "none"
                , width = "none"
                }
    in
    pieceSvg_
        strok
        msgToBeSent
        p


pieceSvg_ : { a | color : String, width : String } -> Msg -> PieceWithFloatPosition -> Svg Msg
pieceSvg_ strok msgToBeSent p =
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
                strok.color
            , strokeWidth
                strok.width
            ]
            []
            :: glyph p.prof (foregroundColor p.pieceColor)
        )


pieceWaitingForAdditionalCommandSvg : PieceOnBoard -> Svg Msg
pieceWaitingForAdditionalCommandSvg p =
    g
        [ transform ("translate(" ++ String.fromFloat (toFloat p.coord.x * 100.0 - 5.0) ++ " " ++ String.fromFloat (toFloat p.coord.y * 100.0 + 5.0) ++ ")")
        , Html.Attributes.style "cursor" "not-allowed"
        ]
        (rect
            [ x "12"
            , y "12"
            , width "80"
            , height "80"
            , fill (backgroundColor p.pieceColor)
            , stroke floatingPieceBorderColor
            , strokeWidth "2"
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


displayCapturedCardsAndTwoDecks : { a | keseDeck : List b, rimaDeck : List c, capturedByKese : List Profession, capturedByRima : List Profession } -> List (Svg Msg)
displayCapturedCardsAndTwoDecks model =
    [ g [ id "keseDeck" ]
        (List.indexedMap
            (\i _ ->
                rect
                    [ x "535.7"
                    , y (String.fromInt (504 - 80 - 10 * i))
                    , width "80"
                    , height "80"
                    , fill (backgroundColor Kese)
                    , strokeWidth "1"
                    , stroke (strokeColor Kese)
                    ]
                    []
            )
            model.keseDeck
        )
    , g [ id "rimaDeck" ]
        (List.indexedMap
            (\i _ ->
                rect
                    [ x "535.7"
                    , y (String.fromInt (10 * i))
                    , width "80"
                    , height "80"
                    , fill (backgroundColor Rima)
                    , strokeWidth "1"
                    , stroke (strokeColor Rima)
                    ]
                    []
            )
            model.rimaDeck
        )
    , g [ id "capturedByKese" ]
        (List.indexedMap
            (\i prof ->
                pieceSvg_
                    { color = strokeColor Rima

                    {- what is captured by Kese turns out to be Rima -}
                    , width = "1"
                    }
                    None
                    { coord =
                        { x =
                            -0.115
                                {- to handle the automatic offset and the 3px difference in the border -} + toFloat i
                                * (spacing <| List.length <| model.capturedByKese)
                        , y = 6.0
                        }
                    , prof = prof
                    , pieceColor = Rima
                    }
            )
            model.capturedByKese
        )
    , g [ id "capturedByRima" ]
        (List.indexedMap
            (\i prof ->
                pieceSvg_
                    { color = strokeColor Kese, width = "1" }
                    None
                    { coord =
                        { x =
                            -0.115
                                {- to handle the automatic offset and the 3px difference in the border -} + 5.0
                                * 0.846
                                - toFloat i
                                * (spacing <| List.length <| model.capturedByRima)
                        , y = -2.0
                        }
                    , prof = prof
                    , pieceColor = Kese
                    }
            )
            model.capturedByRima
        )
    ]


spacing : Int -> Float
spacing n =
    {- 0.846 * 5 + 0.80 == 5.03 -}
    {- Adding to this two halves of width 1 borders gives 504px. -}
    {- This exactly matches (100 pixel * 5) + two halves of width 4 borders -}
    if n <= 6 then
        0.846

    else
        {-
           0.846 * (6-1) + 0.8 == x * (n-1) + 0.8
           x = 0.846 * 5 / (n-1)
        -}
        0.846 * 5.0 / toFloat (n - 1)


stationaryPart : StateOfCards -> List (Svg Msg)
stationaryPart cardState =
    defs []
        [ Svg.filter [ Svg.Attributes.style "color-interpolation-filters:sRGB", id "blur" ]
            [ feGaussianBlur [ stdDeviation "1.5 1.5", result "blur" ] []
            ]
        ]
        :: boardSvg
        ++ displayCapturedCardsAndTwoDecks cardState
        ++ [ playerSvg KeseTurn { victoryCrown = False, bigAndBlurred = KeseTurn == cardState.whoseTurn }
           , playerSvg RimaTurn { victoryCrown = False, bigAndBlurred = RimaTurn == cardState.whoseTurn }
           ]


twoTrashBinsSvg : Maybe WhoseTurn -> List (Svg Msg)
twoTrashBinsSvg trashBinFocus =
    [ g [ id "keseTrashBin", transform "translate(530 560)" ] [ trashBinSvg_ (trashBinFocus == Just KeseTurn) ]
    , g [ id "rimaTrashBin", transform "translate(530 -150)" ] [ trashBinSvg_ (trashBinFocus == Just RimaTurn) ]
    ]


trashBinSvg_ : Bool -> Svg Msg
trashBinSvg_ clickable =
    let
        trashBinSvg =
            {- trash bin -}
            [ Svg.path [ d "M 0.8 22.4 l 11.8 67.4 c 1 4.4 5 7.4 9.4 7.4 c 0 0 0 0 0 0 h 45.4 c 4.4 0 8.2 -3.2 9.4 -7.4 v 0 l 11.8 -67.4 z m 43.8 11.6 c 1.6 0 2.6 1.2 2.6 2.6 v 43.6 c 0 1.4 -1 2.6 -2.6 2.6 c -1.4 0 -2.6 -1.2 -2.6 -2.6 v -43.6 c 0 -1.4 1.2 -2.6 2.6 -2.6 z m -21 0 c 1.4 0 2.6 1.2 2.6 2.4 l 3.8 43.6 c 0.2 1.4 -0.8 2.6 -2.4 2.8 c -1.4 0 -2.6 -1 -2.8 -2.4 l -3.8 -43.4 c -0.2 -1.6 1 -2.8 2.4 -3 c 0.2 0 0.2 0 0.2 0 z m 42 0 c 0 0 0 0 0.2 0 c 1.4 0.2 2.6 1.4 2.4 3 l -3.8 43.4 c -0.2 1.4 -1.4 2.4 -2.8 2.4 c -1.6 -0.2 -2.6 -1.4 -2.4 -2.8 l 3.8 -43.6 c 0 -1.2 1.2 -2.4 2.6 -2.4 z" ] []
            , Svg.path [ d "m 40 0 c -1.4 0 -2.6 1.2 -2.6 2.6 V 6 L 2.6 9 A 3 3 90 0 0 0 12 v 0 v 5.8 H 89.2 v -5.8 v 0 a 3 3 90 0 0 -2.6 -3 l -34.6 -3 V 2.6 c 0 -1.4 -1 -2.6 -2.4 -2.6 z" ] []
            ]
    in
    if clickable then
        g
            [ Svg.Events.onClick SendToTrashBinPart2
            , Html.Attributes.style "cursor" "pointer"
            , fill (trashBinColor clickable)
            ]
            (trashBinSvg ++ [ circle [ cx "45", cy "55", r "16", fill yellowCandidateColor ] [] ])

    else
        g [ fill (trashBinColor clickable) ] trashBinSvg


playerSvg : WhoseTurn -> { a | victoryCrown : Bool, bigAndBlurred : Bool } -> Svg msg
playerSvg playerColor o =
    let
        id_ =
            case playerColor of
                KeseTurn ->
                    "kesePlayer"

                RimaTurn ->
                    "rimaPlayer"

        translateY =
            case playerColor of
                KeseTurn ->
                    442.0

                RimaTurn ->
                    56.75

        color =
            toColor playerColor

        scale =
            if o.bigAndBlurred then
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

        crownStyle =
            [ x "-12", y "-12", width "24", height "24", fill crownColor ]

        crown =
            [ rect crownStyle []
            , rect (transform "rotate(30) " :: crownStyle) []
            , rect (transform "rotate(-30)" :: crownStyle) []
            ]

        blur =
            circle [ cx "0", cy "0", r "12", fill (backgroundColor color), Svg.Attributes.style ("fill:" ++ blurShadowColor ++ ";fill-opacity:1;filter:url(#blur)") ] []
    in
    if o.bigAndBlurred then
        if o.victoryCrown then
            g [ id id_, transform transf ] (crown ++ blur :: person)

        else
            g [ id id_, transform transf ] (blur :: person)

    else
        g [ id id_, transform transf ] person


neitherOccupiedNorWater : List PieceOnBoard -> List Coordinate
neitherOccupiedNorWater board =
    allCoord
        |> filterNot (\coord -> List.member coord (List.map .coord board))
        |> filterNot isWater


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


getCandidatesYellow : Bool -> PieceOnBoard -> List PieceOnBoard -> List Coordinate
getCandidatesYellow hasCircleInHand piece robbedBoard =
    getCandidatesYellow_ piece
        hasCircleInHand
        robbedBoard
        (rawCandidates piece.prof piece.coord)


rawCandidates : Profession -> Coordinate -> List Coordinate
rawCandidates prof coord =
    case prof of
        Circle ->
            [ coord ]

        HorizontalVertical ->
            List.concatMap (addDelta coord) [ ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ]

        Diagonal ->
            List.concatMap (addDelta coord) [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ) ]

        All ->
            List.concatMap (addDelta coord)
                [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ), ( 0, 0 ) ]


getCandidatesYellow_ : PieceOnBoard -> Bool -> List PieceOnBoard -> List Coordinate -> List Coordinate
getCandidatesYellow_ piece hasCircleInHand robbedBoard raw_candidates =
    let
        shipPositions =
            robbedBoard |> List.filter (\p -> p.pieceColor == Ship) |> List.map .coord
    in
    case piece.pieceColor of
        {- If ship, cannot leave water -}
        Ship ->
            if hasCircleInHand then
                {- Allowed location: water OR ships -}
                List.filter isWater raw_candidates
                    ++ filterWhetherMemberOf shipPositions raw_candidates

            else
                {- Allowed location: water -}
                List.filter isWater raw_candidates

        {- If not ship, then restriction on water -}
        _ ->
            if hasCircleInHand then
                {- Allowed location: non-water OR ships -}
                filterNot isWater raw_candidates
                    ++ filterWhetherMemberOf shipPositions raw_candidates

            else
                {- Allowed location: (non-water AND unoccupied) OR ships -}
                filterWhetherMemberOf (neitherOccupiedNorWater robbedBoard) raw_candidates
                    ++ filterWhetherMemberOf shipPositions raw_candidates


getCandidatesYellowWithCommand : MoveCommand -> Bool -> PieceOnBoard -> List PieceOnBoard -> List Coordinate
getCandidatesYellowWithCommand moveCommand hasCircleInHand piece robbedBoard =
    getCandidatesYellow_ piece
        hasCircleInHand
        robbedBoard
        (case moveCommand of
            HorizVert ->
                List.concatMap (addDelta piece.coord) [ ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ]

            Diag ->
                List.concatMap (addDelta piece.coord) [ ( 1, 1 ), ( -1, -1 ), ( -1, 1 ), ( 1, -1 ) ]
        )


targetBlankLink : List (Attribute msg) -> List (Html msg) -> Html msg
targetBlankLink attributes =
    Html.a (Html.Attributes.target "_blank" :: attributes)


view_ : Bool -> HistoryString -> HistoryString -> List (Svg msg) -> List (Html msg) -> Html msg
view_ gameEndTweet historyFirst historySecond svgContent buttons =
    Html.div [ Html.Attributes.style "display" "flex" ]
        [ Html.div [ Html.Attributes.style "padding" "0px 20px 0 20px", Html.Attributes.style "min-width" "360px" ]
            [ Html.h2 [] [ Html.text "架空伝統ゲーム「ケセリマ」棋譜再生" ]
            , Html.p [ Html.Attributes.style "font-size" "80%" ]
                [ targetBlankLink
                    [ href "https://github.com/keserima/keserima.github.io/issues/new" ]
                    [ Html.text "バグなどありましたらここをクリックしてご報告ください" ]
                ]
            ]
        , Html.div []
            (svg [ viewBox "0 -200 900 900", width "540" ] svgContent
                :: Html.br [] []
                :: List.intersperse (Html.text " ") buttons
            )
        , Html.div []
            [ Html.div
                [ Html.Attributes.style "white-space" "pre-wrap"
                , Html.Attributes.style "font-family" "monospace"
                ]
                [ Html.strong
                    [ Html.Attributes.style "background-color" "#aaeeaa" ]
                    [ Html.text historyFirst ]
                , Html.text historySecond
                ]
            ]
        ]


allCoordsOccupiedBy : PieceColor -> List PieceOnBoard -> List Coordinate
allCoordsOccupiedBy color board =
    board |> List.filter (\p -> p.pieceColor == color) |> List.map .coord


view : Model -> Html Msg
view (Model { historyFirst, historySecond, currentStatus }) =
    case currentStatus of
        NothingSelected cardState ->
            view_ False
                historyFirst
                historySecond
                (stationaryPart cardState
                    ++ twoTrashBinsSvg Nothing
                    ++ List.map
                        (\piece ->
                            pieceSvgOnGrid False
                                {- You can move the piece if it is a ship or if it belongs to you. -}
                                (if piece.pieceColor == Ship || piece.pieceColor == toColor cardState.whoseTurn then
                                    GiveFocusTo (PieceOnTheBoard piece.coord)

                                 else
                                    None
                                )
                                piece
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
                [{- default state. no need to cancel everything: the state has been saved -}]

        GameTerminated cardState ->
            view_ True
                historyFirst
                historySecond
                (defs []
                    [ Svg.filter [ Svg.Attributes.style "color-interpolation-filters:sRGB", id "blur" ]
                        [ feGaussianBlur [ stdDeviation "1.5 1.5", result "blur" ] []
                        ]
                    ]
                    :: boardSvg
                    ++ displayCapturedCardsAndTwoDecks cardState
                    ++ [ {- /= is used so that, in case of a draw, both sides receive the crown -}
                         playerSvg KeseTurn
                            { victoryCrown = Rima /= cardState.whoseVictory
                            , bigAndBlurred = Rima /= cardState.whoseVictory
                            }
                       , playerSvg RimaTurn
                            { victoryCrown = Kese /= cardState.whoseVictory
                            , bigAndBlurred = Kese /= cardState.whoseVictory
                            }
                       ]
                    ++ twoTrashBinsSvg Nothing
                    ++ List.map (pieceSvgOnGrid False None) cardState.board
                    ++ List.indexedMap
                        (\i prof -> pieceSvg False None (keseHandPos i prof))
                        cardState.keseHand
                    ++ List.indexedMap
                        (\i prof -> pieceSvg False None (rimaHandPos i prof))
                        cardState.rimaHand
                )
                [{- The game has ended. No cancelling allowed. -}]

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

                                        candidatesYellow =
                                            getCandidatesYellow hasCircleInHand focused_piece robbedBoard

                                        candidatesRed =
                                            case focused_piece.pieceColor of
                                                Ship ->
                                                    []

                                                Kese ->
                                                    getCandidatesYellow True focused_piece robbedBoard
                                                        |> filterWhetherMemberOf (allCoordsOccupiedBy Rima robbedBoard)

                                                Rima ->
                                                    getCandidatesYellow True focused_piece robbedBoard
                                                        |> filterWhetherMemberOf (allCoordsOccupiedBy Kese robbedBoard)
                                    in
                                    List.map
                                        (\piece -> pieceSvgOnGrid (piece.coord == focus_coord) None piece)
                                        cardState.board
                                        ++ (candidatesRed |> List.map (\coord -> goalCandidateRedSvg (MovementToward coord) coord))
                                        ++ (candidatesYellow
                                                |> List.map (\coord -> goalCandidateYellowSvg (MovementToward coord) coord)
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
                            {- Parachuting -}
                            List.map (pieceSvgOnGrid False None) cardState.board
                                {- You cannot capture a piece by parachuting; hence no red -}
                                ++ (neitherOccupiedNorWater cardState.board
                                        |> List.map (\coord -> goalCandidateYellowSvg (MovementToward coord) coord)
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
            view_ False
                historyFirst
                historySecond
                (stationaryPart cardState ++ twoTrashBinsSvg Nothing ++ dynamicPart)
                [ simpleCancelButton ]

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
            view_ False
                historyFirst
                historySecond
                (stationaryPart remaining
                    ++ twoTrashBinsSvg Nothing
                    ++ List.map
                        (pieceSvgOnGrid False None {- You cannot click any piece on the board while waiting for additional sacrifices. -})
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
                    ++ [ pieceWaitingForAdditionalCommandSvg mover ]
                )
                (case List.filter (\p -> p.coord == mover.coord) remaining.board of
                    [ p ] ->
                        case p.pieceColor of
                            Ship ->
                                {- cannot end the turn if stepping on a Ship -}
                                [ cancelAllButton ]

                            Kese ->
                                if mover.pieceColor == Rima then
                                    [ captureAndTurnEndButton
                                    , cancelAllButton
                                    ]

                                else
                                    [ cancelAllButton ]

                            Rima ->
                                if mover.pieceColor == Kese then
                                    [ captureAndTurnEndButton
                                    , cancelAllButton
                                    ]

                                else
                                    [ cancelAllButton ]

                    _ ->
                        {- The resulting square is empty, so it is always possible to declare TurnEnd -}
                        [ turnEndButton, cancelAllButton ]
                )

        WaitForTrashBinClick { mover, remaining, whoseHand, index } ->
            view_ False
                historyFirst
                historySecond
                (stationaryPart remaining
                    ++ twoTrashBinsSvg (Just whoseHand)
                    ++ List.map
                        (pieceSvgOnGrid False None {- You cannot click any piece on the board while waiting for additional sacrifices. -})
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
                    ++ [ pieceWaitingForAdditionalCommandSvg mover ]
                )
                [ cancelAllButton ]

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

                candidatesYellow =
                    getCandidatesYellowWithCommand command hasCircleInHand mover remaining.board

                candidatesRed =
                    case mover.pieceColor of
                        Ship ->
                            []

                        Kese ->
                            getCandidatesYellowWithCommand command True mover remaining.board
                                |> filterWhetherMemberOf (allCoordsOccupiedBy Rima remaining.board)

                        Rima ->
                            getCandidatesYellowWithCommand command True mover remaining.board
                                |> filterWhetherMemberOf (allCoordsOccupiedBy Kese remaining.board)

                dynamicPart =
                    List.map
                        (pieceSvgOnGrid False None)
                        remaining.board
                        ++ (candidatesRed
                                |> List.map (\coord -> goalCandidateRedSvg (MovementToward coord) coord)
                           )
                        ++ (candidatesYellow
                                |> List.map (\coord -> goalCandidateYellowSvg (MovementToward coord) coord)
                           )
                        ++ List.indexedMap (\i prof -> pieceSvg False None (keseHandPos i prof)) remaining.keseHand
                        ++ List.indexedMap (\i prof -> pieceSvg False None (rimaHandPos i prof)) remaining.rimaHand
                        ++ [ pieceWaitingForAdditionalCommandSvg mover ]
            in
            view_ False historyFirst historySecond (stationaryPart remaining ++ twoTrashBinsSvg Nothing ++ dynamicPart) [ cancelAllButton ]

        AfterCircleSacrifice { mover, remaining } ->
            view_ False
                historyFirst
                historySecond
                (stationaryPart remaining
                    ++ twoTrashBinsSvg Nothing
                    ++ List.map
                        (pieceSvgOnGrid False None {- You cannot click any piece on the board while waiting for additional sacrifices. -})
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
                    ++ [ pieceWaitingForAdditionalCommandSvg mover ]
                )
                [ cancelAllButton ]


cancelAllButton : Html Msg
cancelAllButton =
    Html.button [ onClick Cancel, Html.Attributes.style "background-color" "#ffaaaa", Html.Attributes.style "font-size" "150%" ] [ text "全てをキャンセル" ]


simpleCancelButton : Html Msg
simpleCancelButton =
    Html.button [ onClick Cancel, Html.Attributes.style "background-color" "#ffaaaa", Html.Attributes.style "font-size" "150%" ] [ text "キャンセル" ]


captureAndTurnEndButton : Html Msg
captureAndTurnEndButton =
    Html.button [ onClick TurnEnd, Html.Attributes.style "background-color" "#aaffaa", Html.Attributes.style "font-size" "150%" ] [ text "駒を取ってターンエンド" ]


turnEndButton : Html Msg
turnEndButton =
    Html.button [ onClick TurnEnd, Html.Attributes.style "background-color" "#aaffaa", Html.Attributes.style "font-size" "150%" ] [ text "ターンエンド" ]


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


profFromHistoryStr c =
    case c of
        '+' ->
            HorizontalVertical

        'o' ->
            Circle

        _ ->
            Diagonal


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        -- format:
        -- "R+@11 S+@23 K+@15\nK{oxo} R{oo+}\n--------------------------------\nK"
        rimaDice =
            String.slice 0 2 flags.historyFirst == "R+"

        shipDice =
            String.slice 6 8 flags.historyFirst == "S+"

        keseDice =
            String.slice 12 14 flags.historyFirst == "K+"

        keseGoesFirst =
            String.right 1 flags.historyFirst == "K"

        keseHandString =
            String.slice 20 23 flags.historyFirst

        rimaHandString =
            String.slice 27 30 flags.historyFirst

        keseHand =
            List.map profFromHistoryStr (String.toList keseHandString)

        keseDeck =
            List.repeat 15 ()

        rimaHand =
            List.map profFromHistoryStr (String.toList rimaHandString)

        rimaDeck =
            List.repeat 15 ()

        initialStatus =
            NothingSelected
                { whoseTurn =
                    if keseGoesFirst then
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
                , capturedByKese = []
                , capturedByRima = []
                }
    in
    ( Model
        { historyFirst = flags.historyFirst
        , historySecond = flags.historySecond
        , currentStatus = initialStatus
        , saved = initialStatus
        }
    , Cmd.none
    )


assertEq a b =
    \c ->
        if a == b then
            c

        else
            Debug.todo ("assertion failed: `" ++ Debug.toString a ++ "` and `" ++ Debug.toString b ++ "` are not equal")
