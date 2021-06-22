module Main exposing (init, main, view)

import Browser
import Debug
import Html exposing (Html)
import Html.Attributes exposing (href, src)
import KeseRimaSvgElements exposing (..)
import KeseRimaTypes exposing (..)
import List.Extra exposing (filterNot)
import Regex
import String exposing (String)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import SvgColor exposing (..)
import Url.Builder exposing (..)


type alias StateOfCards =
    StateOfCards_ ()


type alias FloatingMover =
    FloatingMover_ ()


type alias Flags =
    { historyFirst : String
    , historySecond : String
    }


type alias Flags2 =
    { historyStr : String
    }


type alias CurrentStatus =
    CurrentStatus_ ()


type PlaybackMsg
    = Orig OriginalMsg
    | TemporarilyDisabled OriginalMsg
    | NoMsg
    | GoBack
    | GoForward


type Model
    = Model
        { saved : CurrentStatus -- Reverts to here when canceled
        , historyFirst : HistoryString
        , historySecond : HistoryString
        , currentStatus : CurrentStatus
        }


main : Program Flags2 Model PlaybackMsg
main =
    Browser.element
        { init = init_
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


coordFromHistoryStr : String -> Coordinate
coordFromHistoryStr q =
    let
        foo a =
            case a of
                "1" ->
                    0

                "2" ->
                    1

                "3" ->
                    2

                "4" ->
                    3

                "5" ->
                    4

                u ->
                    Debug.todo ("unexpected `" ++ u ++ "` encountered while expecting a coordinate")
    in
    { x = foo (String.left 1 q), y = foo (String.right 1 q) }


update : PlaybackMsg -> Model -> ( Model, Cmd PlaybackMsg )
update =
    updateWithPotentialInfoOnDrawnCards NoInfo


type CardDrawInfo
    = NoInfo
    | ThreeCards ( Profession, Profession, Profession )
    | NoCards


updateWithPotentialInfoOnDrawnCards : CardDrawInfo -> PlaybackMsg -> Model -> ( Model, Cmd PlaybackMsg )
updateWithPotentialInfoOnDrawnCards cardsDrawn mesg ((Model { historyFirst, historySecond, currentStatus, saved }) as mdl) =
    case mesg of
        GoForward ->
            if String.left 1 historySecond == "~" then
                updateWithPotentialInfoOnDrawnCards NoInfo (Orig Cancel) mdl

            else
                case currentStatus of
                    NothingSelected cardState ->
                        if String.left 1 historySecond == "S" then
                            -- this is clearly a movement of the ship
                            -- whose beginning is shaped like "S+22"
                            let
                                decodedMsg =
                                    -- hence this is the coordinate
                                    String.slice 2 4 historySecond
                                        |> coordFromHistoryStr
                                        |> PieceOnTheBoard
                                        |> GiveFocusTo
                            in
                            updateWithPotentialInfoOnDrawnCards NoInfo (Orig decodedMsg) mdl

                        else
                        -- otherwise, there are three possibilities for the decoded message:
                        -- PieceOnTheBoard, PieceInKeseHand or PieceInRimaHand.
                        -- If what's played is a piece in either player's hand, then
                        -- the following options are possible:
                        -- * the end of the history string follows immediately after "o", "+" or "x"
                        -- * or, the profession is followed by a coordinate and a period
                        -- * or, the profession is followed by a coordinate, "{", three/zero cards drawn, "}" and then finally a period
                        -- * or, the profession is followed by a ~~~
                        if
                            String.length historySecond == 1 || String.slice 3 4 historySecond == "." || String.slice 1 4 historySecond == "~~~"
                        then
                            let
                                profession =
                                    String.left 1 historySecond |> profFromHistoryStr

                                decodedMsg =
                                    if String.right 1 historyFirst == "K" {- FIXME -} then
                                        case List.Extra.findIndex ((==) profession) cardState.keseHand of
                                            Just i ->
                                                i
                                                    |> PieceInKeseHand
                                                    |> GiveFocusTo

                                            Nothing ->
                                                Debug.todo "cannot find an adequate piece in Kese's Hand 2"

                                    else
                                        case List.Extra.findIndex ((==) profession) cardState.rimaHand of
                                            Just i ->
                                                i
                                                    |> PieceInRimaHand
                                                    |> GiveFocusTo

                                            Nothing ->
                                                Debug.todo "cannot find an adequate piece in Rima's Hand 2"
                            in
                            updateWithPotentialInfoOnDrawnCards NoInfo (Orig decodedMsg) mdl

                        else if String.slice 3 4 historySecond == "{" then
                            let
                                cardDrawInfo =
                                    if String.slice 4 5 historySecond == "}" then
                                        NoCards

                                    else
                                        ThreeCards
                                            ( String.slice 4 5 historySecond |> profFromHistoryStr
                                            , String.slice 5 6 historySecond |> profFromHistoryStr
                                            , String.slice 6 7 historySecond |> profFromHistoryStr
                                            )

                                profession =
                                    String.left 1 historySecond |> profFromHistoryStr

                                decodedMsg =
                                    if String.right 1 historyFirst == "K" {- FIXME -} then
                                        case List.Extra.findIndex ((==) profession) cardState.keseHand of
                                            Just i ->
                                                i
                                                    |> PieceInKeseHand
                                                    |> GiveFocusTo

                                            Nothing ->
                                                Debug.todo "cannot find an adequate piece in Kese's Hand 2"

                                    else
                                        case List.Extra.findIndex ((==) profession) cardState.rimaHand of
                                            Just i ->
                                                i
                                                    |> PieceInRimaHand
                                                    |> GiveFocusTo

                                            Nothing ->
                                                Debug.todo "cannot find an adequate piece in Rima's Hand 2"
                            in
                            updateWithPotentialInfoOnDrawnCards cardDrawInfo (Orig decodedMsg) mdl

                        else
                            -- In this branch, what's played is NOT a piece in either player's hand, but rather a PieceOnTheBoard.
                            -- Also, we already covered the ship.
                            let
                                decodedMsg =
                                    -- hence this is the coordinate
                                    String.slice 1 3 historySecond
                                        |> coordFromHistoryStr
                                        |> PieceOnTheBoard
                                        |> GiveFocusTo
                            in
                            updateWithPotentialInfoOnDrawnCards NoInfo (Orig decodedMsg) mdl

                    MoverIsSelected from _ ->
                        case from of
                            PieceOnTheBoard _ ->
                                -- "-" ++ coordToHistoryStr to
                                let
                                    decodedMsg =
                                        String.slice 1 3 historySecond
                                            |> coordFromHistoryStr
                                            |> MovementToward
                                in
                                updateWithPotentialInfoOnDrawnCards NoInfo (Orig decodedMsg) mdl

                            _ ->
                                let
                                    to =
                                        String.slice 0 2 historySecond |> coordFromHistoryStr
                                in
                                if String.slice 2 3 historySecond == "." then
                                    -- no cards were drawn
                                    updateWithPotentialInfoOnDrawnCards NoInfo (Orig (MovementToward to)) mdl

                                else if String.slice 2 3 historySecond == "{" then
                                    -- cards were drawn, so supply such info
                                    let
                                        cardDrawInfo =
                                            if String.slice 3 4 historySecond == "}" then
                                                NoCards

                                            else
                                                ThreeCards
                                                    ( String.slice 3 4 historySecond |> profFromHistoryStr
                                                    , String.slice 4 5 historySecond |> profFromHistoryStr
                                                    , String.slice 5 6 historySecond |> profFromHistoryStr
                                                    )
                                    in
                                    updateWithPotentialInfoOnDrawnCards cardDrawInfo (Orig (MovementToward to)) mdl

                                else
                                    Debug.todo "Unexpected character. Expected `.` or `{`"

                    AfterSacrifice _ _ ->
                        let
                            to =
                                String.slice 0 2 historySecond |> coordFromHistoryStr
                        in
                        updateWithPotentialInfoOnDrawnCards NoInfo (Orig (MovementToward to)) mdl

                    WaitForTrashBinClick _ ->
                        updateWithPotentialInfoOnDrawnCards NoInfo (Orig SendToTrashBinPart2) mdl

                    AfterCircleSacrifice { remaining } ->
                        let
                            profession =
                                String.left 1 historySecond |> profFromHistoryStr

                            index =
                                getIndexFromProf remaining profession
                        in
                        updateWithPotentialInfoOnDrawnCards NoInfo (Orig (SendToTrashBinPart1 { whoseHand = remaining.whoseTurn, index = index })) mdl

                    NowWaitingForAdditionalSacrifice { remaining } ->
                        case String.left 1 historySecond of
                            "o" ->
                                updateWithPotentialInfoOnDrawnCards NoInfo
                                    (Orig
                                        (SendToTrashBinPart1
                                            { whoseHand = remaining.whoseTurn
                                            , index = getIndexFromProf remaining Circle
                                            }
                                        )
                                    )
                                    mdl

                            "+" ->
                                updateWithPotentialInfoOnDrawnCards NoInfo
                                    (Orig
                                        (SendToTrashBinPart1
                                            { whoseHand = remaining.whoseTurn
                                            , index = getIndexFromProf remaining HorizontalVertical
                                            }
                                        )
                                    )
                                    mdl

                            "x" ->
                                updateWithPotentialInfoOnDrawnCards NoInfo
                                    (Orig
                                        (SendToTrashBinPart1
                                            { whoseHand = remaining.whoseTurn
                                            , index = getIndexFromProf remaining Diagonal
                                            }
                                        )
                                    )
                                    mdl

                            "{" ->
                                -- cards were drawn
                                let
                                    cardDrawInfo =
                                        if String.slice 1 2 historySecond == "}" then
                                            NoCards

                                        else
                                            ThreeCards
                                                ( String.slice 1 2 historySecond |> profFromHistoryStr
                                                , String.slice 2 3 historySecond |> profFromHistoryStr
                                                , String.slice 3 4 historySecond |> profFromHistoryStr
                                                )
                                in
                                updateWithPotentialInfoOnDrawnCards cardDrawInfo (Orig TurnEnd) mdl

                            "[" ->
                                -- capture happened. cards might have been drawn.
                                if String.slice 3 4 historySecond == "{" then
                                    let
                                        cardDrawInfo =
                                            if String.slice 4 5 historySecond == "}" then
                                                NoCards

                                            else
                                                ThreeCards
                                                    ( String.slice 4 5 historySecond |> profFromHistoryStr
                                                    , String.slice 5 6 historySecond |> profFromHistoryStr
                                                    , String.slice 6 7 historySecond |> profFromHistoryStr
                                                    )
                                    in
                                    updateWithPotentialInfoOnDrawnCards cardDrawInfo (Orig TurnEnd) mdl

                                else
                                    updateWithPotentialInfoOnDrawnCards NoInfo (Orig TurnEnd) mdl

                            _ ->
                                -- cards were not drawn
                                updateWithPotentialInfoOnDrawnCards NoInfo (Orig TurnEnd) mdl

                    _ ->
                        Debug.todo "oh no!"

        Orig msg ->
            let
                newHist =
                    historyFirst ++ newHistory cardsDrawn (Orig msg) currentStatus

                newHistorySecond =
                    String.right (String.length historySecond - String.length (newHistory cardsDrawn (Orig msg) currentStatus)) historySecond

                {- FIXME -}
                newStat =
                    updateStatus cardsDrawn msg currentStatus saved
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
                            , historySecond = ""
                            , currentStatus =
                                gameEnd
                            , saved = gameEnd
                            }
                        , Cmd.none
                        )

                    _ ->
                        ( Model { historyFirst = newHist, historySecond = newHistorySecond, currentStatus = newStat, saved = saved }, Cmd.none )

            else
                case newStat of
                    NothingSelected cardState ->
                        ( Model
                            { historyFirst = newHist
                            , historySecond = newHistorySecond
                            , currentStatus = newStat
                            , saved = NothingSelected cardState -- update `saved`
                            }
                        , Cmd.none
                        )

                    _ ->
                        ( Model { historyFirst = newHist, historySecond = newHistorySecond, currentStatus = newStat, saved = saved }, Cmd.none )

        _ ->
            ( Model { historyFirst = historyFirst, historySecond = historySecond, currentStatus = currentStatus, saved = saved }, Cmd.none )


getIndexFromProf : { a | whoseTurn : WhoseTurn, keseHand : List Profession, rimaHand : List Profession } -> Profession -> Int
getIndexFromProf remaining profession =
    case remaining.whoseTurn of
        KeseTurn ->
            case List.Extra.findIndex ((==) profession) remaining.keseHand of
                Just i ->
                    i

                Nothing ->
                    Debug.todo "cannot find an adequate piece in Kese's Hand 1"

        RimaTurn ->
            case List.Extra.findIndex ((==) profession) remaining.rimaHand of
                Just i ->
                    i

                Nothing ->
                    Debug.todo "cannot find an adequate piece in Rima's Hand 1"


newHistory : CardDrawInfo -> PlaybackMsg -> CurrentStatus -> String
newHistory cardsDrawn msg modl =
    case msg of
        Orig m ->
            newHistory_ cardsDrawn m modl

        TemporarilyDisabled _ ->
            ""

        GoForward ->
            "FIXME"

        GoBack ->
            "FIXME"

        NoMsg ->
            "FIXME"


newHistory_ : CardDrawInfo -> OriginalMsg -> CurrentStatus -> String
newHistory_ cardsDrawn msg modl =
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
                        ( [ _ ], () :: () :: () :: _ ) ->
                            coordToHistoryStr to ++ "{" ++ String.join "" (List.map profToHistoryStr (unsafeDeckSummoning cardsDrawn)) ++ "}.\nR"

                        {- FIXME -}
                        _ ->
                            coordToHistoryStr to ++ ".\nR"

                {- Parachuting from RimaHand -}
                PieceInRimaHand _ ->
                    case ( cardState.rimaHand, cardState.rimaDeck ) of
                        ( [ _ ], () :: () :: () :: _ ) ->
                            coordToHistoryStr to ++ "{" ++ String.join "" (List.map profToHistoryStr (unsafeDeckSummoning cardsDrawn)) ++ "}.\nK"

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
                        "{" ++ String.join "" (List.map profToHistoryStr (unsafeDeckSummoning cardsDrawn)) ++ "}"
                        {- FIXME -}

                    else if List.isEmpty remaining.rimaHand then
                        let
                            ( _, _ ) =
                                drawUpToThree remaining.rimaDeck
                        in
                        "{" ++ String.join "" (List.map profToHistoryStr (unsafeDeckSummoning cardsDrawn)) ++ "}"
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


unsafeDeckSummoning : CardDrawInfo -> List Profession
unsafeDeckSummoning a =
    case a of
        NoInfo ->
            Debug.todo "FAILURE: expected to receive cards to be drawn, but got nothing"

        ThreeCards ( b, c, d ) ->
            [ b, c, d ]

        NoCards ->
            []


updateStatus : CardDrawInfo -> OriginalMsg -> CurrentStatus -> CurrentStatus -> CurrentStatus
updateStatus cardsDrawn msg modl saved =
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
                        ( [], () :: () :: () :: zs ) ->
                            NothingSelected
                                { cardState
                                    | board = newBoard
                                    , keseHand = unsafeDeckSummoning cardsDrawn
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
                        ( [], () :: () :: () :: zs ) ->
                            NothingSelected
                                { cardState
                                    | board = newBoard
                                    , rimaHand = unsafeDeckSummoning cardsDrawn
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
                                unsafeDeckSummoning cardsDrawn

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
                                unsafeDeckSummoning cardsDrawn
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


boardSvg : List (Svg PlaybackMsg)
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


pieceSvgOnGrid : Bool -> PlaybackMsg -> PieceOnBoard -> Svg PlaybackMsg
pieceSvgOnGrid focused msg { coord, prof, pieceColor } =
    pieceSvg focused msg { coord = { x = toFloat coord.x, y = toFloat coord.y }, prof = prof, pieceColor = pieceColor }


pieceSvg : Bool -> PlaybackMsg -> PieceWithFloatPosition -> Svg PlaybackMsg
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


msgToIcon msgToBeSent =
    case msgToBeSent of
        Orig None ->
            "not-allowed"

        TemporarilyDisabled _ ->
            "default"

        _ ->
            "pointer"


pieceSvg_ : { a | color : String, width : String } -> PlaybackMsg -> PieceWithFloatPosition -> Svg PlaybackMsg
pieceSvg_ =
    pieceSvg__ msgToIcon


drawUpToThree : List a -> ( List a, List a )
drawUpToThree xs =
    case xs of
        a :: b :: c :: ys ->
            ( [ a, b, c ], ys )

        _ ->
            ( xs, [] )


displayCapturedCardsAndTwoDecks : { a | keseDeck : List b, rimaDeck : List c, capturedByKese : List Profession, capturedByRima : List Profession } -> List (Svg PlaybackMsg)
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
                    (Orig None)
                    { coord =
                        { x =
                            -0.115
                                {- to handle the automatic offset and the 3px difference in the border -} + toFloat i
                                * (KeseRimaSvgElements.spacing <| List.length <| model.capturedByKese)
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
                    (Orig None)
                    { coord =
                        { x =
                            -0.115
                                {- to handle the automatic offset and the 3px difference in the border -} + 5.0
                                * 0.846
                                - toFloat i
                                * (KeseRimaSvgElements.spacing <| List.length <| model.capturedByRima)
                        , y = -2.0
                        }
                    , prof = prof
                    , pieceColor = Kese
                    }
            )
            model.capturedByRima
        )
    ]


stationaryPart : StateOfCards -> List (Svg PlaybackMsg)
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


twoTrashBinsSvg : Maybe WhoseTurn -> List (Svg PlaybackMsg)
twoTrashBinsSvg trashBinFocus =
    [ g [ id "keseTrashBin", transform "translate(530 560)" ] [ Svg.map TemporarilyDisabled <| trashBinSvg_ (trashBinFocus == Just KeseTurn) ]
    , g [ id "rimaTrashBin", transform "translate(530 -150)" ] [ Svg.map TemporarilyDisabled <| trashBinSvg_ (trashBinFocus == Just RimaTurn) ]
    ]


targetBlankLink : List (Attribute msg) -> List (Html msg) -> Html msg
targetBlankLink attributes =
    Html.a (Html.Attributes.target "_blank" :: attributes)


view_ : Bool -> HistoryString -> HistoryString -> List (Svg PlaybackMsg) -> List (Html PlaybackMsg) -> Html PlaybackMsg
view_ gameEndTweet historyFirst historySecond svgContent buttons =
    Html.div [ Html.Attributes.style "display" "flex" ]
        [ Html.div [ Html.Attributes.style "padding" "0px 20px 0 20px", Html.Attributes.style "min-width" "360px" ]
            [ Html.h2 [] [ Html.text "架空伝統ゲーム「ケセリマ」棋譜再生" ]
            , Html.p []
                [ targetBlankLink [ href "https://novelup.plus/story/433986940" ]
                    [ Html.img [ src "../imgs/keserima.png", Html.Attributes.height 200 ] []
                    ]
                ]
            , Html.p []
                [ targetBlankLink [ href "../play/index.html", Html.Attributes.style "font-size" "200%" ] [ Html.text "新規対局はこちら" ]
                ]
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
                , Html.div [ Html.Attributes.style "display" "flex" ]
                    [ Html.button [ Html.Attributes.style "width" "20%", Html.Attributes.disabled True ] [ Html.text "←" ]
                    , Html.div [ Html.Attributes.style "width" "60%" ] []
                    , Html.button
                        [ Html.Attributes.style "width" "20%"
                        , Html.Attributes.disabled (gameEndTweet || String.isEmpty historySecond)
                        , onClick GoForward
                        ]
                        [ Html.text "→" ]
                    ]
                ]
            ]
        ]


view : Model -> Html PlaybackMsg
view =
    view2 TemporarilyDisabled


view2 : (OriginalMsg -> PlaybackMsg) -> Model -> Html PlaybackMsg
view2 cnst (Model { historyFirst, historySecond, currentStatus }) =
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
                                (cnst
                                    (if piece.pieceColor == Ship || piece.pieceColor == toColor cardState.whoseTurn then
                                        GiveFocusTo (PieceOnTheBoard piece.coord)

                                     else
                                        None
                                    )
                                )
                                piece
                        )
                        cardState.board
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (cnst
                                    (if cardState.whoseTurn == KeseTurn then
                                        GiveFocusTo (PieceInKeseHand i)

                                     else
                                        None
                                    )
                                )
                                (keseHandPos i prof)
                        )
                        cardState.keseHand
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (cnst
                                    (if cardState.whoseTurn == RimaTurn then
                                        GiveFocusTo (PieceInRimaHand i)

                                     else
                                        None
                                    )
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
                    ++ List.map (pieceSvgOnGrid False (cnst None)) cardState.board
                    ++ List.indexedMap
                        (\i prof -> pieceSvg False (cnst None) (keseHandPos i prof))
                        cardState.keseHand
                    ++ List.indexedMap
                        (\i prof -> pieceSvg False (cnst None) (rimaHandPos i prof))
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
                                        (\piece -> pieceSvgOnGrid (piece.coord == focus_coord) (cnst None) piece)
                                        cardState.board
                                        ++ (candidatesRed |> List.map (\coord -> goalCandidateRedSvg (cnst (MovementToward coord)) coord))
                                        ++ (candidatesYellow
                                                |> List.map (\coord -> goalCandidateYellowSvg (cnst (MovementToward coord)) coord)
                                           )
                                        ++ List.indexedMap
                                            (\i prof ->
                                                pieceSvg False (cnst None) (keseHandPos i prof)
                                            )
                                            cardState.keseHand
                                        ++ List.indexedMap
                                            (\i prof ->
                                                pieceSvg False (cnst None) (rimaHandPos i prof)
                                            )
                                            cardState.rimaHand

                        _ ->
                            {- Parachuting -}
                            List.map (pieceSvgOnGrid False (cnst None)) cardState.board
                                {- You cannot capture a piece by parachuting; hence no red -}
                                ++ (neitherOccupiedNorWater cardState.board
                                        |> List.map (\coord -> goalCandidateYellowSvg (cnst (MovementToward coord)) coord)
                                   )
                                ++ List.indexedMap
                                    (\i prof ->
                                        case focus of
                                            PieceInKeseHand ind ->
                                                pieceSvg (ind == i) (cnst None) (keseHandPos i prof)

                                            _ ->
                                                pieceSvg False (cnst None) (keseHandPos i prof)
                                    )
                                    cardState.keseHand
                                ++ List.indexedMap
                                    (\i prof ->
                                        case focus of
                                            PieceInRimaHand ind ->
                                                pieceSvg (ind == i) (cnst None) (rimaHandPos i prof)

                                            _ ->
                                                pieceSvg False (cnst None) (rimaHandPos i prof)
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
                        (pieceSvgOnGrid False (cnst None) {- You cannot click any piece on the board while waiting for additional sacrifices. -})
                        remaining.board
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (cnst
                                    (if remaining.whoseTurn == KeseTurn && (isSacrificingCircleRequired == (prof == Circle)) then
                                        SendToTrashBinPart1 { whoseHand = KeseTurn, index = i }

                                     else
                                        None
                                    )
                                )
                                (keseHandPos i prof)
                        )
                        remaining.keseHand
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (cnst
                                    (if remaining.whoseTurn == RimaTurn && (isSacrificingCircleRequired == (prof == Circle)) then
                                        SendToTrashBinPart1 { whoseHand = RimaTurn, index = i }

                                     else
                                        None
                                    )
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
                        (pieceSvgOnGrid False (cnst None) {- You cannot click any piece on the board while waiting for additional sacrifices. -})
                        remaining.board
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg (whoseHand == KeseTurn && i == index)
                                (cnst None)
                                (keseHandPos i prof)
                        )
                        remaining.keseHand
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg (whoseHand == RimaTurn && i == index)
                                (cnst None)
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
                        (pieceSvgOnGrid False (cnst None))
                        remaining.board
                        ++ (candidatesRed
                                |> List.map (\coord -> goalCandidateRedSvg (cnst <| MovementToward coord) coord)
                           )
                        ++ (candidatesYellow
                                |> List.map (\coord -> goalCandidateYellowSvg (cnst <| MovementToward coord) coord)
                           )
                        ++ List.indexedMap (\i prof -> pieceSvg False (cnst None) (keseHandPos i prof)) remaining.keseHand
                        ++ List.indexedMap (\i prof -> pieceSvg False (cnst None) (rimaHandPos i prof)) remaining.rimaHand
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
                        (pieceSvgOnGrid False (cnst None) {- You cannot click any piece on the board while waiting for additional sacrifices. -})
                        remaining.board
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (cnst
                                    (if remaining.whoseTurn == KeseTurn && prof /= Circle then
                                        SendToTrashBinPart1 { whoseHand = KeseTurn, index = i }

                                     else
                                        None
                                    )
                                )
                                (keseHandPos i prof)
                        )
                        remaining.keseHand
                    ++ List.indexedMap
                        (\i prof ->
                            pieceSvg False
                                (cnst
                                    (if remaining.whoseTurn == RimaTurn && prof /= Circle then
                                        SendToTrashBinPart1 { whoseHand = RimaTurn, index = i }

                                     else
                                        None
                                    )
                                )
                                (rimaHandPos i prof)
                        )
                        remaining.rimaHand
                    ++ [ pieceWaitingForAdditionalCommandSvg mover ]
                )
                [ cancelAllButton ]


cancelAllButton : Html PlaybackMsg
cancelAllButton =
    Html.button [ onClick (TemporarilyDisabled Cancel), Html.Attributes.disabled True, Html.Attributes.style "background-color" "#ffaaaa", Html.Attributes.style "font-size" "150%" ] [ text "全てをキャンセル" ]


simpleCancelButton : Html PlaybackMsg
simpleCancelButton =
    Html.button [ onClick (TemporarilyDisabled Cancel), Html.Attributes.disabled True, Html.Attributes.style "background-color" "#ffaaaa", Html.Attributes.style "font-size" "150%" ] [ text "キャンセル" ]


captureAndTurnEndButton : Html PlaybackMsg
captureAndTurnEndButton =
    Html.button [ onClick (TemporarilyDisabled TurnEnd), Html.Attributes.disabled True, Html.Attributes.style "background-color" "#aaffaa", Html.Attributes.style "font-size" "150%" ] [ text "駒を取ってターンエンド" ]


turnEndButton : Html PlaybackMsg
turnEndButton =
    Html.button [ onClick (TemporarilyDisabled TurnEnd), Html.Attributes.disabled True, Html.Attributes.style "background-color" "#aaffaa", Html.Attributes.style "font-size" "150%" ] [ text "ターンエンド" ]


keseHandPos : Int -> Profession -> PieceWithFloatPosition
keseHandPos i prof =
    { coord = { x = toFloat i + 1.0, y = 5.0 }, prof = prof, pieceColor = Kese }


rimaHandPos : Int -> Profession -> PieceWithFloatPosition
rimaHandPos i prof =
    { coord = { x = 3.0 - toFloat i, y = -1.0 }, prof = prof, pieceColor = Rima }


profFromHistoryChar : Char -> Profession
profFromHistoryChar c =
    case c of
        '+' ->
            HorizontalVertical

        'o' ->
            Circle

        'x' ->
            Diagonal

        _ ->
            Debug.todo ("unexpected `" ++ String.fromChar c ++ "` encountered while expecting a profession")


profFromHistoryStr : String -> Profession
profFromHistoryStr c =
    case c of
        "+" ->
            HorizontalVertical

        "o" ->
            Circle

        "x" ->
            Diagonal

        _ ->
            Debug.todo ("unexpected `" ++ c ++ "` encountered while expecting a profession")


init_ : Flags2 -> ( Model, Cmd PlaybackMsg )
init_ { historyStr } =
    init
        { historyFirst = String.left 66 historyStr
        , historySecond = String.slice 66 (String.length historyStr) historyStr
        }


init : Flags -> ( Model, Cmd PlaybackMsg )
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
            List.map profFromHistoryChar (String.toList keseHandString)

        keseDeck =
            List.repeat 15 ()

        rimaHand =
            List.map profFromHistoryChar (String.toList rimaHandString)

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
                    initialBoard { keseDice = keseDice, rimaDice = rimaDice, shipDice = shipDice }
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
