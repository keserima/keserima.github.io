module KeseRimaTypes exposing (..)
import Regex

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


type alias HistoryString =
    String


type alias FloatingMover_ a =
    { mover : PieceOnBoard, remaining : StateOfCards_ a }


type MoveCommand
    = HorizVert
    | Diag


type alias StateOfCards_ a =
    -- This `a` should be filled with `Profession` for the main game (because the info is hidden but known)
    -- This `a` should be filled with `()` for the playback (because the info is lost and irrecoverable)
    { board : List PieceOnBoard
    , capturedByKese : List Profession
    , capturedByRima : List Profession
    , keseDeck : List a
    , rimaDeck : List a
    , keseHand : List Profession
    , rimaHand : List Profession
    , whoseTurn : WhoseTurn
    }


type Focus
    = PieceOnTheBoard Coordinate
    | PieceInKeseHand Int
    | PieceInRimaHand Int

type CurrentStatus_ a
    = NothingSelected (StateOfCards_ a)
    | GameTerminated
        { board : List PieceOnBoard
        , capturedByKese : List Profession
        , capturedByRima : List Profession
        , keseDeck : List a
        , rimaDeck : List a
        , keseHand : List Profession
        , rimaHand : List Profession
        , whoseVictory : PieceColor
        }
    | MoverIsSelected Focus (StateOfCards_ a)
    | {- Sacrifice is necessary if currently stepping; otherwise not necessary -} NowWaitingForAdditionalSacrifice (FloatingMover_ a)
    | WaitForTrashBinClick { mover : PieceOnBoard, remaining : (StateOfCards_ a), whoseHand : WhoseTurn, index : Int }
    | AfterSacrifice MoveCommand (FloatingMover_ a)
    | AfterCircleSacrifice (FloatingMover_ a)

type OriginalMsg
    = None
    | Cancel
    | TurnEnd {- whether it is a capture or not is determined by whether there is an overlap -}
    | GiveFocusTo Focus
    | SendToTrashBinPart1 { whoseHand : WhoseTurn, index : Int }
    | SendToTrashBinPart2
    | MovementToward Coordinate


toColor : WhoseTurn -> PieceColor
toColor w =
    case w of
        KeseTurn ->
            Kese

        RimaTurn ->
            Rima


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

twoConsecutivePasses : Regex.Regex
twoConsecutivePasses =
    {- Unforgivable dark magic -}
    Maybe.withDefault Regex.never <|
        Regex.fromString "([RK]o[1-5][1-5]-[1-5][1-5]\\.\\n){2}"


getWhoseTurn : CurrentStatus_ a -> Maybe WhoseTurn
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



isVictorious : List Profession -> Bool
isVictorious list =
    List.member All list || List.all (\p -> List.member p list) [ Diagonal, HorizontalVertical, Circle ]


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
