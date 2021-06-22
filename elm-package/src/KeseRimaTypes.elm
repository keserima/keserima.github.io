module KeseRimaTypes exposing (..)


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
