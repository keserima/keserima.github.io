module Main exposing (init, main, view)

import Browser
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Maybe.Extra
import Url
import Url.Parser exposing (..)
import Url.Parser.Query as Query


type alias Msg =
    ()


type alias Model =
    Maybe String


type alias Flags =
    { url : String }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( case
        toRoute
            ("https://keserima.github.io/playback/index.html"
                ++ (flags.url
                        |> String.toList
                        |> List.Extra.dropWhile ((/=) '?')
                        |> String.fromList
                   )
            )
      of
        Playback a ->
            a

        NotFound ->
            Nothing
    , Cmd.none
    )


view : Model -> Html Msg
view m =
    Html.div []
        [ Html.textarea
            [ Html.Attributes.rows 15
            , Html.Attributes.cols 80
            , Html.Attributes.readonly True
            , Html.Attributes.style "font-family" "monospace"
            ]
            [ Html.text (Maybe.withDefault "棋譜が無効です。" m) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update () model =
    ( model, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


type Route
    = Playback (Maybe String)
    | NotFound


route : Parser (Route -> a) a
route =
    map Playback (s "playback" </> s "index.html" <?> Query.string "playback")


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            Maybe.withDefault NotFound (parse route url)
