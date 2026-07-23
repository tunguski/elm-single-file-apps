module Notes exposing (main)

{-| **Notes** — a two-pane note keeper. Titles and bodies on the left, a plain-text editor on the
right. All notes live in the file itself; Ctrl+S writes them back. See [`App`](App).
-}

import App exposing (Env)
import Html exposing (Html, button, div, header, input, li, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E


main : Program () (App.State Model) (App.Event Msg)
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , encode = encode
        , decoder = decoder
        }



-- MODEL ----------------------------------------------------------------------


type alias Note =
    { id : Int
    , title : String
    , body : String
    }


type alias Model =
    { notes : List Note
    , selected : Maybe Int
    , nextId : Int
    , query : String
    }


init : Env -> Model
init _ =
    { notes = []
    , selected = Nothing
    , nextId = 1
    , query = ""
    }



-- UPDATE ---------------------------------------------------------------------


type Msg
    = New
    | Select Int
    | Delete Int
    | SetTitle String
    | SetBody String
    | SetQuery String


update : Msg -> Model -> Model
update msg model =
    case msg of
        New ->
            let
                note =
                    { id = model.nextId, title = "Untitled", body = "" }
            in
            { model
                | notes = note :: model.notes
                , selected = Just note.id
                , nextId = model.nextId + 1
            }

        Select id ->
            { model | selected = Just id }

        Delete id ->
            { model
                | notes = List.filter (\n -> n.id /= id) model.notes
                , selected =
                    if model.selected == Just id then
                        Nothing

                    else
                        model.selected
            }

        SetTitle t ->
            { model | notes = mapSelected model (\n -> { n | title = t }) }

        SetBody b ->
            { model | notes = mapSelected model (\n -> { n | body = b }) }

        SetQuery q ->
            { model | query = q }


mapSelected : Model -> (Note -> Note) -> List Note
mapSelected model f =
    case model.selected of
        Nothing ->
            model.notes

        Just id ->
            List.map
                (\n ->
                    if n.id == id then
                        f n

                    else
                        n
                )
                model.notes



-- VIEW -----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        visible =
            List.filter (matches model.query) model.notes
    in
    div [ class "app app--split" ]
        [ div [ class "pane pane--list" ]
            [ header [ class "pane__head" ]
                [ input
                    [ class "search"
                    , placeholder "Search notes…"
                    , value model.query
                    , onInput SetQuery
                    ]
                    []
                , button [ class "btn btn--primary", onClick New ] [ text "+ New" ]
                ]
            , ul [ class "list" ] (List.map (row model.selected) visible)
            ]
        , div [ class "pane pane--main" ] [ editor model ]
        ]


matches : String -> Note -> Bool
matches q note =
    let
        needle =
            String.toLower (String.trim q)
    in
    needle
        == ""
        || String.contains needle (String.toLower note.title)
        || String.contains needle (String.toLower note.body)


row : Maybe Int -> Note -> Html Msg
row selected note =
    li
        [ class "list__item"
        , classList [ ( "is-active", selected == Just note.id ) ]
        , onClick (Select note.id)
        ]
        [ span [ class "list__title" ]
            [ text (nonEmpty note.title "Untitled") ]
        , span [ class "list__sub" ] [ text (preview note.body) ]
        ]


editor : Model -> Html Msg
editor model =
    case selectedNote model of
        Nothing ->
            div [ class "empty" ]
                [ text "Select a note, or create one with "
                , span [ class "kbd" ] [ text "+ New" ]
                , text "."
                ]

        Just note ->
            div [ class "editor" ]
                [ input
                    [ class "editor__title"
                    , value note.title
                    , placeholder "Title"
                    , onInput SetTitle
                    ]
                    []
                , textarea
                    [ class "editor__body"
                    , value note.body
                    , placeholder "Start writing…"
                    , onInput SetBody
                    ]
                    []
                , div [ class "editor__foot" ]
                    [ button
                        [ class "btn btn--danger", onClick (Delete note.id) ]
                        [ text "Delete" ]
                    ]
                ]


selectedNote : Model -> Maybe Note
selectedNote model =
    case model.selected of
        Nothing ->
            Nothing

        Just id ->
            List.head (List.filter (\n -> n.id == id) model.notes)


preview : String -> String
preview body =
    let
        flat =
            String.join " " (String.words body)
    in
    if String.length flat > 60 then
        String.left 60 flat ++ "…"

    else
        nonEmpty flat "No text yet"


nonEmpty : String -> String -> String
nonEmpty s fallback =
    if String.trim s == "" then
        fallback

    else
        s



-- CODEC ----------------------------------------------------------------------


encode : Model -> E.Value
encode model =
    E.object
        [ ( "notes", E.list encodeNote model.notes )
        , ( "nextId", E.int model.nextId )
        ]


encodeNote : Note -> E.Value
encodeNote note =
    E.object
        [ ( "id", E.int note.id )
        , ( "title", E.string note.title )
        , ( "body", E.string note.body )
        ]


decoder : Env -> D.Decoder Model
decoder _ =
    D.map2
        (\notes nextId ->
            { notes = notes
            , selected = Maybe.map .id (List.head notes)
            , nextId = nextId
            , query = ""
            }
        )
        (D.field "notes" (D.list noteDecoder))
        (D.oneOf [ D.field "nextId" D.int, D.succeed 1 ])


noteDecoder : D.Decoder Note
noteDecoder =
    D.map3 Note
        (D.field "id" D.int)
        (D.field "title" D.string)
        (D.field "body" D.string)
