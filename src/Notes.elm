module Notes exposing (main)

{-| **Notes** — a two-pane note keeper. Notes live in the file (Ctrl+S), and can optionally sync to a
[`Backend`](Backend) server via [`Sync`](Sync): turn on **Cloud**, and each note becomes a per-user
document you can share with other users by their login. Shared notes (owned by someone else) show a
badge and are read-only. See [`App`](App).
-}

import App exposing (Env)
import Html exposing (Html, button, div, header, input, li, span, text, textarea, ul)
import Html.Attributes exposing (class, classList, disabled, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import Set
import Sync


main : Program () (App.State Model) (App.Event Msg)
main =
    App.programEffect
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , encode = encode
        , decoder = decoder
        }


config : Sync.Config
config =
    { defaultBase = "https://notes.matsuo.pl" }



-- MODEL


type alias Note =
    { id : String
    , title : String
    , body : String
    , owner : String -- backend owner uuid; "" for a local-only note
    }


type alias Model =
    { notes : List Note
    , selected : Maybe String
    , seq : Int
    , query : String
    , sync : Sync.Model
    , shareFor : Maybe String
    , shareLogin : String
    }


init : Env -> ( Model, Cmd Msg )
init env =
    ( { notes = []
      , selected = Nothing
      , seq = 1
      , query = ""
      , sync = Sync.init config env.newId
      , shareFor = Nothing
      , shareLogin = ""
      }
    , Cmd.none
    )


mine : Model -> Note -> Bool
mine model note =
    note.owner == "" || note.owner == model.sync.uid



-- UPDATE


type Msg
    = New
    | Select String
    | Delete String
    | SetTitle String
    | SetBody String
    | SetQuery String
    | SyncMsg Sync.Msg
    | GotDocs (Result Http.Error (List Sync.Doc))
    | Pushed (Result Http.Error ())
    | OpenShare (Maybe String)
    | SetShareLogin String
    | DoShare String
    | Shared (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New ->
            let
                id =
                    model.sync.uid ++ "-" ++ String.fromInt model.seq

                note =
                    { id = id, title = "Untitled", body = "", owner = model.sync.uid }
            in
            ( { model | notes = note :: model.notes, selected = Just id, seq = model.seq + 1 }
            , pushIfCloud model note
            )

        Select id ->
            ( { model | selected = Just id, shareFor = Nothing }, Cmd.none )

        Delete id ->
            ( { model
                | notes = List.filter (\n -> n.id /= id) model.notes
                , selected =
                    if model.selected == Just id then
                        Nothing

                    else
                        model.selected
              }
            , removeIfCloud model id
            )

        SetTitle t ->
            editSelected model (\n -> { n | title = t })

        SetBody b ->
            editSelected model (\n -> { n | body = b })

        SetQuery q ->
            ( { model | query = q }, Cmd.none )

        SyncMsg m ->
            let
                ( sync2, cmd, out ) =
                    Sync.update m model.sync

                model2 =
                    { model | sync = sync2 }
            in
            case out of
                Sync.Refresh ->
                    ( model2, Cmd.batch [ Cmd.map SyncMsg cmd, Sync.pull sync2 GotDocs ] )

                _ ->
                    ( model2, Cmd.map SyncMsg cmd )

        GotDocs (Ok docs) ->
            ( mergeDocs docs model, Cmd.none )

        GotDocs (Err _) ->
            ( model, Cmd.none )

        Pushed _ ->
            ( model, Cmd.none )

        OpenShare id ->
            ( { model | shareFor = id, shareLogin = "" }, Cmd.none )

        SetShareLogin s ->
            ( { model | shareLogin = s }, Cmd.none )

        DoShare id ->
            ( { model | shareFor = Nothing }, Sync.share model.sync id model.shareLogin Shared )

        Shared _ ->
            ( model, Cmd.none )


editSelected : Model -> (Note -> Note) -> ( Model, Cmd Msg )
editSelected model f =
    case model.selected of
        Nothing ->
            ( model, Cmd.none )

        Just id ->
            let
                notes =
                    List.map
                        (\n ->
                            if n.id == id then
                                f n

                            else
                                n
                        )
                        model.notes

                changed =
                    List.filter (\n -> n.id == id) notes
            in
            ( { model | notes = notes }
            , case changed of
                n :: _ ->
                    if mine model n then
                        pushIfCloud model n

                    else
                        Cmd.none

                [] ->
                    Cmd.none
            )


pushIfCloud : Model -> Note -> Cmd Msg
pushIfCloud model note =
    if Sync.cloudOn model.sync then
        Sync.push model.sync (noteToDoc note) Pushed

    else
        Cmd.none


removeIfCloud : Model -> String -> Cmd Msg
removeIfCloud model id =
    if Sync.cloudOn model.sync then
        Sync.remove model.sync id Shared

    else
        Cmd.none


noteToDoc : Note -> Sync.Doc
noteToDoc note =
    { id = note.id
    , title = note.title
    , visibility = "private"
    , owner = note.owner
    , body = E.object [ ( "title", E.string note.title ), ( "body", E.string note.body ) ]
    }


docToNote : Sync.Doc -> Maybe Note
docToNote doc =
    case D.decodeValue bodyDecoder doc.body of
        Ok ( t, b ) ->
            Just { id = doc.id, title = t, body = b, owner = doc.owner }

        Err _ ->
            Nothing


bodyDecoder : D.Decoder ( String, String )
bodyDecoder =
    D.map2 Tuple.pair
        (D.oneOf [ D.field "title" D.string, D.succeed "Untitled" ])
        (D.oneOf [ D.field "body" D.string, D.succeed "" ])


mergeDocs : List Sync.Doc -> Model -> Model
mergeDocs docs model =
    let
        remote =
            List.filterMap docToNote docs

        remoteIds =
            Set.fromList (List.map .id remote)

        localOnly =
            List.filter (\n -> not (Set.member n.id remoteIds)) model.notes
    in
    { model | notes = remote ++ localOnly }



-- VIEW


view : Model -> Html Msg
view model =
    let
        visible =
            List.filter (matches model.query) model.notes
    in
    div [ class "app" ]
        [ Html.map SyncMsg (Sync.view model.sync)
        , div [ class "app--split app--under-bar" ]
            [ div [ class "pane pane--list" ]
                [ header [ class "pane__head" ]
                    [ input [ class "search", placeholder "Search notes…", value model.query, onInput SetQuery ] []
                    , button [ class "btn btn--primary", onClick New ] [ text "+ New" ]
                    ]
                , ul [ class "list" ] (List.map (row model) visible)
                ]
            , div [ class "pane pane--main" ] [ editor model ]
            ]
        ]


matches : String -> Note -> Bool
matches q note =
    let
        needle =
            String.toLower (String.trim q)
    in
    needle == "" || String.contains needle (String.toLower note.title) || String.contains needle (String.toLower note.body)


row : Model -> Note -> Html Msg
row model note =
    li
        [ classList [ ( "list__item", True ), ( "is-active", model.selected == Just note.id ) ]
        , onClick (Select note.id)
        ]
        [ span [ class "list__title" ]
            [ text (nonEmpty note.title "Untitled")
            , if mine model note then
                text ""

              else
                span [ class "badge badge--shared" ] [ text "shared" ]
            ]
        , span [ class "list__sub" ] [ text (preview note.body) ]
        ]


editor : Model -> Html Msg
editor model =
    case selectedNote model of
        Nothing ->
            div [ class "empty" ] [ text "Select a note, or create one with ", span [ class "kbd" ] [ text "+ New" ], text "." ]

        Just note ->
            let
                readOnly =
                    not (mine model note)
            in
            div [ class "editor" ]
                [ input [ class "editor__title", value note.title, placeholder "Title", onInput SetTitle, disabled readOnly ] []
                , textarea [ class "editor__body", value note.body, placeholder "Start writing…", onInput SetBody, disabled readOnly ] []
                , div [ class "editor__foot" ]
                    (if readOnly then
                        [ span [ class "muted" ] [ text ("shared by " ++ String.left 6 note.owner) ] ]

                     else
                        [ shareControl model note
                        , button [ class "btn btn--danger", onClick (Delete note.id) ] [ text "Delete" ]
                        ]
                    )
                ]


shareControl : Model -> Note -> Html Msg
shareControl model note =
    if not (Sync.cloudOn model.sync) then
        text ""

    else if model.shareFor == Just note.id then
        div [ class "share-form" ]
            [ input [ class "sync-in", placeholder "share with login", value model.shareLogin, onInput SetShareLogin ] []
            , button [ class "btn btn--primary", onClick (DoShare note.id) ] [ text "Share" ]
            , button [ class "btn btn--ghost", onClick (OpenShare Nothing) ] [ text "×" ]
            ]

    else
        button [ class "btn", onClick (OpenShare (Just note.id)) ] [ text "Share" ]


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



-- CODEC


encode : Model -> E.Value
encode model =
    E.object
        [ ( "notes", E.list encodeNote model.notes )
        , ( "seq", E.int model.seq )
        , ( "sync", Sync.encode model.sync )
        ]


encodeNote : Note -> E.Value
encodeNote note =
    E.object
        [ ( "id", E.string note.id )
        , ( "title", E.string note.title )
        , ( "body", E.string note.body )
        , ( "owner", E.string note.owner )
        ]


decoder : Env -> D.Decoder Model
decoder env =
    D.map3
        (\notes seq sync ->
            { notes = notes
            , selected = Maybe.map .id (List.head notes)
            , seq = seq
            , query = ""
            , sync = sync
            , shareFor = Nothing
            , shareLogin = ""
            }
        )
        (D.oneOf [ D.field "notes" (D.list noteDecoder), D.succeed [] ])
        (D.oneOf [ D.field "seq" D.int, D.succeed 1 ])
        (D.oneOf [ D.field "sync" (Sync.decoder config env.newId), D.succeed (Sync.init config env.newId) ])


noteDecoder : D.Decoder Note
noteDecoder =
    D.map4 Note
        (D.field "id" idDecoder)
        (D.field "title" D.string)
        (D.field "body" D.string)
        (D.oneOf [ D.field "owner" D.string, D.succeed "" ])


{-| Old files stored integer note ids; accept both so existing notes still load. -}
idDecoder : D.Decoder String
idDecoder =
    D.oneOf [ D.string, D.map String.fromInt D.int ]
