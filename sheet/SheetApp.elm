module SheetApp exposing (main)

{-| **Spreadsheet** — the real [elm-spreadsheet](https://github.com/tunguski/elm-spreadsheet) engine
and grid as a self-saving single-file app, now with an optional **cloud** account.

The heavy lifting is entirely reused: [`SheetDoc.config`](SheetDoc) is the workspace document for a
single spreadsheet — its `codec` (JSON), `empty`/`activate` (create + recalc), `updateDoc` (pure) and
`viewDoc` (the full grid). We drive that config through the shared [`App`](App) harness so the sheet's
cells live in the file itself and Ctrl+S writes them back.

On top of that, the whole sheet is treated as **one** backend document (id `<uid>-sheet`). Turn on
**Cloud** and every edit pushes the sheet up; a private/public toggle controls whether other users can
find it; and a small search box browses other people's public sheets read-only. See [`Sync`](Sync).

The workspace *site* shell (managing many spreadsheets in localStorage) is intentionally dropped —
file-save + this per-user cloud document replace it: one file _is_ one spreadsheet.

-}

import App exposing (Env)
import Dict
import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import SheetDoc exposing (SheetDoc, SheetMsg)
import Sync
import Workspace


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
    { defaultBase = "https://spreadsheet.matsuo.pl" }



-- MODEL


type alias Model =
    { sheet : SheetDoc -- the user's own, editable, persisted+pushed spreadsheet
    , sync : Sync.Model
    , visibility : String -- "private" | "public"
    , browseQuery : String
    , browseResults : List Sync.Doc -- other users' public sheets
    , browsing : Maybe SheetDoc -- a public sheet opened read-only (Nothing = editing own)
    }


init : Env -> ( Model, Cmd Msg )
init env =
    ( { sheet = SheetDoc.config.activate SheetDoc.config.empty
      , sync = Sync.init config env.newId
      , visibility = "private"
      , browseQuery = ""
      , browseResults = []
      , browsing = Nothing
      }
    , Cmd.none
    )


{-| The fixed per-user document id for this file's sheet. -}
docId : Model -> String
docId model =
    model.sync.uid ++ "-sheet"


ownDoc : Model -> Sync.Doc
ownDoc model =
    { id = docId model
    , title = "My sheet"
    , visibility = model.visibility
    , owner = model.sync.uid
    , body = SheetDoc.config.codec.encode model.sheet
    }


pushIfCloud : Model -> Cmd Msg
pushIfCloud model =
    if Sync.cloudOn model.sync then
        Sync.push model.sync (ownDoc model) Pushed

    else
        Cmd.none



-- UPDATE


type Msg
    = SheetMsg SheetMsg
    | SyncMsg Sync.Msg
    | GotDocs (Result Http.Error (List Sync.Doc))
    | Pushed (Result Http.Error ())
    | SetVisibility String
    | SetBrowseQuery String
    | DoBrowse
    | GotPublic (Result Http.Error (List Sync.Doc))
    | ViewPublic String
    | CloseBrowse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SheetMsg m ->
            case model.browsing of
                -- Editing a public sheet mutates only the local read-only copy; never pushed.
                Just b ->
                    ( { model | browsing = Just (SheetDoc.config.updateDoc m b) }, Cmd.none )

                Nothing ->
                    let
                        model2 =
                            { model | sheet = SheetDoc.config.updateDoc m model.sheet }
                    in
                    ( model2, pushIfCloud model2 )

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
            let
                own =
                    List.filter (\d -> d.id == docId model || d.owner == model.sync.uid) docs
                        |> List.head
            in
            case own of
                Just doc ->
                    case D.decodeValue SheetDoc.config.codec.decoder doc.body of
                        Ok sd ->
                            ( { model | sheet = SheetDoc.config.activate sd, visibility = doc.visibility }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                Nothing ->
                    -- No sheet on the server yet: seed it with what we have.
                    ( model, pushIfCloud model )

        GotDocs (Err _) ->
            ( model, Cmd.none )

        Pushed _ ->
            ( model, Cmd.none )

        SetVisibility v ->
            let
                model2 =
                    { model | visibility = v }
            in
            ( model2, pushIfCloud model2 )

        SetBrowseQuery q ->
            ( { model | browseQuery = q }, Cmd.none )

        DoBrowse ->
            ( model, Sync.search model.sync model.browseQuery GotPublic )

        GotPublic (Ok docs) ->
            ( { model | browseResults = List.filter (\d -> d.owner /= model.sync.uid) docs }, Cmd.none )

        GotPublic (Err _) ->
            ( model, Cmd.none )

        ViewPublic id ->
            case List.head (List.filter (\d -> d.id == id) model.browseResults) of
                Just doc ->
                    case D.decodeValue SheetDoc.config.codec.decoder doc.body of
                        Ok sd ->
                            ( { model | browsing = Just (SheetDoc.config.activate sd) }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CloseBrowse ->
            ( { model | browsing = Nothing }, Cmd.none )



-- VIEW


{-| A minimal editor environment: comments and cross-document references are workspace features that
don't apply to a standalone file, so they're switched off. With `commentsVisible = False`, the grid
never renders the comment marker (the one place the document view would want an icon font).
-}
editorEnv : Workspace.EditorEnv
editorEnv =
    { comments = Dict.empty
    , commentsVisible = False
    , commentCount = \_ -> 0
    , refError = Nothing
    , refWarnings = []
    }


view : Model -> Html Msg
view model =
    let
        displayed =
            Maybe.withDefault model.sheet model.browsing
    in
    div [ class "app" ]
        [ Html.map SyncMsg (Sync.view model.sync)
        , cloudBar model
        , div [ class "app--under-bar" ]
            [ Html.map SheetMsg (SheetDoc.config.viewDoc editorEnv displayed) ]
        ]


{-| The controls that sit just below the Sync account bar: a private/public toggle for the user's own
sheet, and a search box to browse other people's public sheets read-only. -}
cloudBar : Model -> Html Msg
cloudBar model =
    div [ class "sheet-cloudbar" ]
        [ label [ class "sheet-vis-toggle" ]
            [ input
                [ type_ "checkbox"
                , Html.Attributes.checked (model.visibility == "public")
                , onClick (SetVisibility (toggleVis model.visibility))
                ]
                []
            , span [] [ text "Public" ]
            ]
        , input
            [ class "sheet-browse-in"
            , placeholder "Browse public sheets…"
            , value model.browseQuery
            , onInput SetBrowseQuery
            ]
            []
        , button [ class "btn btn--ghost", onClick DoBrowse ] [ text "Search" ]
        , browseResults model
        , case model.browsing of
            Just _ ->
                span [ class "sheet-readonly" ]
                    [ text "Viewing a public sheet (read-only) "
                    , button [ class "btn btn--ghost", onClick CloseBrowse ] [ text "Back to my sheet" ]
                    ]

            Nothing ->
                text ""
        ]


browseResults : Model -> Html Msg
browseResults model =
    if List.isEmpty model.browseResults then
        text ""

    else
        div [ class "sheet-browse-results" ]
            (List.map
                (\d ->
                    button
                        [ class "btn", onClick (ViewPublic d.id) ]
                        [ text (nonEmpty d.title "Untitled") ]
                )
                model.browseResults
            )


toggleVis : String -> String
toggleVis v =
    if v == "public" then
        "private"

    else
        "public"


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
        [ ( "sheet", SheetDoc.config.codec.encode model.sheet )
        , ( "sync", Sync.encode model.sync )
        , ( "visibility", E.string model.visibility )
        ]


decoder : Env -> D.Decoder Model
decoder env =
    D.map3
        (\sheet sync visibility ->
            { sheet = SheetDoc.config.activate sheet
            , sync = sync
            , visibility = visibility
            , browseQuery = ""
            , browseResults = []
            , browsing = Nothing
            }
        )
        (D.oneOf [ D.field "sheet" SheetDoc.config.codec.decoder, D.succeed SheetDoc.config.empty ])
        (D.oneOf [ D.field "sync" (Sync.decoder config env.newId), D.succeed (Sync.init config env.newId) ])
        (D.oneOf [ D.field "visibility" D.string, D.succeed "private" ])
