module Todos exposing (main)

{-| **Todos** — a polished single-list to-do app. Add tasks, tick them off, edit inline, and
filter by All / Active / Completed. Every list lives inside the file itself; Ctrl+S writes it
back. Turn on **Cloud** and each task becomes a per-user [`Backend`](Backend) document you can
share with other users by their login (via [`Sync`](Sync)); shared tasks (owned by someone else)
show a badge and are read-only. See [`App`](App).
-}

import App exposing (Env)
import Html exposing (Html, button, div, footer, header, input, label, li, section, span, text, ul)
import Html.Attributes exposing (attribute, autofocus, checked, class, classList, disabled, placeholder, type_, value)
import Html.Events exposing (on, onBlur, onClick, onDoubleClick, onInput)
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
    { defaultBase = "https://todos.matsuo.pl" }



-- MODEL ----------------------------------------------------------------------


type alias Task =
    { id : String
    , text : String
    , done : Bool
    , owner : String -- backend owner uuid; "" for a local-only task
    }


type Filter
    = All
    | Active
    | Completed


type alias Model =
    { tasks : List Task
    , input : String
    , filter : Filter
    , seq : Int
    , editing : Maybe String
    , editText : String
    , sync : Sync.Model
    , shareFor : Maybe String
    , shareLogin : String
    }


init : Env -> ( Model, Cmd Msg )
init env =
    ( { tasks = []
      , input = ""
      , filter = All
      , seq = 1
      , editing = Nothing
      , editText = ""
      , sync = Sync.init config env.newId
      , shareFor = Nothing
      , shareLogin = ""
      }
    , Cmd.none
    )


mine : Model -> Task -> Bool
mine model task =
    task.owner == "" || task.owner == model.sync.uid



-- UPDATE ---------------------------------------------------------------------


type Msg
    = SetInput String
    | Add
    | Toggle String
    | Delete String
    | SetFilter Filter
    | ClearCompleted
    | StartEdit String String
    | SetEditText String
    | CommitEdit
    | CancelEdit
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
        SetInput s ->
            ( { model | input = s }, Cmd.none )

        Add ->
            let
                trimmed =
                    String.trim model.input
            in
            if trimmed == "" then
                ( model, Cmd.none )

            else
                let
                    task =
                        { id = model.sync.uid ++ "-" ++ String.fromInt model.seq
                        , text = trimmed
                        , done = False
                        , owner = model.sync.uid
                        }
                in
                ( { model
                    | tasks = model.tasks ++ [ task ]
                    , input = ""
                    , seq = model.seq + 1
                  }
                , pushIfCloud model task
                )

        Toggle id ->
            editTask model id (\t -> { t | done = not t.done })

        Delete id ->
            ( { model
                | tasks = List.filter (\t -> t.id /= id) model.tasks
                , editing =
                    if model.editing == Just id then
                        Nothing

                    else
                        model.editing
              }
            , removeIfCloud model id
            )

        SetFilter f ->
            ( { model | filter = f }, Cmd.none )

        ClearCompleted ->
            let
                doomed =
                    List.filter (\t -> t.done && mine model t) model.tasks
            in
            ( { model | tasks = List.filter (\t -> not (t.done && mine model t)) model.tasks }
            , Cmd.batch (List.map (\t -> removeIfCloud model t.id) doomed)
            )

        StartEdit id current ->
            ( { model | editing = Just id, editText = current }, Cmd.none )

        SetEditText s ->
            ( { model | editText = s }, Cmd.none )

        CommitEdit ->
            case model.editing of
                Nothing ->
                    ( model, Cmd.none )

                Just id ->
                    let
                        trimmed =
                            String.trim model.editText
                    in
                    if trimmed == "" then
                        -- Editing to empty deletes the task, matching the usual TodoMVC feel.
                        ( { model
                            | tasks = List.filter (\t -> t.id /= id) model.tasks
                            , editing = Nothing
                            , editText = ""
                          }
                        , removeIfCloud model id
                        )

                    else
                        editTask
                            { model | editing = Nothing, editText = "" }
                            id
                            (\t -> { t | text = trimmed })

        CancelEdit ->
            ( { model | editing = Nothing, editText = "" }, Cmd.none )

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


{-| Apply `f` to the task with `id`, and push the result to the cloud if it is ours. -}
editTask : Model -> String -> (Task -> Task) -> ( Model, Cmd Msg )
editTask model id f =
    let
        tasks =
            mapTask id f model.tasks

        changed =
            List.filter (\t -> t.id == id) tasks
    in
    ( { model | tasks = tasks }
    , case changed of
        t :: _ ->
            if mine model t then
                pushIfCloud model t

            else
                Cmd.none

        [] ->
            Cmd.none
    )


mapTask : String -> (Task -> Task) -> List Task -> List Task
mapTask id f tasks =
    List.map
        (\t ->
            if t.id == id then
                f t

            else
                t
        )
        tasks


pushIfCloud : Model -> Task -> Cmd Msg
pushIfCloud model task =
    if Sync.cloudOn model.sync then
        Sync.push model.sync (taskToDoc task) Pushed

    else
        Cmd.none


removeIfCloud : Model -> String -> Cmd Msg
removeIfCloud model id =
    if Sync.cloudOn model.sync then
        Sync.remove model.sync id Shared

    else
        Cmd.none


taskToDoc : Task -> Sync.Doc
taskToDoc task =
    { id = task.id
    , title = task.text
    , visibility = "private"
    , owner = task.owner
    , body = E.object [ ( "text", E.string task.text ), ( "done", E.bool task.done ) ]
    }


docToTask : Sync.Doc -> Maybe Task
docToTask doc =
    case D.decodeValue bodyDecoder doc.body of
        Ok ( t, d ) ->
            Just { id = doc.id, text = t, done = d, owner = doc.owner }

        Err _ ->
            Nothing


bodyDecoder : D.Decoder ( String, Bool )
bodyDecoder =
    D.map2 Tuple.pair
        (D.oneOf [ D.field "text" D.string, D.succeed "" ])
        (D.oneOf [ D.field "done" D.bool, D.succeed False ])


mergeDocs : List Sync.Doc -> Model -> Model
mergeDocs docs model =
    let
        remote =
            List.filterMap docToTask docs

        remoteIds =
            Set.fromList (List.map .id remote)

        localOnly =
            List.filter (\t -> not (Set.member t.id remoteIds)) model.tasks
    in
    { model | tasks = remote ++ localOnly }



-- VIEW -----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        visible =
            List.filter (keep model.filter) model.tasks

        remaining =
            List.length (List.filter (\t -> not t.done) model.tasks)

        completed =
            List.length (List.filter .done model.tasks)
    in
    div [ class "app" ]
        [ Html.map SyncMsg (Sync.view model.sync)
        , div [ class "app--under-bar" ]
            [ header [ class "toolbar" ]
                [ span [ class "toolbar__title" ] [ text "Todos" ]
                , input
                    [ class "todo-input"
                    , placeholder "What needs doing?"
                    , value model.input
                    , onInput SetInput
                    , onEnter Add
                    ]
                    []
                , button [ class "btn btn--primary", onClick Add ] [ text "Add" ]
                ]
            , section [ class "body" ]
                [ if List.isEmpty model.tasks then
                    emptyState

                  else
                    div []
                        [ ul [ class "todos" ] (List.map (taskRow model) visible)
                        , if List.isEmpty visible then
                            div [ class "todos-empty muted" ] [ text (noMatchLabel model.filter) ]

                          else
                            text ""
                        ]
                ]
            , footer [ class "todo-foot" ]
                [ span [ class "muted todo-count" ] [ text (countLabel remaining) ]
                , div [ class "filters" ]
                    [ filterBtn model.filter All "All"
                    , filterBtn model.filter Active "Active"
                    , filterBtn model.filter Completed "Completed"
                    ]
                , button
                    [ class "btn btn--ghost"
                    , onClick ClearCompleted
                    , attribute "aria-disabled"
                        (if completed == 0 then
                            "true"

                         else
                            "false"
                        )
                    ]
                    [ text ("Clear completed" ++ completedSuffix completed) ]
                ]
            ]
        ]


emptyState : Html Msg
emptyState =
    div [ class "empty" ]
        [ div []
            [ div [ class "todo-empty-title" ] [ text "Nothing to do yet" ]
            , div [ class "muted" ]
                [ text "Type a task above and press "
                , span [ class "kbd" ] [ text "Enter" ]
                , text " to add it."
                ]
            ]
        ]


taskRow : Model -> Task -> Html Msg
taskRow model task =
    if model.editing == Just task.id && mine model task then
        li [ class "todo is-editing" ]
            [ input
                [ class "todo-edit"
                , value model.editText
                , autofocus True
                , onInput SetEditText
                , onEnter CommitEdit
                , onEscape CancelEdit
                , onBlur CommitEdit
                ]
                []
            ]

    else if mine model task then
        li [ classList [ ( "todo", True ), ( "is-done", task.done ) ] ]
            [ label [ class "todo-check" ]
                [ input
                    [ type_ "checkbox"
                    , checked task.done
                    , onClick (Toggle task.id)
                    ]
                    []
                ]
            , span
                [ class "todo-text"
                , onDoubleClick (StartEdit task.id task.text)
                ]
                [ text task.text ]
            , shareControl model task
            , button
                [ class "btn btn--danger btn--sm todo-del"
                , onClick (Delete task.id)
                , attribute "aria-label" "Delete task"
                ]
                [ text "Delete" ]
            ]

    else
        -- Shared with us by another user: read-only.
        li [ classList [ ( "todo", True ), ( "is-done", task.done ), ( "is-shared", True ) ] ]
            [ label [ class "todo-check" ]
                [ input [ type_ "checkbox", checked task.done, disabled True ] [] ]
            , span [ class "todo-text" ] [ text task.text ]
            , span [ class "badge badge--shared" ] [ text "shared" ]
            ]


shareControl : Model -> Task -> Html Msg
shareControl model task =
    if not (Sync.cloudOn model.sync) then
        text ""

    else if model.shareFor == Just task.id then
        span [ class "share-form" ]
            [ input [ class "sync-in", placeholder "share with login", value model.shareLogin, onInput SetShareLogin ] []
            , button [ class "btn btn--primary btn--sm", onClick (DoShare task.id) ] [ text "Share" ]
            , button [ class "btn btn--ghost btn--sm", onClick (OpenShare Nothing) ] [ text "×" ]
            ]

    else
        button [ class "btn btn--ghost btn--sm todo-share", onClick (OpenShare (Just task.id)) ] [ text "Share" ]


filterBtn : Filter -> Filter -> String -> Html Msg
filterBtn current f label_ =
    button
        [ classList
            [ ( "btn", True )
            , ( "btn--ghost", True )
            , ( "filter", True )
            , ( "is-active", current == f )
            ]
        , onClick (SetFilter f)
        ]
        [ text label_ ]


keep : Filter -> Task -> Bool
keep filter task =
    case filter of
        All ->
            True

        Active ->
            not task.done

        Completed ->
            task.done


countLabel : Int -> String
countLabel n =
    if n == 1 then
        "1 item left"

    else
        String.fromInt n ++ " items left"


completedSuffix : Int -> String
completedSuffix n =
    if n == 0 then
        ""

    else
        " (" ++ String.fromInt n ++ ")"


noMatchLabel : Filter -> String
noMatchLabel filter =
    case filter of
        All ->
            "No tasks."

        Active ->
            "No active tasks — all done!"

        Completed ->
            "Nothing completed yet."



-- KEYBOARD -------------------------------------------------------------------


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    onKey 13 msg


onEscape : Msg -> Html.Attribute Msg
onEscape msg =
    onKey 27 msg


onKey : Int -> Msg -> Html.Attribute Msg
onKey code msg =
    on "keydown"
        (D.field "keyCode" D.int
            |> D.andThen
                (\actual ->
                    if actual == code then
                        D.succeed msg

                    else
                        D.fail "other key"
                )
        )



-- CODEC ----------------------------------------------------------------------


encode : Model -> E.Value
encode model =
    E.object
        [ ( "tasks", E.list encodeTask model.tasks )
        , ( "seq", E.int model.seq )
        , ( "sync", Sync.encode model.sync )
        ]


encodeTask : Task -> E.Value
encodeTask task =
    E.object
        [ ( "id", E.string task.id )
        , ( "text", E.string task.text )
        , ( "done", E.bool task.done )
        , ( "owner", E.string task.owner )
        ]


decoder : Env -> D.Decoder Model
decoder env =
    D.map3
        (\tasks seq sync ->
            { tasks = tasks
            , input = ""
            , filter = All
            , seq = seq
            , editing = Nothing
            , editText = ""
            , sync = sync
            , shareFor = Nothing
            , shareLogin = ""
            }
        )
        (D.oneOf [ D.field "tasks" (D.list taskDecoder), D.succeed [] ])
        (D.oneOf [ D.field "seq" D.int, D.field "nextId" D.int, D.succeed 1 ])
        (D.oneOf [ D.field "sync" (Sync.decoder config env.newId), D.succeed (Sync.init config env.newId) ])


taskDecoder : D.Decoder Task
taskDecoder =
    D.map4 Task
        (D.field "id" idDecoder)
        (D.field "text" D.string)
        (D.oneOf [ D.field "done" D.bool, D.succeed False ])
        (D.oneOf [ D.field "owner" D.string, D.succeed "" ])


{-| Old files stored integer task ids; accept both so existing lists still load. -}
idDecoder : D.Decoder String
idDecoder =
    D.oneOf [ D.string, D.map String.fromInt D.int ]
