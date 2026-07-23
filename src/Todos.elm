module Todos exposing (main)

{-| **Todos** — a polished single-list to-do app. Add tasks, tick them off, edit inline, and
filter by All / Active / Completed. Every list lives inside the file itself; Ctrl+S writes it
back. See [`App`](App).
-}

import App exposing (Env)
import Html exposing (Html, button, div, footer, header, input, label, li, section, span, text, ul)
import Html.Attributes exposing (attribute, autofocus, checked, class, classList, placeholder, type_, value)
import Html.Events exposing (on, onBlur, onClick, onDoubleClick, onInput)
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


type alias Task =
    { id : Int
    , text : String
    , done : Bool
    }


type Filter
    = All
    | Active
    | Completed


type alias Model =
    { tasks : List Task
    , input : String
    , filter : Filter
    , nextId : Int
    , editing : Maybe Int
    , editText : String
    }


init : Env -> Model
init _ =
    { tasks = []
    , input = ""
    , filter = All
    , nextId = 1
    , editing = Nothing
    , editText = ""
    }



-- UPDATE ---------------------------------------------------------------------


type Msg
    = SetInput String
    | Add
    | Toggle Int
    | Delete Int
    | SetFilter Filter
    | ClearCompleted
    | StartEdit Int String
    | SetEditText String
    | CommitEdit
    | CancelEdit


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetInput s ->
            { model | input = s }

        Add ->
            let
                trimmed =
                    String.trim model.input
            in
            if trimmed == "" then
                model

            else
                { model
                    | tasks = model.tasks ++ [ { id = model.nextId, text = trimmed, done = False } ]
                    , input = ""
                    , nextId = model.nextId + 1
                }

        Toggle id ->
            { model | tasks = mapTask id (\t -> { t | done = not t.done }) model.tasks }

        Delete id ->
            { model
                | tasks = List.filter (\t -> t.id /= id) model.tasks
                , editing =
                    if model.editing == Just id then
                        Nothing

                    else
                        model.editing
            }

        SetFilter f ->
            { model | filter = f }

        ClearCompleted ->
            { model | tasks = List.filter (\t -> not t.done) model.tasks }

        StartEdit id current ->
            { model | editing = Just id, editText = current }

        SetEditText s ->
            { model | editText = s }

        CommitEdit ->
            case model.editing of
                Nothing ->
                    model

                Just id ->
                    let
                        trimmed =
                            String.trim model.editText
                    in
                    if trimmed == "" then
                        -- Editing to empty deletes the task, matching the usual TodoMVC feel.
                        { model
                            | tasks = List.filter (\t -> t.id /= id) model.tasks
                            , editing = Nothing
                            , editText = ""
                        }

                    else
                        { model
                            | tasks = mapTask id (\t -> { t | text = trimmed }) model.tasks
                            , editing = Nothing
                            , editText = ""
                        }

        CancelEdit ->
            { model | editing = Nothing, editText = "" }


mapTask : Int -> (Task -> Task) -> List Task -> List Task
mapTask id f tasks =
    List.map
        (\t ->
            if t.id == id then
                f t

            else
                t
        )
        tasks



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
                    [ ul [ class "todos" ] (List.map (taskRow model.editing model.editText) visible)
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


taskRow : Maybe Int -> String -> Task -> Html Msg
taskRow editing editText task =
    if editing == Just task.id then
        li [ class "todo is-editing" ]
            [ input
                [ class "todo-edit"
                , value editText
                , autofocus True
                , onInput SetEditText
                , onEnter CommitEdit
                , onEscape CancelEdit
                , onBlur CommitEdit
                ]
                []
            ]

    else
        li [ class "todo", classList [ ( "is-done", task.done ) ] ]
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
            , button
                [ class "btn btn--danger btn--sm todo-del"
                , onClick (Delete task.id)
                , attribute "aria-label" "Delete task"
                ]
                [ text "Delete" ]
            ]


filterBtn : Filter -> Filter -> String -> Html Msg
filterBtn current f label_ =
    button
        [ class "btn btn--ghost filter"
        , classList [ ( "is-active", current == f ) ]
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
        , ( "nextId", E.int model.nextId )
        ]


encodeTask : Task -> E.Value
encodeTask task =
    E.object
        [ ( "id", E.int task.id )
        , ( "text", E.string task.text )
        , ( "done", E.bool task.done )
        ]


decoder : Env -> D.Decoder Model
decoder _ =
    D.map2
        (\tasks nextId ->
            { tasks = tasks
            , input = ""
            , filter = All
            , nextId = nextId
            , editing = Nothing
            , editText = ""
            }
        )
        (D.oneOf [ D.field "tasks" (D.list taskDecoder), D.succeed [] ])
        (D.oneOf [ D.field "nextId" D.int, D.succeed 1 ])


taskDecoder : D.Decoder Task
taskDecoder =
    D.map3 Task
        (D.field "id" D.int)
        (D.field "text" D.string)
        (D.oneOf [ D.field "done" D.bool, D.succeed False ])
