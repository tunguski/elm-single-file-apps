module Calendar exposing (main)

{-| **Calendar** — a month-view calendar with per-day events. Navigate months, jump to today,
click a day to add or delete events. All date math is pure integer arithmetic (no `Time`); the
current date arrives via the boot `Env`. Every change round-trips through the embedded data block,
and Ctrl+S writes the page back to disk.

Events can optionally sync to a [`Backend`](Backend) server via [`Sync`](Sync): turn on **Cloud**, and
each event becomes a per-user document you can share with other users by their login. Events owned by
someone else show a badge and are read-only. See [`App`](App).
-}

import App exposing (Env)
import Dict exposing (Dict)
import Html exposing (Html, button, div, form, header, input, li, section, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
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
    { defaultBase = "https://calendar.matsuo.pl" }



-- MODEL ----------------------------------------------------------------------


type alias Event =
    { id : String
    , title : String
    , time : String
    , owner : String -- backend owner uuid; "" for a local-only event
    }


type alias Model =
    { events : Dict String (List Event)
    , year : Int
    , month : Int
    , selected : Maybe String
    , seq : Int
    , today : String
    , draftTitle : String
    , draftTime : String
    , sync : Sync.Model
    , shareFor : Maybe String
    , shareLogin : String
    }


init : Env -> ( Model, Cmd Msg )
init env =
    let
        ( y, m ) =
            defaultYearMonth env.today
    in
    ( { events = Dict.empty
      , year = y
      , month = m
      , selected = Nothing
      , seq = 1
      , today = env.today
      , draftTitle = ""
      , draftTime = ""
      , sync = Sync.init config env.newId
      , shareFor = Nothing
      , shareLogin = ""
      }
    , Cmd.none
    )


mine : Model -> Event -> Bool
mine model ev =
    ev.owner == "" || ev.owner == model.sync.uid



-- UPDATE ---------------------------------------------------------------------


type Msg
    = PrevMonth
    | NextMonth
    | GoToday
    | SelectDay String
    | ClosePanel
    | SetDraftTitle String
    | SetDraftTime String
    | AddEvent
    | DeleteEvent String String
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
        PrevMonth ->
            let
                ( y, m ) =
                    if model.month == 1 then
                        ( model.year - 1, 12 )

                    else
                        ( model.year, model.month - 1 )
            in
            ( { model | year = y, month = m }, Cmd.none )

        NextMonth ->
            let
                ( y, m ) =
                    if model.month == 12 then
                        ( model.year + 1, 1 )

                    else
                        ( model.year, model.month + 1 )
            in
            ( { model | year = y, month = m }, Cmd.none )

        GoToday ->
            let
                ( y, m ) =
                    defaultYearMonth model.today
            in
            ( { model | year = y, month = m }, Cmd.none )

        SelectDay key ->
            if model.selected == Just key then
                ( { model | selected = Nothing }, Cmd.none )

            else
                ( { model | selected = Just key, draftTitle = "", draftTime = "", shareFor = Nothing }, Cmd.none )

        ClosePanel ->
            ( { model | selected = Nothing }, Cmd.none )

        SetDraftTitle s ->
            ( { model | draftTitle = s }, Cmd.none )

        SetDraftTime s ->
            ( { model | draftTime = s }, Cmd.none )

        AddEvent ->
            case model.selected of
                Nothing ->
                    ( model, Cmd.none )

                Just key ->
                    let
                        title =
                            String.trim model.draftTitle
                    in
                    if title == "" then
                        ( model, Cmd.none )

                    else
                        let
                            ev =
                                { id = model.sync.uid ++ "-" ++ String.fromInt model.seq
                                , title = title
                                , time = String.trim model.draftTime
                                , owner = model.sync.uid
                                }

                            existing =
                                Maybe.withDefault [] (Dict.get key model.events)

                            updated =
                                sortEvents (existing ++ [ ev ])
                        in
                        ( { model
                            | events = Dict.insert key updated model.events
                            , seq = model.seq + 1
                            , draftTitle = ""
                            , draftTime = ""
                          }
                        , pushIfCloud model key ev
                        )

        DeleteEvent key id ->
            let
                remaining =
                    Maybe.withDefault [] (Dict.get key model.events)
                        |> List.filter (\e -> e.id /= id)

                events =
                    if List.isEmpty remaining then
                        Dict.remove key model.events

                    else
                        Dict.insert key remaining model.events
            in
            ( { model | events = events }, removeIfCloud model id )

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


{-| Events with a time sort ahead of untimed ones; ties keep insertion order via a stable-ish key.
-}
sortEvents : List Event -> List Event
sortEvents evs =
    List.sortBy
        (\e ->
            if e.time == "" then
                "~" ++ e.id

            else
                e.time
        )
        evs



-- SYNC GLUE ------------------------------------------------------------------


pushIfCloud : Model -> String -> Event -> Cmd Msg
pushIfCloud model key ev =
    if Sync.cloudOn model.sync then
        Sync.push model.sync (eventToDoc key ev) Pushed

    else
        Cmd.none


removeIfCloud : Model -> String -> Cmd Msg
removeIfCloud model id =
    if Sync.cloudOn model.sync then
        Sync.remove model.sync id Shared

    else
        Cmd.none


eventToDoc : String -> Event -> Sync.Doc
eventToDoc key ev =
    { id = ev.id
    , title = ev.title
    , visibility = "private"
    , owner = ev.owner
    , body =
        E.object
            [ ( "date", E.string key )
            , ( "title", E.string ev.title )
            , ( "time", E.string ev.time )
            ]
    }


docToEvent : Sync.Doc -> Maybe ( String, Event )
docToEvent doc =
    case D.decodeValue bodyDecoder doc.body of
        Ok ( date, title, time ) ->
            Just ( date, { id = doc.id, title = title, time = time, owner = doc.owner } )

        Err _ ->
            Nothing


bodyDecoder : D.Decoder ( String, String, String )
bodyDecoder =
    D.map3 (\date title time -> ( date, title, time ))
        (D.oneOf [ D.field "date" D.string, D.succeed "" ])
        (D.oneOf [ D.field "title" D.string, D.succeed "Untitled" ])
        (D.oneOf [ D.field "time" D.string, D.succeed "" ])


{-| Rebuild the event dict from the remote events plus any local-only events (by id) not present
remotely, filing each under its own date. -}
mergeDocs : List Sync.Doc -> Model -> Model
mergeDocs docs model =
    let
        remotePairs =
            List.filterMap docToEvent docs

        remoteIds =
            Set.fromList (List.map (\( _, ev ) -> ev.id) remotePairs)

        localPairs =
            Dict.toList model.events
                |> List.concatMap (\( k, evs ) -> List.map (\ev -> ( k, ev )) evs)
                |> List.filter (\( _, ev ) -> not (Set.member ev.id remoteIds))

        rebuilt =
            List.foldl
                (\( k, ev ) acc ->
                    Dict.update k (\m -> Just (ev :: Maybe.withDefault [] m)) acc)
                Dict.empty
                (remotePairs ++ localPairs)
    in
    { model | events = Dict.map (\_ evs -> sortEvents evs) rebuilt }



-- DATE MATH ------------------------------------------------------------------


isLeap : Int -> Bool
isLeap y =
    (modBy 4 y == 0 && modBy 100 y /= 0) || modBy 400 y == 0


daysInMonth : Int -> Int -> Int
daysInMonth y m =
    case m of
        1 ->
            31

        2 ->
            if isLeap y then
                29

            else
                28

        3 ->
            31

        4 ->
            30

        5 ->
            31

        6 ->
            30

        7 ->
            31

        8 ->
            31

        9 ->
            30

        10 ->
            31

        11 ->
            30

        _ ->
            31


{-| Sakamoto's algorithm. Returns 0 = Sunday .. 6 = Saturday.
Verified: 2026-01-01 → Thu, 2000-01-01 → Sat, 2024-02-29 → Thu.
-}
dayOfWeek : Int -> Int -> Int -> Int
dayOfWeek year month day =
    let
        t =
            [ 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4 ]

        tm =
            List.drop (month - 1) t |> List.head |> Maybe.withDefault 0

        y =
            if month < 3 then
                year - 1

            else
                year
    in
    modBy 7 (y + (y // 4) - (y // 100) + (y // 400) + tm + day)


{-| Monday-based weekday index: 0 = Monday .. 6 = Sunday.
-}
mondayIndex : Int -> Int -> Int -> Int
mondayIndex year month day =
    modBy 7 (dayOfWeek year month day + 6)


parseDate : String -> Maybe ( Int, Int, Int )
parseDate s =
    case String.split "-" s of
        [ ys, ms, ds ] ->
            case ( String.toInt ys, String.toInt ms, String.toInt ds ) of
                ( Just y, Just m, Just d ) ->
                    Just ( y, m, d )

                _ ->
                    Nothing

        _ ->
            Nothing


defaultYearMonth : String -> ( Int, Int )
defaultYearMonth s =
    case parseDate s of
        Just ( y, m, _ ) ->
            ( y, m )

        Nothing ->
            ( 2026, 1 )


pad2 : Int -> String
pad2 n =
    String.padLeft 2 '0' (String.fromInt n)


dateKey : Int -> Int -> Int -> String
dateKey y m d =
    String.padLeft 4 '0' (String.fromInt y) ++ "-" ++ pad2 m ++ "-" ++ pad2 d


monthName : Int -> String
monthName m =
    case m of
        1 ->
            "January"

        2 ->
            "February"

        3 ->
            "March"

        4 ->
            "April"

        5 ->
            "May"

        6 ->
            "June"

        7 ->
            "July"

        8 ->
            "August"

        9 ->
            "September"

        10 ->
            "October"

        11 ->
            "November"

        _ ->
            "December"



-- GRID -----------------------------------------------------------------------


type alias Cell =
    { key : String
    , day : Int
    , inMonth : Bool
    }


{-| The cells covering the whole month, padded with adjacent-month days to fill complete weeks.
-}
monthCells : Int -> Int -> List Cell
monthCells year month =
    let
        leading =
            mondayIndex year month 1

        dim =
            daysInMonth year month

        ( py, pm ) =
            if month == 1 then
                ( year - 1, 12 )

            else
                ( year, month - 1 )

        pdim =
            daysInMonth py pm

        ( ny, nm ) =
            if month == 12 then
                ( year + 1, 1 )

            else
                ( year, month + 1 )

        leadCells =
            List.range (pdim - leading + 1) pdim
                |> List.map (\d -> Cell (dateKey py pm d) d False)

        thisCells =
            List.range 1 dim
                |> List.map (\d -> Cell (dateKey year month d) d True)

        used =
            leading + dim

        trailing =
            modBy 7 (7 - modBy 7 used)

        trailCells =
            List.range 1 trailing
                |> List.map (\d -> Cell (dateKey ny nm d) d False)
    in
    leadCells ++ thisCells ++ trailCells


chunk7 : List a -> List (List a)
chunk7 xs =
    case xs of
        [] ->
            []

        _ ->
            List.take 7 xs :: chunk7 (List.drop 7 xs)



-- VIEW -----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ Html.map SyncMsg (Sync.view model.sync)
        , div [ class "app--under-bar" ]
            [ header [ class "toolbar" ]
                [ span [ class "toolbar__title" ] [ text "Calendar" ]
                , span [ class "cal-month" ]
                    [ text (monthName model.month ++ " " ++ String.fromInt model.year) ]
                , div [ class "cal-nav" ]
                    [ button [ class "btn btn--ghost", onClick PrevMonth ] [ text "‹ Prev" ]
                    , button [ class "btn", onClick GoToday ] [ text "Today" ]
                    , button [ class "btn btn--ghost", onClick NextMonth ] [ text "Next ›" ]
                    ]
                ]
            , section [ class "body" ]
                [ div [ class "cal-layout" ]
                    [ viewGrid model
                    , viewPanel model
                    ]
                ]
            ]
        ]


weekdayHeaders : List String
weekdayHeaders =
    [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]


viewGrid : Model -> Html Msg
viewGrid model =
    let
        weeks =
            chunk7 (monthCells model.year model.month)
    in
    div [ class "cal-grid-wrap" ]
        [ div [ class "cal-weekdays" ]
            (List.map (\w -> div [ class "cal-weekday" ] [ text w ]) weekdayHeaders)
        , div [ class "cal-grid" ]
            (List.map (viewWeek model) weeks)
        ]


viewWeek : Model -> List Cell -> Html Msg
viewWeek model cells =
    div [ class "cal-week" ] (List.map (viewCell model) cells)


viewCell : Model -> Cell -> Html Msg
viewCell model cell =
    let
        dayEvents =
            Maybe.withDefault [] (Dict.get cell.key model.events)
    in
    div
        [ classList
            [ ( "cal-cell", True )
            , ( "is-out", not cell.inMonth )
            , ( "is-today", cell.key == model.today )
            , ( "is-selected", model.selected == Just cell.key )
            ]
        , onClick (SelectDay cell.key)
        ]
        [ div [ class "cal-cell__num" ] [ text (String.fromInt cell.day) ]
        , viewCellEvents model dayEvents
        ]


viewCellEvents : Model -> List Event -> Html Msg
viewCellEvents model evs =
    case evs of
        [] ->
            text ""

        _ ->
            let
                shown =
                    List.take 3 evs

                extra =
                    List.length evs - List.length shown

                pills =
                    List.map (viewPill model) shown

                more =
                    if extra > 0 then
                        [ div [ class "cal-more" ] [ text ("+" ++ String.fromInt extra ++ " more") ] ]

                    else
                        []
            in
            div [ class "cal-cell__events" ] (pills ++ more)


viewPill : Model -> Event -> Html Msg
viewPill model e =
    div
        [ classList
            [ ( "cal-pill", True )
            , ( "cal-pill--shared", not (mine model e) )
            ]
        ]
        [ text (pillLabel e) ]


pillLabel : Event -> String
pillLabel e =
    if e.time == "" then
        e.title

    else
        e.time ++ " " ++ e.title


viewPanel : Model -> Html Msg
viewPanel model =
    case model.selected of
        Nothing ->
            div [ class "cal-panel cal-panel--empty" ]
                [ div [ class "muted" ] [ text "Select a day to view and add events." ] ]

        Just key ->
            let
                dayEvents =
                    Maybe.withDefault [] (Dict.get key model.events)
            in
            div [ class "cal-panel" ]
                [ div [ class "cal-panel__head" ]
                    [ span [ class "cal-panel__title" ] [ text (prettyDate key) ]
                    , button
                        [ class "btn btn--ghost btn--sm", onClick ClosePanel ]
                        [ text "Close" ]
                    ]
                , if List.isEmpty dayEvents then
                    div [ class "cal-panel__empty muted" ] [ text "No events yet." ]

                  else
                    ul [ class "cal-events" ]
                        (List.map (viewEventRow model key) dayEvents)
                , form [ class "cal-add", onSubmit AddEvent ]
                    [ input
                        [ class "cal-add__title"
                        , placeholder "New event title"
                        , value model.draftTitle
                        , onInput SetDraftTitle
                        ]
                        []
                    , input
                        [ class "cal-add__time"
                        , type_ "time"
                        , value model.draftTime
                        , onInput SetDraftTime
                        ]
                        []
                    , button
                        [ class "btn btn--primary"
                        , type_ "submit"
                        , disabled (String.trim model.draftTitle == "")
                        ]
                        [ text "Add" ]
                    ]
                ]


viewEventRow : Model -> String -> Event -> Html Msg
viewEventRow model key e =
    li [ class "cal-event" ]
        [ span [ class "cal-event__time" ]
            [ text
                (if e.time == "" then
                    "—"

                 else
                    e.time
                )
            ]
        , span [ class "cal-event__title" ]
            [ text e.title
            , if mine model e then
                text ""

              else
                span [ class "badge badge--shared" ] [ text "shared" ]
            ]
        , if mine model e then
            eventActions model key e

          else
            span [ class "muted cal-event__owner" ] [ text ("by " ++ String.left 6 e.owner) ]
        ]


eventActions : Model -> String -> Event -> Html Msg
eventActions model key e =
    if model.shareFor == Just e.id then
        div [ class "share-form" ]
            [ input [ class "sync-in", placeholder "share with login", value model.shareLogin, onInput SetShareLogin ] []
            , button [ class "btn btn--primary btn--sm", onClick (DoShare e.id) ] [ text "Share" ]
            , button [ class "btn btn--ghost btn--sm", onClick (OpenShare Nothing) ] [ text "×" ]
            ]

    else
        span [ class "cal-event__actions" ]
            [ if Sync.cloudOn model.sync then
                button [ class "btn btn--sm cal-event__share", onClick (OpenShare (Just e.id)) ] [ text "Share" ]

              else
                text ""
            , button
                [ class "btn btn--danger btn--sm cal-event__del"
                , onClick (DeleteEvent key e.id)
                ]
                [ text "Delete" ]
            ]


prettyDate : String -> String
prettyDate key =
    case parseDate key of
        Just ( y, m, d ) ->
            let
                wd =
                    weekdayLong (dayOfWeek y m d)
            in
            wd ++ ", " ++ monthName m ++ " " ++ String.fromInt d ++ ", " ++ String.fromInt y

        Nothing ->
            key


weekdayLong : Int -> String
weekdayLong dow =
    case dow of
        0 ->
            "Sunday"

        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "Wednesday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        _ ->
            "Saturday"



-- CODEC ----------------------------------------------------------------------


encode : Model -> E.Value
encode model =
    E.object
        [ ( "events", E.dict identity (E.list encodeEvent) model.events )
        , ( "seq", E.int model.seq )
        , ( "sync", Sync.encode model.sync )
        ]


encodeEvent : Event -> E.Value
encodeEvent e =
    E.object
        [ ( "id", E.string e.id )
        , ( "title", E.string e.title )
        , ( "time", E.string e.time )
        , ( "owner", E.string e.owner )
        ]


decoder : Env -> D.Decoder Model
decoder env =
    let
        ( y, m ) =
            defaultYearMonth env.today
    in
    D.map3
        (\events seq sync ->
            { events = events
            , year = y
            , month = m
            , selected = Nothing
            , seq = seq
            , today = env.today
            , draftTitle = ""
            , draftTime = ""
            , sync = sync
            , shareFor = Nothing
            , shareLogin = ""
            }
        )
        (D.oneOf [ D.field "events" (D.dict (D.list eventDecoder)), D.succeed Dict.empty ])
        (D.oneOf [ D.field "seq" D.int, D.field "nextId" D.int, D.succeed 1 ])
        (D.oneOf [ D.field "sync" (Sync.decoder config env.newId), D.succeed (Sync.init config env.newId) ])


eventDecoder : D.Decoder Event
eventDecoder =
    D.map4 Event
        (D.field "id" idDecoder)
        (D.field "title" D.string)
        (D.oneOf [ D.field "time" D.string, D.succeed "" ])
        (D.oneOf [ D.field "owner" D.string, D.succeed "" ])


{-| Old files stored integer event ids; accept both so existing events still load. -}
idDecoder : D.Decoder String
idDecoder =
    D.oneOf [ D.string, D.map String.fromInt D.int ]
