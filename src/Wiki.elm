module Wiki exposing (main)

{-| **Wiki** — a wiki of interlinked plain-text pages. Any page's body can link to another with
`[[Some Page]]`. Pages live in the file itself (Ctrl+S), and can optionally sync to a
[`Backend`](Backend) server via [`Sync`](Sync).

Two namespaces:

  - **Personal** — your own pages (private) plus pages shared with you. Owned pages are full CRUD and,
    with **Cloud** on, are pushed to the server (and deleted from it). Others' pages are read-only.
  - **Global** — public pages from every user. Search the server (`Sync.search`) and browse the
    results; the client keeps a capped cache of the last few global pages you opened (persisted in the
    file). Global pages are read-only.

Owned pages can be **published** (visibility → public, which lists them in Global) and **shared** with
a specific user by login. See [`App`](App).
-}

import App exposing (Env)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h2, h3, header, input, li, span, text, textarea, ul)
import Html.Attributes exposing (class, classList, disabled, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
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
    { defaultBase = "https://wiki.matsuo.pl" }


globalCacheLimit : Int
globalCacheLimit =
    20



-- MODEL ----------------------------------------------------------------------


type alias Page =
    { id : String
    , title : String
    , body : String
    , owner : String -- backend owner uuid; "" for a local-only page
    , visibility : String -- "private" | "public"
    }


type Namespace
    = Personal
    | Global


type alias Model =
    { pages : List Page -- personal pages (owned + shared with me)
    , globalCache : List Page -- capped cache of recently opened public pages
    , globalResults : List Page -- last Sync.search results (not persisted)
    , namespace : Namespace
    , current : Maybe String -- id of the open page
    , editing : Bool
    , query : String
    , seq : Int
    , sync : Sync.Model
    , shareFor : Maybe String
    , shareLogin : String
    }


init : Env -> ( Model, Cmd Msg )
init env =
    let
        sync =
            Sync.init config env.newId

        home =
            defaultHome sync.uid
    in
    ( { pages = [ home ]
      , globalCache = []
      , globalResults = []
      , namespace = Personal
      , current = Just home.id
      , editing = False
      , query = ""
      , seq = 2
      , sync = sync
      , shareFor = Nothing
      , shareLogin = ""
      }
    , Cmd.none
    )


defaultHome : String -> Page
defaultHome uid =
    { id = uid ++ "-1", title = "Home", body = homeBody, owner = uid, visibility = "private" }


homeBody : String
homeBody =
    String.join "\n"
        [ "# Welcome"
        , ""
        , "This is your wiki. Every page can link to another using double brackets,"
        , "like [[Ideas]] or [[Reading list]]."
        , ""
        , "Click a link to open that page — a missing page is created empty the first"
        , "time you visit it. Hit Edit to write, Done to save."
        , ""
        , "Turn on Cloud to sync your pages, publish a page to the Global namespace, or"
        , "share it with another user."
        ]


isOwn : Model -> Page -> Bool
isOwn model page =
    page.owner == "" || page.owner == model.sync.uid


canEdit : Model -> Page -> Bool
canEdit model page =
    isOwn model page


knownPages : Model -> List Page
knownPages model =
    model.pages ++ model.globalCache ++ model.globalResults


findById : String -> Model -> Maybe Page
findById id model =
    List.head (List.filter (\p -> p.id == id) (knownPages model))


selectedPage : Model -> Maybe Page
selectedPage model =
    Maybe.andThen (\id -> findById id model) model.current



-- UPDATE ---------------------------------------------------------------------


type Msg
    = Navigate String
    | Select String
    | OpenGlobal Page
    | New
    | SetQuery String
    | SetBody String
    | SetTitle String
    | ToggleEdit
    | Delete
    | SetNamespace Namespace
    | DoSearch
    | TogglePublish
    | SyncMsg Sync.Msg
    | GotDocs (Result Http.Error (List Sync.Doc))
    | GotGlobal (Result Http.Error (List Sync.Doc))
    | Pushed (Result Http.Error ())
    | OpenShare (Maybe String)
    | SetShareLogin String
    | DoShare String
    | Shared (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate raw ->
            let
                title =
                    String.trim raw
            in
            case findByTitle model.pages title of
                Just p ->
                    ( { model | current = Just p.id, namespace = Personal, editing = False }, Cmd.none )

                Nothing ->
                    case findByTitle model.globalCache title of
                        Just p ->
                            ( { model
                                | current = Just p.id
                                , namespace = Global
                                , editing = False
                                , globalCache = cacheOpen p model.globalCache
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            createPage title model

        Select id ->
            ( { model | current = Just id, editing = False, shareFor = Nothing }, Cmd.none )

        OpenGlobal page ->
            ( { model
                | current = Just page.id
                , namespace = Global
                , editing = False
                , shareFor = Nothing
                , globalCache = cacheOpen page model.globalCache
              }
            , Cmd.none
            )

        New ->
            createNamed (uniqueTitle model.pages "New page") { model | editing = True, query = "" }

        SetQuery q ->
            ( { model | query = q }, Cmd.none )

        SetBody b ->
            editSelected model (\p -> { p | body = b })

        SetTitle t ->
            editSelected model (\p -> { p | title = t })

        ToggleEdit ->
            ( { model | editing = not model.editing }, Cmd.none )

        TogglePublish ->
            editSelected model
                (\p ->
                    { p
                        | visibility =
                            if p.visibility == "public" then
                                "private"

                            else
                                "public"
                    }
                )

        Delete ->
            case model.current of
                Nothing ->
                    ( model, Cmd.none )

                Just id ->
                    let
                        pages =
                            List.filter (\p -> p.id /= id) model.pages
                    in
                    ( { model
                        | pages = pages
                        , current = Maybe.map .id (List.head pages)
                        , editing = False
                      }
                    , removeIfCloud model id
                    )

        SetNamespace ns ->
            let
                cur =
                    case ns of
                        Personal ->
                            Maybe.map .id (List.head model.pages)

                        Global ->
                            Maybe.map .id (List.head (globalList model))
            in
            ( { model | namespace = ns, editing = False, current = cur, shareFor = Nothing }, Cmd.none )

        DoSearch ->
            ( model, Sync.search model.sync model.query GotGlobal )

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

        GotGlobal (Ok docs) ->
            ( { model | globalResults = List.filterMap docToPage docs }, Cmd.none )

        GotGlobal (Err _) ->
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


{-| Create a page with the given exact title (used by New; a fresh id is minted). -}
createNamed : String -> Model -> ( Model, Cmd Msg )
createNamed title model =
    let
        id =
            model.sync.uid ++ "-" ++ String.fromInt model.seq

        page =
            { id = id, title = title, body = "", owner = model.sync.uid, visibility = "private" }
    in
    ( { model
        | pages = page :: model.pages
        , current = Just id
        , seq = model.seq + 1
        , namespace = Personal
      }
    , pushIfCloud model page
    )


{-| Create a personal page for a followed [[link]] that resolved nowhere. -}
createPage : String -> Model -> ( Model, Cmd Msg )
createPage title model =
    createNamed title { model | editing = False }


editSelected : Model -> (Page -> Page) -> ( Model, Cmd Msg )
editSelected model f =
    case model.current of
        Nothing ->
            ( model, Cmd.none )

        Just id ->
            let
                pages =
                    List.map
                        (\p ->
                            if p.id == id then
                                f p

                            else
                                p
                        )
                        model.pages

                changed =
                    List.filter (\p -> p.id == id) pages
            in
            ( { model | pages = pages }
            , case changed of
                p :: _ ->
                    if isOwn model p then
                        pushIfCloud model p

                    else
                        Cmd.none

                [] ->
                    Cmd.none
            )


cacheOpen : Page -> List Page -> List Page
cacheOpen page cache =
    (page :: List.filter (\p -> p.id /= page.id) cache)
        |> List.take globalCacheLimit


pushIfCloud : Model -> Page -> Cmd Msg
pushIfCloud model page =
    if Sync.cloudOn model.sync then
        Sync.push model.sync (pageToDoc page) Pushed

    else
        Cmd.none


removeIfCloud : Model -> String -> Cmd Msg
removeIfCloud model id =
    if Sync.cloudOn model.sync then
        Sync.remove model.sync id Shared

    else
        Cmd.none


pageToDoc : Page -> Sync.Doc
pageToDoc page =
    { id = page.id
    , title = page.title
    , visibility = page.visibility
    , owner = page.owner
    , body = E.object [ ( "title", E.string page.title ), ( "body", E.string page.body ) ]
    }


docToPage : Sync.Doc -> Maybe Page
docToPage doc =
    case D.decodeValue bodyDecoder doc.body of
        Ok ( t, b ) ->
            Just
                { id = doc.id
                , title =
                    if String.trim t == "" then
                        doc.title

                    else
                        t
                , body = b
                , owner = doc.owner
                , visibility = doc.visibility
                }

        Err _ ->
            Nothing


bodyDecoder : D.Decoder ( String, String )
bodyDecoder =
    D.map2 Tuple.pair
        (D.oneOf [ D.field "title" D.string, D.succeed "" ])
        (D.oneOf [ D.field "body" D.string, D.succeed "" ])


mergeDocs : List Sync.Doc -> Model -> Model
mergeDocs docs model =
    let
        remote =
            List.filterMap docToPage docs

        remoteIds =
            Set.fromList (List.map .id remote)

        localOnly =
            List.filter (\p -> not (Set.member p.id remoteIds)) model.pages
    in
    { model | pages = remote ++ localOnly }


findByTitle : List Page -> String -> Maybe Page
findByTitle pages title =
    List.head (List.filter (\p -> p.title == title) pages)


globalList : Model -> List Page
globalList model =
    dedupById (model.globalResults ++ model.globalCache)


dedupById : List Page -> List Page
dedupById pages =
    let
        step page ( seen, acc ) =
            if Set.member page.id seen then
                ( seen, acc )

            else
                ( Set.insert page.id seen, page :: acc )
    in
    List.foldl step ( Set.empty, [] ) pages
        |> Tuple.second
        |> List.reverse


uniqueTitle : List Page -> String -> String
uniqueTitle pages base =
    let
        taken t =
            List.any (\p -> p.title == t) pages
    in
    if not (taken base) then
        base

    else
        let
            go n =
                let
                    candidate =
                        base ++ " " ++ String.fromInt n
                in
                if taken candidate then
                    go (n + 1)

                else
                    candidate
        in
        go 2



-- VIEW -----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ Html.map SyncMsg (Sync.view model.sync)
        , div [ class "app--split app--under-bar" ]
            [ div [ class "pane pane--list" ]
                [ nsSwitch model
                , header [ class "pane__head" ]
                    [ input
                        [ class "search"
                        , placeholder (searchPlaceholder model.namespace)
                        , value model.query
                        , onInput SetQuery
                        ]
                        []
                    , actionButton model
                    ]
                , ul [ class "list" ] (listRows model)
                ]
            , div [ class "pane pane--main" ] [ mainPane model ]
            ]
        ]


nsSwitch : Model -> Html Msg
nsSwitch model =
    div [ class "wiki-ns" ]
        [ button
            [ classList [ ( "btn", True ), ( "btn--primary", model.namespace == Personal ) ]
            , onClick (SetNamespace Personal)
            ]
            [ text "Personal" ]
        , button
            [ classList [ ( "btn", True ), ( "btn--primary", model.namespace == Global ) ]
            , onClick (SetNamespace Global)
            ]
            [ text "Global" ]
        ]


searchPlaceholder : Namespace -> String
searchPlaceholder ns =
    case ns of
        Personal ->
            "Search pages…"

        Global ->
            "Search public pages…"


actionButton : Model -> Html Msg
actionButton model =
    case model.namespace of
        Personal ->
            button [ class "btn btn--primary", onClick New ] [ text "+ New page" ]

        Global ->
            button [ class "btn btn--primary", onClick DoSearch ] [ text "Search" ]


listRows : Model -> List (Html Msg)
listRows model =
    case model.namespace of
        Personal ->
            model.pages
                |> List.filter (\p -> matchesQuery model.query p.title)
                |> List.map (personalRow model)

        Global ->
            globalList model
                |> List.filter (\p -> matchesQuery model.query p.title)
                |> List.map (globalRow model)


matchesQuery : String -> String -> Bool
matchesQuery q title =
    let
        needle =
            String.toLower (String.trim q)
    in
    needle == "" || String.contains needle (String.toLower title)


personalRow : Model -> Page -> Html Msg
personalRow model page =
    pageRow model page (Select page.id) (personalBadges model page)


globalRow : Model -> Page -> Html Msg
globalRow model page =
    pageRow model page (OpenGlobal page) [ publicBadge ]


pageRow : Model -> Page -> Msg -> List (Html Msg) -> Html Msg
pageRow model page msg badges =
    li
        [ classList [ ( "list__item", True ), ( "is-active", model.current == Just page.id ) ]
        , onClick msg
        ]
        [ span [ class "list__title" ] (text (nonEmpty page.title "Untitled") :: badges)
        , span [ class "list__sub" ] [ text (preview page.body) ]
        ]


personalBadges : Model -> Page -> List (Html Msg)
personalBadges model page =
    (if page.visibility == "public" then
        [ publicBadge ]

     else
        []
    )
        ++ (if isOwn model page then
                []

            else
                [ span [ class "badge badge--shared" ] [ text "shared" ] ]
           )


publicBadge : Html Msg
publicBadge =
    span [ class "badge badge--public" ] [ text "public" ]


preview : String -> String
preview body =
    let
        flat =
            String.join " " (String.words (stripMarkup body))
    in
    if String.trim flat == "" then
        "Empty page"

    else if String.length flat > 60 then
        String.left 60 flat ++ "…"

    else
        flat


{-| Strip the `#` heading marker and `[[ ]]` brackets so list previews read as plain prose. -}
stripMarkup : String -> String
stripMarkup body =
    body
        |> String.replace "[[" ""
        |> String.replace "]]" ""
        |> String.replace "# " ""


mainPane : Model -> Html Msg
mainPane model =
    case selectedPage model of
        Nothing ->
            div [ class "empty" ]
                [ text (emptyHint model.namespace) ]

        Just page ->
            if model.editing && canEdit model page then
                editor model page

            else
                reader model page


emptyHint : Namespace -> String
emptyHint ns =
    case ns of
        Personal ->
            "Select a page, or create one with + New page."

        Global ->
            "Search for public pages, then open one to read it."


reader : Model -> Page -> Html Msg
reader model page =
    div [ class "editor" ]
        [ div [ class "wiki-head" ]
            [ h2 [ class "wiki-title" ] [ text (nonEmpty page.title "Untitled") ]
            , metaBadge page
            , div [ class "wiki-actions" ] (readerActions model page)
            ]
        , div [ class "editor__body wiki-doc" ] (viewBody model page.body)
        ]


metaBadge : Page -> Html Msg
metaBadge page =
    if page.visibility == "public" then
        publicBadge

    else
        text ""


readerActions : Model -> Page -> List (Html Msg)
readerActions model page =
    if canEdit model page then
        [ button [ class "btn", onClick ToggleEdit ] [ text "Edit" ]
        , publishControl model page
        , shareControl model page
        ]

    else
        [ span [ class "muted" ] [ text ("read only — by " ++ String.left 6 page.owner) ] ]


publishControl : Model -> Page -> Html Msg
publishControl model page =
    if not (Sync.cloudOn model.sync) then
        text ""

    else if page.visibility == "public" then
        button [ class "btn", onClick TogglePublish ] [ text "Unpublish" ]

    else
        button [ class "btn", onClick TogglePublish ] [ text "Publish" ]


shareControl : Model -> Page -> Html Msg
shareControl model page =
    if not (Sync.cloudOn model.sync) then
        text ""

    else if model.shareFor == Just page.id then
        div [ class "share-form" ]
            [ input [ class "sync-in", placeholder "share with login", value model.shareLogin, onInput SetShareLogin ] []
            , button [ class "btn btn--primary", onClick (DoShare page.id) ] [ text "Share" ]
            , button [ class "btn btn--ghost", onClick (OpenShare Nothing) ] [ text "×" ]
            ]

    else
        button [ class "btn", onClick (OpenShare (Just page.id)) ] [ text "Share" ]


editor : Model -> Page -> Html Msg
editor _ page =
    div [ class "editor" ]
        [ input
            [ class "editor__title"
            , value page.title
            , placeholder "Page title"
            , onInput SetTitle
            ]
            []
        , textarea
            [ class "editor__body wiki-source"
            , value page.body
            , placeholder "Write here… link to another page with [[Its Title]]."
            , onInput SetBody
            ]
            []
        , div [ class "editor__foot" ]
            [ button [ class "btn btn--danger", onClick Delete ] [ text "Delete" ]
            , button [ class "btn btn--primary", onClick ToggleEdit ] [ text "Done" ]
            ]
        ]


nonEmpty : String -> String -> String
nonEmpty s fallback =
    if String.trim s == "" then
        fallback

    else
        s



-- RENDERING WIKI BODY --------------------------------------------------------


viewBody : Model -> String -> List (Html Msg)
viewBody model body =
    let
        known =
            Set.fromList (List.map .title (knownPages model))
    in
    List.map (viewLine known) (String.split "\n" body)


viewLine : Set String -> String -> Html Msg
viewLine known line =
    if String.startsWith "# " line then
        h2 [ class "wiki-h1" ] (inline known (String.dropLeft 2 line))

    else if String.startsWith "## " line then
        h3 [ class "wiki-h2" ] (inline known (String.dropLeft 3 line))

    else if String.trim line == "" then
        div [ class "wiki-blank" ] []

    else
        div [ class "wiki-line" ] (inline known line)


inline : Set String -> String -> List (Html Msg)
inline known line =
    List.map (viewSegment known) (parseSegments line)


type Segment
    = Txt String
    | Lnk String


{-| Split a line into plain-text runs and `[[wiki links]]`. An unterminated `[[` is left as text. -}
parseSegments : String -> List Segment
parseSegments str =
    case String.indexes "[[" str of
        [] ->
            keepText str

        open :: _ ->
            let
                before =
                    String.left open str

                rest =
                    String.dropLeft (open + 2) str
            in
            case String.indexes "]]" rest of
                [] ->
                    keepText str

                close :: _ ->
                    let
                        target =
                            String.trim (String.left close rest)

                        after =
                            String.dropLeft (close + 2) rest
                    in
                    keepText before ++ [ Lnk target ] ++ parseSegments after


keepText : String -> List Segment
keepText s =
    if s == "" then
        []

    else
        [ Txt s ]


viewSegment : Set String -> Segment -> Html Msg
viewSegment known seg =
    case seg of
        Txt t ->
            text t

        Lnk target ->
            a
                [ classList [ ( "wiki-link", True ), ( "wiki-link--new", not (Set.member target known) ) ]
                , onClick (Navigate target)
                ]
                [ text target ]



-- CODEC ----------------------------------------------------------------------


encode : Model -> E.Value
encode model =
    E.object
        [ ( "pages", E.list encodePage model.pages )
        , ( "globalCache", E.list encodePage model.globalCache )
        , ( "current", maybe E.string model.current )
        , ( "seq", E.int model.seq )
        , ( "namespace", E.string (nsToString model.namespace) )
        , ( "sync", Sync.encode model.sync )
        ]


encodePage : Page -> E.Value
encodePage page =
    E.object
        [ ( "id", E.string page.id )
        , ( "title", E.string page.title )
        , ( "body", E.string page.body )
        , ( "owner", E.string page.owner )
        , ( "visibility", E.string page.visibility )
        ]


nsToString : Namespace -> String
nsToString ns =
    case ns of
        Personal ->
            "personal"

        Global ->
            "global"


nsFromString : String -> Namespace
nsFromString s =
    if s == "global" then
        Global

    else
        Personal


maybe : (a -> E.Value) -> Maybe a -> E.Value
maybe f m =
    case m of
        Just v ->
            f v

        Nothing ->
            E.null


decoder : Env -> D.Decoder Model
decoder env =
    D.map6 assemble
        (D.oneOf [ D.field "sync" (Sync.decoder config env.newId), D.succeed (Sync.init config env.newId) ])
        (D.oneOf
            [ D.field "pages" (D.list pageDecoder)
            , D.field "pages" (D.map dictToPages (D.dict D.string)) -- migrate old title→body files
            , D.succeed []
            ]
        )
        (D.oneOf [ D.field "globalCache" (D.list pageDecoder), D.succeed [] ])
        (D.oneOf [ D.field "seq" D.int, D.succeed 1 ])
        (D.oneOf [ D.field "current" D.string, D.succeed "" ])
        (D.oneOf [ D.field "namespace" D.string, D.succeed "personal" ])


assemble : Sync.Model -> List Page -> List Page -> Int -> String -> String -> Model
assemble sync pagesRaw cacheRaw seqRaw currentRaw nsRaw =
    let
        ( assigned, seq1 ) =
            assignIds sync.uid (max seqRaw 1) pagesRaw

        pages =
            if List.isEmpty assigned then
                [ defaultHome sync.uid ]

            else
                assigned

        seqFinal =
            if List.isEmpty assigned then
                max seq1 2

            else
                seq1

        ( cache, _ ) =
            assignIds sync.uid seqFinal cacheRaw

        current =
            resolveCurrent currentRaw pages
    in
    { pages = pages
    , globalCache = cache
    , globalResults = []
    , namespace = nsFromString nsRaw
    , current = current
    , editing = False
    , query = ""
    , seq = seqFinal
    , sync = sync
    , shareFor = Nothing
    , shareLogin = ""
    }


{-| Give any page that lacks an id (a migrated dict entry) a fresh unique id, returning the next
free sequence number. -}
assignIds : String -> Int -> List Page -> ( List Page, Int )
assignIds uid startSeq pages =
    let
        step page ( acc, seq ) =
            if page.id == "" then
                ( { page | id = uid ++ "-" ++ String.fromInt seq } :: acc, seq + 1 )

            else
                ( page :: acc, seq )
    in
    List.foldl step ( [], startSeq ) pages
        |> Tuple.mapFirst List.reverse


{-| The saved `current` may be an id (new files) or a page title (old files); accept either, else
fall back to the first page. -}
resolveCurrent : String -> List Page -> Maybe String
resolveCurrent raw pages =
    if List.any (\p -> p.id == raw) pages then
        Just raw

    else
        case findByTitle pages raw of
            Just p ->
                Just p.id

            Nothing ->
                Maybe.map .id (List.head pages)


pageDecoder : D.Decoder Page
pageDecoder =
    D.map5 Page
        (D.oneOf [ D.field "id" D.string, D.succeed "" ])
        (D.field "title" D.string)
        (D.oneOf [ D.field "body" D.string, D.succeed "" ])
        (D.oneOf [ D.field "owner" D.string, D.succeed "" ])
        (D.oneOf [ D.field "visibility" D.string, D.succeed "private" ])


dictToPages : Dict String String -> List Page
dictToPages pages =
    Dict.toList pages
        |> List.map (\( title, body ) -> { id = "", title = title, body = body, owner = "", visibility = "private" })
