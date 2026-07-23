module Wiki exposing (main)

{-| **Wiki** — a personal wiki of interlinked plain-text pages. Pages are keyed by title; the body
of any page can link to another with `[[Some Page]]`. Clicking a link opens that page, creating it
empty if it doesn't exist yet. All pages live in the file itself; Ctrl+S writes them back.
See [`App`](App).
-}

import App exposing (Env)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h2, h3, header, input, li, span, text, textarea, ul)
import Html.Attributes exposing (class, classList, placeholder, value)
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


type alias Model =
    { pages : Dict String String
    , current : String
    , editing : Bool
    , query : String
    , draftTitle : String
    }


init : Env -> Model
init _ =
    { pages = Dict.singleton "Home" homeBody
    , current = "Home"
    , editing = False
    , query = ""
    , draftTitle = ""
    }


homeBody : String
homeBody =
    String.join "\n"
        [ "# Welcome"
        , ""
        , "This is your personal wiki. Every page can link to another using double"
        , "brackets, like [[Ideas]] or [[Reading list]]."
        , ""
        , "Click a link to open that page — a missing page is created empty the first"
        , "time you visit it. Hit Edit to write, Done to save."
        ]



-- UPDATE ---------------------------------------------------------------------


type Msg
    = Navigate String
    | New
    | SetQuery String
    | SetBody String
    | SetTitle String
    | ToggleEdit
    | Delete


update : Msg -> Model -> Model
update msg model =
    case msg of
        Navigate target ->
            let
                base =
                    if model.editing then
                        commitRename { model | editing = False }

                    else
                        model

                trimmed =
                    String.trim target

                pages =
                    if Dict.member trimmed base.pages then
                        base.pages

                    else
                        Dict.insert trimmed "" base.pages
            in
            { base | pages = pages, current = trimmed, editing = False }

        New ->
            let
                title =
                    uniqueTitle model.pages "New page"
            in
            { model
                | pages = Dict.insert title "" model.pages
                , current = title
                , editing = True
                , draftTitle = title
                , query = ""
            }

        SetQuery q ->
            { model | query = q }

        SetBody b ->
            { model | pages = Dict.insert model.current b model.pages }

        SetTitle t ->
            { model | draftTitle = t }

        ToggleEdit ->
            if model.editing then
                commitRename { model | editing = False }

            else
                { model | editing = True, draftTitle = model.current }

        Delete ->
            let
                pruned =
                    Dict.remove model.current model.pages

                ( pages, current ) =
                    case Dict.keys pruned of
                        first :: _ ->
                            ( pruned, first )

                        [] ->
                            ( Dict.singleton "Home" "", "Home" )
            in
            { model | pages = pages, current = current, editing = False }


{-| Apply a pending title edit: move the current page's body under the new title. Refuses empty
titles, no-op renames, and names that would clobber an existing page.
-}
commitRename : Model -> Model
commitRename model =
    let
        new =
            String.trim model.draftTitle
    in
    if new == "" || new == model.current || Dict.member new model.pages then
        model

    else
        let
            body =
                Maybe.withDefault "" (Dict.get model.current model.pages)

            pages =
                Dict.insert new body (Dict.remove model.current model.pages)
        in
        { model | pages = pages, current = new }


uniqueTitle : Dict String String -> String -> String
uniqueTitle pages base =
    if not (Dict.member base pages) then
        base

    else
        let
            go n =
                let
                    candidate =
                        base ++ " " ++ String.fromInt n
                in
                if Dict.member candidate pages then
                    go (n + 1)

                else
                    candidate
        in
        go 2



-- VIEW -----------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        titles =
            List.filter (matchesQuery model.query) (Dict.keys model.pages)
    in
    div [ class "app app--split" ]
        [ div [ class "pane pane--list" ]
            [ header [ class "pane__head" ]
                [ input
                    [ class "search"
                    , placeholder "Search pages…"
                    , value model.query
                    , onInput SetQuery
                    ]
                    []
                , button [ class "btn btn--primary", onClick New ] [ text "+ New page" ]
                ]
            , ul [ class "list" ] (List.map (pageRow model) titles)
            ]
        , div [ class "pane pane--main" ] [ main_ model ]
        ]


matchesQuery : String -> String -> Bool
matchesQuery q title =
    let
        needle =
            String.toLower (String.trim q)
    in
    needle == "" || String.contains needle (String.toLower title)


pageRow : Model -> String -> Html Msg
pageRow model title =
    li
        [ class "list__item"
        , classList [ ( "is-active", title == model.current ) ]
        , onClick (Navigate title)
        ]
        [ span [ class "list__title" ] [ text title ]
        , span [ class "list__sub" ]
            [ text (preview (Maybe.withDefault "" (Dict.get title model.pages))) ]
        ]


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


{-| Strip the `#` heading marker and `[[ ]]` brackets so list previews read as plain prose.
-}
stripMarkup : String -> String
stripMarkup body =
    body
        |> String.replace "[[" ""
        |> String.replace "]]" ""
        |> String.replace "# " ""


main_ : Model -> Html Msg
main_ model =
    if model.editing then
        editor model

    else
        reader model


reader : Model -> Html Msg
reader model =
    div [ class "editor" ]
        [ div [ class "wiki-head" ]
            [ h2 [ class "wiki-title" ] [ text model.current ]
            , div [ class "wiki-actions" ]
                [ button [ class "btn", onClick ToggleEdit ] [ text "Edit" ] ]
            ]
        , div [ class "editor__body wiki-doc" ]
            (viewBody model.pages (Maybe.withDefault "" (Dict.get model.current model.pages)))
        ]


editor : Model -> Html Msg
editor model =
    div [ class "editor" ]
        [ input
            [ class "editor__title"
            , value model.draftTitle
            , placeholder "Page title"
            , onInput SetTitle
            ]
            []
        , textarea
            [ class "editor__body wiki-source"
            , value (Maybe.withDefault "" (Dict.get model.current model.pages))
            , placeholder "Write here… link to another page with [[Its Title]]."
            , onInput SetBody
            ]
            []
        , div [ class "editor__foot" ]
            [ button [ class "btn btn--danger", onClick Delete ] [ text "Delete" ]
            , button [ class "btn btn--primary", onClick ToggleEdit ] [ text "Done" ]
            ]
        ]



-- RENDERING WIKI BODY --------------------------------------------------------


viewBody : Dict String String -> String -> List (Html Msg)
viewBody pages body =
    List.map (viewLine pages) (String.split "\n" body)


viewLine : Dict String String -> String -> Html Msg
viewLine pages line =
    if String.startsWith "# " line then
        h2 [ class "wiki-h1" ] (inline pages (String.dropLeft 2 line))

    else if String.startsWith "## " line then
        h3 [ class "wiki-h2" ] (inline pages (String.dropLeft 3 line))

    else if String.trim line == "" then
        div [ class "wiki-blank" ] []

    else
        div [ class "wiki-line" ] (inline pages line)


inline : Dict String String -> String -> List (Html Msg)
inline pages line =
    List.map (viewSegment pages) (parseSegments line)


type Segment
    = Txt String
    | Lnk String


{-| Split a line into plain-text runs and `[[wiki links]]`. An unterminated `[[` is left as text.
-}
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


viewSegment : Dict String String -> Segment -> Html Msg
viewSegment pages seg =
    case seg of
        Txt t ->
            text t

        Lnk target ->
            let
                exists =
                    Dict.member target pages
            in
            a
                [ class "wiki-link"
                , classList [ ( "wiki-link--new", not exists ) ]
                , onClick (Navigate target)
                ]
                [ text target ]



-- CODEC ----------------------------------------------------------------------


encode : Model -> E.Value
encode model =
    E.object
        [ ( "pages", E.dict identity E.string model.pages )
        , ( "current", E.string model.current )
        ]


decoder : Env -> D.Decoder Model
decoder _ =
    D.map2
        (\pages rawCurrent ->
            let
                filled =
                    if Dict.isEmpty pages then
                        Dict.singleton "Home" homeBody

                    else
                        pages

                current =
                    if Dict.member rawCurrent filled then
                        rawCurrent

                    else
                        Maybe.withDefault "Home" (List.head (Dict.keys filled))
            in
            { pages = filled
            , current = current
            , editing = False
            , query = ""
            , draftTitle = ""
            }
        )
        (D.oneOf [ D.field "pages" (D.dict D.string), D.succeed Dict.empty ])
        (D.oneOf [ D.field "current" D.string, D.succeed "Home" ])
