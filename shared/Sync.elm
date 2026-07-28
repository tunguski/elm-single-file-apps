module Sync exposing
    ( Model, Msg, Doc, Config, Out(..)
    , init, update, view
    , encode, decoder
    , cloudOn, principalHeader
    , pull, push, remove, share, search
    )

{-| **Sync** — the reusable client half of the [`Backend`](Backend) platform. It owns the "account
bar" (server URL, register / login / logout, a cloud-sync toggle) and the identity that is stored
_in the app's file_: a per-file uuid, plus a session token once the user logs in. When logged in the
uuid is not sent — requests carry `Authorization: Bearer <token>`; otherwise `X-User-Id: <uuid>`.

An app embeds `Model` in its model, maps `Msg`, and calls the HTTP helpers ([`pull`](#pull) /
[`push`](#push) / …) with its own message constructors to move its elements to and from the server as
[`Doc`](#Doc)s. `update` returns an [`Out`](#Out) telling the app when to refresh (after connecting or
a manual sync).

-}

import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E



-- MODEL


{-| Static per-app configuration. `defaultBase` is the backend origin used until the user overrides it
(e.g. `"https://notes.matsuo.pl"`); when the app is served by that same backend it resolves to the
same origin.
-}
type alias Config =
    { defaultBase : String }


type alias Model =
    { base : String -- backend base URL (no trailing slash)
    , uid : String -- this file's identity
    , token : Maybe String -- session token when logged in
    , login : Maybe String -- current login name
    , cloud : Bool -- is cloud sync enabled?
    , formOpen : Bool
    , fLogin : String
    , fPassword : String
    , status : String
    }


{-| What the app should do after handling a `Msg`. `Refresh` means "the connection/identity changed —
`pull` now". `Pushed`/`Removed`/`Shared` echo completed writes if the app wants to react.
-}
type Out
    = Idle
    | Refresh
    | Failed String


init : Config -> String -> Model
init config uid =
    { base = trimSlash config.defaultBase
    , uid = uid
    , token = Nothing
    , login = Nothing
    , cloud = False
    , formOpen = False
    , fLogin = ""
    , fPassword = ""
    , status = ""
    }


{-| Is cloud sync enabled (so the app should push local changes)? -}
cloudOn : Model -> Bool
cloudOn model =
    model.cloud



-- MSG / UPDATE


type Msg
    = SetBase String
    | ToggleCloud
    | OpenForm Bool
    | SetLogin String
    | SetPassword String
    | DoRegister
    | DoLogin
    | DoLogout
    | GotAuth (Result Http.Error Auth)
    | GotLogout (Result Http.Error ())
    | ManualSync


type alias Auth =
    { token : String, uid : String }


update : Msg -> Model -> ( Model, Cmd Msg, Out )
update msg model =
    case msg of
        SetBase b ->
            ( { model | base = trimSlash b }, Cmd.none, Idle )

        ToggleCloud ->
            let
                on =
                    not model.cloud
            in
            ( { model | cloud = on }
            , Cmd.none
            , if on then
                Refresh

              else
                Idle
            )

        OpenForm open ->
            ( { model | formOpen = open, status = "" }, Cmd.none, Idle )

        SetLogin s ->
            ( { model | fLogin = s }, Cmd.none, Idle )

        SetPassword s ->
            ( { model | fPassword = s }, Cmd.none, Idle )

        DoRegister ->
            ( { model | status = "…" }, authRequest model "register" GotAuth, Idle )

        DoLogin ->
            ( { model | status = "…" }, authRequest model "login" GotAuth, Idle )

        DoLogout ->
            ( { model | token = Nothing, login = Nothing, status = "" }
            , logoutRequest model GotLogout
            , Idle
            )

        GotLogout _ ->
            ( model, Cmd.none, Idle )

        GotAuth (Ok auth) ->
            ( { model
                | token = Just auth.token
                , login = Just model.fLogin
                , uid = auth.uid
                , cloud = True
                , formOpen = False
                , fPassword = ""
                , status = ""
              }
            , Cmd.none
            , Refresh
            )

        GotAuth (Err e) ->
            ( { model | status = httpError e }, Cmd.none, Failed (httpError e) )

        ManualSync ->
            ( model, Cmd.none, Refresh )



-- HTTP HELPERS (called by the app with its own msg)


{-| A document as it crosses the wire. `body` is the app-specific payload (a JSON value); `title` and
`visibility` (`"private"`/`"public"`) are the backend's shared columns.
-}
type alias Doc =
    { id : String
    , title : String
    , visibility : String
    , owner : String
    , body : E.Value
    }


{-| The auth header the app should include on ITS OWN backend calls made outside Sync (rarely needed —
the helpers below already add it). -}
principalHeader : Model -> Http.Header
principalHeader model =
    case model.token of
        Just t ->
            Http.header "Authorization" ("Bearer " ++ t)

        Nothing ->
            Http.header "X-User-Id" model.uid


{-| GET the caller's documents (owned + shared). -}
pull : Model -> (Result Http.Error (List Doc) -> msg) -> Cmd msg
pull model toMsg =
    req model "GET" "/api/docs" Http.emptyBody (Http.expectJson toMsg (D.list docDecoder))


{-| PUT (create/update) one document. -}
push : Model -> Doc -> (Result Http.Error () -> msg) -> Cmd msg
push model doc toMsg =
    req model "PUT" ("/api/docs/" ++ doc.id) (Http.jsonBody (encodeDocBody doc)) (Http.expectWhatever toMsg)


{-| DELETE one document (owner only, server-enforced). -}
remove : Model -> String -> (Result Http.Error () -> msg) -> Cmd msg
remove model id toMsg =
    req model "DELETE" ("/api/docs/" ++ id) Http.emptyBody (Http.expectWhatever toMsg)


{-| Share a document with another user by login. -}
share : Model -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
share model id login toMsg =
    req model
        "POST"
        ("/api/docs/" ++ id ++ "/share")
        (Http.jsonBody (E.object [ ( "login", E.string login ), ( "access", E.string "read" ) ]))
        (Http.expectWhatever toMsg)


{-| Search public documents (across all users) by title. -}
search : Model -> String -> (Result Http.Error (List Doc) -> msg) -> Cmd msg
search model q toMsg =
    req model "GET" ("/api/search?q=" ++ percent q) Http.emptyBody (Http.expectJson toMsg (D.list docDecoder))


req : Model -> String -> String -> Http.Body -> Http.Expect msg -> Cmd msg
req model method path body expect =
    Http.request
        { method = method
        , headers = [ principalHeader model ]
        , url = model.base ++ path
        , body = body
        , expect = expect
        , timeout = Just 15000
        , tracker = Nothing
        }


authRequest : Model -> String -> (Result Http.Error Auth -> msg) -> Cmd msg
authRequest model kind toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-User-Id" model.uid ]
        , url = model.base ++ "/api/auth/" ++ kind
        , body = E.object [ ( "login", E.string model.fLogin ), ( "password", E.string model.fPassword ) ] |> Http.jsonBody
        , expect = Http.expectJson toMsg authDecoder
        , timeout = Just 15000
        , tracker = Nothing
        }


logoutRequest : Model -> (Result Http.Error () -> msg) -> Cmd msg
logoutRequest model toMsg =
    req model "POST" "/api/auth/logout" Http.emptyBody (Http.expectWhatever toMsg)



-- CODECS


authDecoder : D.Decoder Auth
authDecoder =
    D.map2 Auth (D.field "token" D.string) (D.field "uid" D.string)


docDecoder : D.Decoder Doc
docDecoder =
    D.map5 Doc
        (D.field "id" D.string)
        (D.oneOf [ D.field "title" D.string, D.succeed "" ])
        (D.oneOf [ D.field "visibility" D.string, D.succeed "private" ])
        (D.oneOf [ D.field "owner" D.string, D.succeed "" ])
        (D.oneOf [ D.field "body" D.value, D.succeed E.null ])


encodeDocBody : Doc -> E.Value
encodeDocBody doc =
    E.object
        [ ( "title", E.string doc.title )
        , ( "visibility", E.string doc.visibility )
        , ( "body", doc.body )
        ]


{-| Persist the account state in the app's file (so the session survives Ctrl+S). -}
encode : Model -> E.Value
encode model =
    E.object
        [ ( "base", E.string model.base )
        , ( "uid", E.string model.uid )
        , ( "token", maybe E.string model.token )
        , ( "login", maybe E.string model.login )
        , ( "cloud", E.bool model.cloud )
        ]


{-| Restore the account state; `fallbackUid` (from `App.Env.newId`) is used when the file has none. -}
decoder : Config -> String -> D.Decoder Model
decoder config fallbackUid =
    D.map5
        (\base uid token login cloud ->
            { base = trimSlash base
            , uid =
                if uid == "" then
                    fallbackUid

                else
                    uid
            , token = token
            , login = login
            , cloud = cloud
            , formOpen = False
            , fLogin = Maybe.withDefault "" login
            , fPassword = ""
            , status = ""
            }
        )
        (D.oneOf [ D.field "base" D.string, D.succeed config.defaultBase ])
        (D.oneOf [ D.field "uid" D.string, D.succeed "" ])
        (D.oneOf [ D.field "token" (D.nullable D.string), D.succeed Nothing ])
        (D.oneOf [ D.field "login" (D.nullable D.string), D.succeed Nothing ])
        (D.oneOf [ D.field "cloud" D.bool, D.succeed False ])



-- VIEW  (the account bar)


view : Model -> Html Msg
view model =
    div [ class "sync-bar" ]
        [ label [ class "sync-toggle" ]
            [ input [ type_ "checkbox", Html.Attributes.checked model.cloud, onClick ToggleCloud ] []
            , span [] [ text "Cloud" ]
            ]
        , input
            [ class "sync-url"
            , placeholder "backend URL"
            , value model.base
            , onInput SetBase
            ]
            []
        , identityView model
        , if model.formOpen then
            formView model

          else
            text ""
        , if model.status == "" then
            text ""

          else
            span [ class "sync-status" ] [ text model.status ]
        ]


identityView : Model -> Html Msg
identityView model =
    case model.login of
        Just lg ->
            span [ class "sync-id" ]
                [ span [ class "sync-user" ] [ text ("@" ++ lg) ]
                , button [ class "btn btn--ghost", onClick DoLogout ] [ text "Log out" ]
                ]

        Nothing ->
            span [ class "sync-id" ]
                [ span [ class "sync-anon", Html.Attributes.title model.uid ] [ text ("guest " ++ String.left 6 model.uid) ]
                , button [ class "btn btn--ghost", onClick (OpenForm (not model.formOpen)) ] [ text "Log in / Register" ]
                ]


formView : Model -> Html Msg
formView model =
    div [ class "sync-form" ]
        [ input [ class "sync-in", placeholder "login", value model.fLogin, onInput SetLogin ] []
        , input [ class "sync-in", type_ "password", placeholder "password", value model.fPassword, onInput SetPassword ] []
        , button [ class "btn btn--primary", onClick DoLogin ] [ text "Log in" ]
        , button [ class "btn", onClick DoRegister ] [ text "Register" ]
        , button [ class "btn btn--ghost", onClick (OpenForm False) ] [ text "×" ]
        ]



-- HELPERS


trimSlash : String -> String
trimSlash s =
    if String.endsWith "/" s then
        String.dropRight 1 s

    else
        s


maybe : (a -> E.Value) -> Maybe a -> E.Value
maybe f m =
    case m of
        Just v ->
            f v

        Nothing ->
            E.null


percent : String -> String
percent s =
    -- Minimal query escaping for the search term (spaces + a few reserved chars).
    s
        |> String.replace "%" "%25"
        |> String.replace " " "%20"
        |> String.replace "&" "%26"
        |> String.replace "#" "%23"
        |> String.replace "+" "%2B"


httpError : Http.Error -> String
httpError e =
    case e of
        Http.BadStatus 401 ->
            "invalid login or password"

        Http.BadStatus 409 ->
            "that login is taken"

        Http.BadStatus code ->
            "error " ++ String.fromInt code

        Http.NetworkError ->
            "can't reach the server"

        Http.Timeout ->
            "timed out"

        Http.BadBody _ ->
            "bad response"

        Http.BadUrl _ ->
            "bad URL"
