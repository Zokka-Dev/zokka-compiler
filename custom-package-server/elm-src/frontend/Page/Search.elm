module Page.Search exposing
  ( Model
  , init
  , Msg
  , update
  , view
  )


import Browser.Navigation as Nav
import Elm.Version as V
import Html exposing (..)
import Html.Attributes exposing (autocomplete, autofocus, class, href, name, placeholder, style, type_, value)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Html.Keyed as Keyed
import Http
import Href
import Json.Decode as Decode
import Page.Search.Entry as Entry exposing (Entry)
import Page.Problem as Problem
import Process
import Session
import Skeleton
import Task
import Url.Builder as Url
import Url
import Utils.LoginUpdate exposing (LoginUpdate(..), httpErrorToLoginUpdate)



-- MODEL


type alias Model =
  { session : Session.Data
  , query : String
  -- FIXME: This is technically bad data modelling; it shouldn't be possible for
  -- the author to be Just ... and the repository to be Nothing
  , repository : Maybe String
  , author : Maybe String
  , entries : Entries
  }


type Entries
  = Failure
  | Loading
  | Success (List Entry)



init : Session.Data -> Maybe String -> Maybe String -> Maybe String -> ( Model, Cmd Msg, LoginUpdate )
init session query repository author =
  case Session.getEntries session of
    Just entries ->
      ( Model session (Maybe.withDefault "" query) repository author (Success entries)
      , Cmd.none
      , NoUpdateAboutLoginStatus
      )

    Nothing ->
      ( Model session (Maybe.withDefault "" query) repository author Loading
      , Http.send GotPackages <|
          Http.get "http://localhost:3000/dashboard/all-data" (Entry.redecodeBackToEntries)
      , NoUpdateAboutLoginStatus
      )



-- UPDATE


type Msg
  = QueryChanged String
  | QuerySubmitted
  | GotPackages (Result Http.Error (List Entry))
  | DelayedUrlChange String


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg, LoginUpdate )
update key msg model =
  case msg of
    QueryChanged query ->
      ( { model | query = query }
      , Process.sleep 300
          |> Task.perform (\_ -> DelayedUrlChange query)
      , NoUpdateAboutLoginStatus
      )

    GotPackages result ->
      case Debug.log "GotPackages result" result of
        Err httpError ->
            ({ model | entries = Failure }, Cmd.none, httpErrorToLoginUpdate httpError)

        Ok entries ->
          ( { model
                | entries = Success entries
                , session = Session.addEntries entries model.session
            }
          , Cmd.none
          , ConfirmedUserIsLoggedIn
          )

    DelayedUrlChange query ->
      if query == model.query then
        ( model, addQueryToUrl key model, NoUpdateAboutLoginStatus )
      else
        -- Input changed, do nothing
        ( model, Cmd.none, NoUpdateAboutLoginStatus )

    QuerySubmitted ->
      if String.contains "->" model.query then
        ( model, searchByType model.query, NoUpdateAboutLoginStatus )
      else
        ( model, Cmd.none, NoUpdateAboutLoginStatus )


searchByType : String -> Cmd msg
searchByType query =
  Nav.load <|
    Url.crossOrigin "https://klaftertief.github.io" [ "elm-search" ] [ Url.string "q" query ]


addQueryToUrl : Nav.Key -> Model -> Cmd msg
addQueryToUrl key model =
  Nav.replaceUrl key <|
    Url.absolute
      (case model.author of
         Nothing     -> []
         Just author -> [ "packages", author ]
      )
      (if String.isEmpty model.query then
        []
      else
        [ Url.string "q" model.query ]
      )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
  { title = Maybe.withDefault "Elm Packages" model.author
  , header =
      case model.author of
        Nothing     -> []
        Just author ->
          case model.repository of
            Just repository -> [ Skeleton.authorSegment repository author ]
            Nothing -> []
  , warning = Skeleton.NoProblems
  , attrs = []
  , kids =
      [ lazy3 viewSearch model.query model.author model.entries
      , viewSidebar
      ]
  }



-- VIEW SEARCH


viewSearch : String -> Maybe String -> Entries -> Html Msg
viewSearch query author entries =
  div [ class "catalog" ]
    [ Html.form
        [ onSubmit QuerySubmitted
        ]
        [ input
            [ placeholder "Search"
            , value query
            , onInput QueryChanged
            , autofocus True
            , name "q"
            , autocomplete False
            ]
            []
        ]
    , case entries of
        Failure ->
          div Problem.styles (Problem.offline "search.json")

        Loading ->
          text "" -- TODO

        Success es ->
          let
            results =
              Entry.search query author es
                |> List.take 300
                |> List.map viewEntry
          in
          div []
            [ Keyed.node "div" [] <|
                ("h", viewHint (List.isEmpty results) query) :: results
            ]
    ]



-- VIEW ENTRY


viewEntry : Entry -> (String, Html msg)
viewEntry entry =
  ( entry.author ++ "/" ++ entry.project
  , lazy viewEntryHelp entry
  )


viewEntryHelp : Entry -> Html msg
viewEntryHelp ({ repositoryId, author, project, summary } as entry) =
  div [ class "pkg-summary" ]
    [ div [ class "pkg-summary-title" ]
        [ h1 []
            [ a [ href (Href.toVersion repositoryId author project Nothing Nothing) ]
                [ span [ class "pkg-summary-author" ] [ text (author ++ "/") ]
                , wbr [] []
                , span [ class "pkg-summary-project" ] [ text project ]
                ]
            ]
        , viewLatestVersion entry
        ]
    , p [ class "pkg-summary-desc" ] [ text summary ]
    ]


viewLatestVersion : Entry -> Html msg
viewLatestVersion entry =
  div [ class "pkg-summary-version" ] <|
    case V.toTuple entry.version of
      (1, 0, 0) ->
        [ a
            [ href (Href.toVersion entry.repositoryId entry.author entry.project (Just entry.version) Nothing) ]
            [ text (V.toString entry.version) ]
        ]

      _ ->
        [ a
            [ href (Href.toProject entry.repositoryId entry.author entry.project) ]
            [ text "… " ]
        , a
            [ href (Href.toVersion entry.repositoryId entry.author entry.project (Just entry.version) Nothing) ]
            [ text (V.toString entry.version) ]
        ]




-- VIEW SIDEBAR


viewSidebar : Html msg
viewSidebar =
  div [ class "catalog-sidebar" ]
    [ h2 [] [ text "Core Packages" ]
    , ul []
        [ li [] [ a [ href "/packages/elm" ] [ text "elm" ] ]
        , li [] [ a [ href "/packages/elm-explorations" ] [ text "elm-explorations" ] ]
        ]
    , h2 [] [ text "Resources" ]
    , ul []
        [ li [] [ a [ href "https://klaftertief.github.io/elm-search/" ] [ text "Search by Type" ] ]
        , li [] [ a [ href "https://guide.elm-lang.org/install/elm.html#elm-install" ] [ text "Using Packages" ] ]
        , li [] [ a [ href "/help/design-guidelines" ] [ text "API Design Guidelines" ] ]
        , li [] [ a [ href "/help/documentation-format" ] [ text "Write great docs" ] ]
        , li [] [ a [ href "https://elm-lang.org" ] [ text "Elm Website" ] ]
        ]
    ]



-- VIEW HINTS


viewHint : Bool -> String -> Html msg
viewHint noAlts query =
  viewHintHelp noAlts (String.toLower (String.replace "-" " " query)) hints


viewHintHelp : Bool -> String -> List (Hint msg) -> Html msg
viewHintHelp noAlts query remainingHints =
  case remainingHints of
    [] ->
      text ""

    hint :: otherHints ->
      if String.startsWith query hint.term && (noAlts || String.length query >= hint.min) then
        hint.html
      else
        viewHintHelp noAlts query otherHints


type alias Hint msg =
  { term : String
  , min : Int
  , html : Html msg
  }


hints : List (Hint msg)
hints =
  [ Hint "spa" 3 singlePageApp
  , Hint "single page app" 5 singlePageApp
  , Hint "components" 5 components
  , Hint "router" 4 router
  , Hint "routing" 4 router
  , Hint "routes" 4 router
  , Hint "focus" 4 focus
  , Hint "blur" 4 focus
  , Hint "scroll" 4 scroll
  , Hint "scrollheight" 7 scroll
  , Hint "scrollwidth" 7 scroll
  , Hint "scrollx" 7 scroll
  , Hint "scrolly" 7 scroll
  , Hint "scrollto" 7 scroll
  , Hint "scrollintoview" 7 scroll
  , Hint "mouse" 4 mouse
  , Hint "keyboard" 4 keyboard
  , Hint "window" 4 window
  , Hint "visibility" 5 window
  , Hint "animation" 5 animation
  , Hint "requestanimationframe" 8 animation
  , Hint "lenses" 4 lenses
  ]


makeHint : List (Html msg) -> Html msg
makeHint message =
  p [ class "pkg-hint" ] <|
    b [] [ text "Hint:" ] :: text " " :: message


singlePageApp : Html msg
singlePageApp =
  makeHint
    [ text "All single-page apps in Elm use "
    , codeLink "https://package.elm-lang.org/packages/elm/browser/latest/" "elm/browser"
    , text " to control the URL, with help from "
    , codeLink "https://package.elm-lang.org/packages/elm/url/latest/" "elm/url"
    , text " convert between URLs and nice structured data. I very highly recommend working through "
    , guide
    , text " to learn how! Once you have made one or two single-page apps the standard way, it will be much easier to tell which (if any) of the packages below can make your code any easier."
    ]


components : Html msg
components =
  makeHint
    [ text "Components are objects!"
    , ul [ style "list-style-type" "none" ]
        [ li [] [ text "Components = Local State + Methods" ]
        , li [] [ text "Local State + Methods = Objects" ]
        ]
    , text "We get very few folks asking how to structure Elm code with objects. Elm does not have objects! We get a lot of folks asking about how to use components, but it is essentially the same question. Elm emphasizes "
    , i [] [ text "functions" ]
    , text " instead. Folks usually have the best experience if they follow the advice in "
    , guide
    , text " and "
    , a [ href "https://youtu.be/XpDsk374LDE" ] [ text "The Life of a File" ]
    , text ", exploring and understanding the techniques specific to Elm "
    , i [] [ text "before" ]
    , text " trying to bring in techniques from other languages."
    ]


router : Html msg
router =
  makeHint
    [ text "The "
    , codeLink "https://package.elm-lang.org/packages/elm/url/latest/" "elm/url"
    , text " package has everything you need to turn paths, queries, and hashes into useful data. But definitely work through "
    , guide
    , text " to learn how this fits into a "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser" (Just "application")) "Browser.application"
    , text " that manages the URL!"
    ]


focus : Html msg
focus =
  makeHint
    [ text "Check out "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Dom" Nothing) "Browser.Dom"
    , text " for focusing on certain nodes. It uses tasks, so be sure you have learned about "
    , code [] [ text "Cmd" ]
    , text " values in "
    , guide
    , text " and then read through the "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "core" Nothing "Task" Nothing) "Task"
    , text " module so you do not have to guess at how anything works!"
    ]


scroll : Html msg
scroll =
  makeHint
    [ text "Check out "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Dom" Nothing) "Browser.Dom"
    , text " for getting and setting scroll positions. It uses tasks, so be sure you have learned about "
    , code [] [ text "Cmd" ]
    , text " values in "
    , guide
    , text " and then read through the "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "core" Nothing "Task" Nothing) "Task"
    , text " module so you do not have to guess at how anything works!"
    ]


mouse : Html msg
mouse =
  makeHint
    [ text "Folks usually use "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "html" Nothing "Html.Events" Nothing) "Html.Events"
    , text " to detect clicks on buttons. If you want mouse events for the whole page, you may want "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Events" Nothing) "Browser.Events"
    , text " instead. Reading "
    , guide
    , text " should give the foundation for using either!"
    ]


keyboard : Html msg
keyboard =
  makeHint
    [ text "Folks usually use "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "html" Nothing "Html.Events" Nothing) "Html.Events"
    , text " for key presses in text fields. If you want keyboard events for the whole page, you may want "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Events" Nothing) "Browser.Events"
    , text " instead. Reading "
    , guide
    , text " should give the foundation for using either!"
    ]


window : Html msg
window =
  makeHint
    [ text "Use "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Dom" Nothing) "Browser.Dom"
    , text " to get the current window size, and use "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Events" Nothing) "Browser.Events"
    , text " to detect when the window changes size or is not visible at the moment."
    ]


animation : Html msg
animation =
  makeHint
    [ text "If you are not using CSS animations, you will need "
    , codeLink (Href.toModuleInStandardElmPackageRepo "elm" "browser" Nothing "Browser.Events" (Just "onAnimationFrame")) "onAnimationFrame"
    , text " to get smooth animations. The packages below may make one of these paths easier for you, but sometimes it is easier to just do things directly!"
    ]


lenses : Html msg
lenses =
  makeHint
    [ text "Lenses are not commonly used in Elm. Their design focuses on manipulating deeply nested data structures, like records in records in dictionaries in lists. But rather than introducing a complex system to help with already complex data structures, we encourage folks to first work on simplifying the data structure."
    , br [] []
    , br [] []
    , text "Maybe this means flattening records. Or using "
    , a [ href "https://guide.elm-lang.org/types/custom_types.html" ] [ text "custom types" ]
    , text " to model different possibilities more precisely. Or representing graphs with "
    , codeText "Dict"
    , text " values as described "
    , a [ href "https://evancz.gitbooks.io/functional-programming-in-elm/graphs/" ] [ text "here" ]
    , text ". Or using the module system to create strong boundaries, using opaque types with helper functions to contain complexity."
    , br [] []
    , br [] []
    , text "Point is, there are many paths to explore that will produce easier code with stronger guarantees, and folks are always happy to help if you share your situation on "
    , a [ href "http://elmlang.herokuapp.com/" ] [ text "Slack" ]
    , text " or "
    , a [ href "https://discourse.elm-lang.org/" ] [ text "Discourse" ]
    , text "!"
    ]


guide : Html msg
guide =
  codeLink "https://guide.elm-lang.org" "guide.elm-lang.org"


codeLink : String -> String -> Html msg
codeLink url txt =
  a [ href url ] [ codeText txt ]


codeText : String -> Html msg
codeText txt =
  code [] [ text txt ]
