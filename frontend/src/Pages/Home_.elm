module Pages.Home_ exposing (Model, Msg, page)

import Api.Question as Question exposing (Question)
import Gen.Params.Home_ exposing (Params)
import Page
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http exposing (defaultConfig)
import Request
import Shared
import View exposing (View)
import Http exposing (header, Header)

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { questions : WebData (List Question) }


init : ( Model, Cmd Msg )
init =
    ( { questions = Loading }, loadQuestions )



-- COMMANDS

config : Http.Config 
config = { defaultConfig | headers = [header "Access-Control-Request-Method" "GET"] }

loadQuestions : Cmd Msg
loadQuestions =
    Http.getWithConfig config "http://localhost:5000/api/questions" HandleGetQuestions Question.listDecoder



-- UPDATE


type Msg
    = NoOp
    | HandleGetQuestions (WebData (List Question))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HandleGetQuestions data ->
            ( { model | questions = data }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view { questions } =
    case questions of
        Failure e ->
            View.placeholder "An Error occurred"

        Loading ->
            View.placeholder "Loading..."

        _ ->
            View.placeholder "Data loaded"
