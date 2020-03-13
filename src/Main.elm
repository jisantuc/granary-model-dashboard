module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as JD
import Json.Schema.Definitions as Schema
import Uuid as Uuid



---- MODEL ----
-- gameplan:
-- - [ ] make a request to list models
-- - [ ] decode those models into the model type
-- - [ ] show a table full of models
-- - [ ] when a model row is clicked, go to a detail page
-- - [ ] on the detail page, make a request for predictions
-- - [ ] decode those predictions into the prediction type
-- - [ ] show a "create prediction button"
-- - [ ] on click allow creating a json object
-- - [ ] show the validation status of that json object the whole time
-- - [ ] if it's valid for the model's arguments, submit a request and
--       refresh the list of predictions


type alias Model =
    {}


type alias GranaryModel =
    { id : Uuid.Uuid
    , name : String
    , validator : Schema.Schema
    , jobDefinition : String
    , jobQueue : String
    }


decoderGranaryModel : JD.Decoder GranaryModel
decoderGranaryModel =
    JD.map5
        GranaryModel
        (JD.field "id" Uuid.decoder)
        (JD.field "name" JD.string)
        (JD.field "validator" (JD.field "schema" Schema.decoder))
        (JD.field "jobDefinition" JD.string)
        (JD.field "jobQueue" JD.string)


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Http.get
        { url = "http://localhost:3000/api/models"
        , expect = Http.expectJson GotModels (JD.list decoderGranaryModel)
        }
    )



---- UPDATE ----


type Msg
    = NoOp
    | GotModels (Result Http.Error (List GranaryModel))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
