module Main exposing (main)

import Browser
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , fillPortion
        , height
        , padding
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Http
import Json.Decode as JD
import Json.Schema.Definitions as Schema
import Uuid as Uuid



---- MODEL ----
-- gameplan:
-- - [x] make a request to list models
-- - [x] decode those models into the model type
-- - [x] show a table full of models with elm-ui
-- - [ ] when a model row is clicked, go to a detail page
-- - [ ] on the detail page, make a request for predictions
-- - [ ] decode those predictions into the prediction type
-- - [ ] show a "create prediction button"
-- - [ ] on click allow creating a json object
-- - [ ] show the validation status of that json object the whole time
-- - [ ] if it's valid for the model's arguments, submit a request and
--       refresh the list of predictions


type alias Model =
    List GranaryModel


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
    ( []
    , Http.get
        { url = "http://localhost:8080/api/models"
        , expect = Http.expectJson GotModels (JD.list decoderGranaryModel)
        }
    )



---- UPDATE ----


type Msg
    = GotModels (Result Http.Error (List GranaryModel))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotModels (Ok models) ->
            ( models, Cmd.none )

        GotModels (Err _) ->
            ( model, Cmd.none )



---- VIEW ----


mkHeaderName : String -> Element msg
mkHeaderName s =
    el
        [ Font.bold
        , Border.widthEach
            { bottom = 1
            , left = 0
            , right = 0
            , top = 0
            }
        , Font.size 24
        ]
        (Element.text s)


mkRowElement : String -> Element msg
mkRowElement s =
    el [ Font.size 16 ] (text s)


modelTable : Model -> Element Msg
modelTable model =
    Element.table [ padding 3, spacing 5 ]
        { data = model
        , columns =
            [ { header = mkHeaderName "Model ID"
              , width = fill
              , view = \granaryModel -> mkRowElement (Uuid.toString granaryModel.id)
              }
            , { header = mkHeaderName "Model name"
              , width = fill
              , view = \granaryModel -> mkRowElement granaryModel.name
              }
            , { header = mkHeaderName "Job Definition"
              , width = fill
              , view = \granaryModel -> mkRowElement granaryModel.jobDefinition
              }
            , { header = mkHeaderName "Job Queue"
              , width = fill
              , view = \granaryModel -> mkRowElement granaryModel.jobQueue
              }
            ]
        }


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column [ width fill ]
            [ row
                [ width fill
                , height (fillPortion 1)
                , padding 10
                , Background.color (rgb255 0 255 255)
                , Font.bold
                , Font.italic
                , Font.size 32
                ]
                [ text "Granary" ]
            , row
                [ width fill
                , height fill
                , padding 10
                ]
                [ modelTable model ]
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
