module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , fillPortion
        , height
        , link
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
import Element.Input as Input
import Http as Http
import HttpBuilder as B
import Iso8601
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Schema as Schema
import Json.Schema.Definitions as Schema
import Json.Schema.Validation as Validation
import Time
import Url
import Uuid as Uuid



---- MODEL ----
-- gameplan:
-- - [x] make a request to list models
-- - [x] decode those models into the model type
-- - [x] show a table full of models with elm-ui
-- - [x] when a model row is clicked, go to a detail page
-- - [x] uncenter all the stuff?
-- - [x] on the detail page, make a request for predictions
-- - [x] decode those predictions into the prediction type
-- - [x] show a "create prediction button"
-- - [x] allow entering and storing a token to use for Granary requests
-- - [x] after storing that token, do all the stuff above
-- - [x] on click allow creating a json object
-- - [x] show the validation status of that json object the whole time
-- - [ ] if it's valid for the model's arguments, submit a request and
--       refresh the list of predictions


type alias ModelDetail =
    { predictions : List GranaryPrediction
    , model : GranaryModel
    , addingPrediction : Bool
    , newPrediction : Result (List Validation.Error) JD.Value
    , newPredictionRaw : String
    }


type alias GranaryToken =
    String


type alias Model =
    { url : Url.Url
    , key : Nav.Key
    , granaryModels : List GranaryModel
    , modelDetail : Maybe ModelDetail
    , secrets : Maybe GranaryToken
    , secretsUnsubmitted : Maybe GranaryToken
    }


type alias GranaryModel =
    { id : Uuid.Uuid
    , name : String
    , validator : Schema.Schema
    , jobDefinition : String
    , jobQueue : String
    }


type alias GranaryPrediction =
    { id : Uuid.Uuid
    , modelId : Uuid.Uuid
    , invokedAt : Time.Posix
    , statusReason : Maybe String
    , outputLocation : Maybe String
    , webhookId : Maybe Uuid.Uuid
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


decoderGranaryPrediction : JD.Decoder GranaryPrediction
decoderGranaryPrediction =
    JD.map6
        GranaryPrediction
        (JD.field "id" Uuid.decoder)
        (JD.field "modelId" Uuid.decoder)
        (JD.field "invokedAt" JDE.datetime)
        (JD.field "statusReason" <| JD.maybe JD.string)
        (JD.field "outputLocation" <| JD.maybe JD.string)
        (JD.field "webhookId" <| JD.maybe Uuid.decoder)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { url = url
      , key = key
      , granaryModels = []
      , modelDetail = Nothing
      , secrets = Nothing
      , secretsUnsubmitted = Nothing
      }
    , Cmd.none
    )


modelUrl : Uuid.Uuid -> String
modelUrl =
    (++) "https://granary.rasterfoundry.com/api/models/" << Uuid.toString


predictionsUrl : Uuid.Uuid -> String
predictionsUrl =
    (++) "https://granary.rasterfoundry.com/api/predictions?modelId=" << Uuid.toString


fetchModels : Maybe GranaryToken -> Cmd.Cmd Msg
fetchModels token =
    token
        |> Maybe.map
            (\t ->
                B.get "https://granary.rasterfoundry.com/api/models"
                    |> B.withExpect (Http.expectJson GotModels (JD.list decoderGranaryModel))
                    |> B.withBearerToken t
                    |> B.request
            )
        |> Maybe.withDefault Cmd.none


fetchModel : Maybe GranaryToken -> Uuid.Uuid -> Cmd.Cmd Msg
fetchModel token modelId =
    token
        |> Maybe.map
            (\t ->
                modelUrl modelId
                    |> B.get
                    |> B.withExpect (Http.expectJson GotModel decoderGranaryModel)
                    |> B.withBearerToken t
                    |> B.request
            )
        |> Maybe.withDefault Cmd.none


fetchPredictions : Maybe GranaryToken -> Uuid.Uuid -> Cmd.Cmd Msg
fetchPredictions token modelId =
    token
        |> Maybe.map
            (\t ->
                predictionsUrl modelId
                    |> B.get
                    |> B.withExpect (Http.expectJson GotPredictions (JD.list decoderGranaryPrediction))
                    |> B.withBearerToken t
                    |> B.request
            )
        |> Maybe.withDefault Cmd.none



---- UPDATE ----


type Msg
    = GotModels (Result Http.Error (List GranaryModel))
    | GotModel (Result Http.Error GranaryModel)
    | GotPredictions (Result Http.Error (List GranaryPrediction))
    | NewPrediction Uuid.Uuid Schema.Schema
    | Navigation Browser.UrlRequest
    | UrlChanged Url.Url
    | PredictionInput String
    | TokenInput String
    | TokenSubmit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            let
                maybeModelId =
                    String.dropLeft 1 url.path |> Uuid.fromString

                cmd =
                    maybeModelId
                        |> Maybe.map (fetchModel model.secrets)
                        |> Maybe.withDefault Cmd.none
            in
            ( model, cmd )

        Navigation urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        GotModel (Ok granaryModel) ->
            ( { model
                | granaryModels = []
                , modelDetail = Just <| ModelDetail [] granaryModel False (Result.Err []) "{}"
              }
            , fetchPredictions model.secrets granaryModel.id
            )

        GotModel (Err _) ->
            ( model, Nav.pushUrl model.key "/" )

        GotModels (Ok models) ->
            ( { model | granaryModels = models }, Cmd.none )

        GotModels (Err _) ->
            ( model, Cmd.none )

        GotPredictions (Ok predictions) ->
            let
                baseModelDetail =
                    model.modelDetail

                updatedModelDetail =
                    Maybe.map (\rec -> { rec | predictions = predictions }) baseModelDetail
            in
            ( { model | modelDetail = updatedModelDetail }, Cmd.none )

        GotPredictions (Err _) ->
            ( model, Cmd.none )

        NewPrediction _ _ ->
            let
                baseModelDetail =
                    model.modelDetail

                updatedModelDetail =
                    Maybe.map (\rec -> { rec | addingPrediction = True }) baseModelDetail
            in
            ( { model | modelDetail = updatedModelDetail }, Cmd.none )

        PredictionInput s ->
            let
                baseModelDetail =
                    model.modelDetail

                valueDecodeResult =
                    JD.decodeString JD.value s
                        |> Result.mapError
                            (always
                                [ { details = Validation.AlwaysFail
                                  , jsonPointer =
                                        { ns = ""
                                        , path = []
                                        }
                                  }
                                ]
                            )

                updatedModelDetail =
                    baseModelDetail
                        |> Maybe.map
                            (\rec ->
                                { rec
                                    | newPredictionRaw = s
                                    , newPrediction =
                                        valueDecodeResult
                                            |> Result.andThen
                                                (\value ->
                                                    Schema.validateValue { applyDefaults = True }
                                                        value
                                                        rec.model.validator
                                                )
                                }
                            )
            in
            ( { model | modelDetail = updatedModelDetail }, Cmd.none )

        TokenInput s ->
            ( { model | secretsUnsubmitted = Just s }, Cmd.none )

        TokenSubmit ->
            ( { model | secrets = model.secretsUnsubmitted, secretsUnsubmitted = Nothing }
            , fetchModels model.secretsUnsubmitted
            )



---- VIEW ----


fontRed : Element.Attr d m
fontRed =
    rgb255 255 0 0 |> Font.color


fontGreen : Element.Attr d m
fontGreen =
    rgb255 0 255 0 |> Font.color


mkHeaderName : String -> Element msg
mkHeaderName s =
    el
        [ Font.bold
        , Font.size 24
        , Border.widthEach
            { bottom = 1
            , left = 0
            , right = 0
            , top = 0
            }
        ]
        (Element.text s)


mkRowElement : String -> Element msg
mkRowElement s =
    el [ Font.size 16 ] (text s)


modelLink : GranaryModel -> Element msg
modelLink grModel =
    link []
        { url = "/" ++ (grModel.id |> Uuid.toString)
        , label = mkRowElement grModel.name
        }


newPredictionButton : Uuid.Uuid -> Schema.Schema -> Element Msg
newPredictionButton modelId modelSchema =
    Input.button
        [ Background.color <| rgb255 0 255 255
        , Element.focused [ Background.color (rgb255 255 0 255) ]
        ]
        { onPress = Just (NewPrediction modelId modelSchema)
        , label = Element.el [ Font.bold ] (text "New!")
        }


modelTable : Model -> Element Msg
modelTable model =
    Element.table [ padding 3, spacing 10, Element.alignLeft ]
        { data = model.granaryModels
        , columns =
            [ { header = mkHeaderName "Model name"
              , width = fill
              , view = \granaryModel -> modelLink granaryModel
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


predictionsTable : ModelDetail -> Element msg
predictionsTable detail =
    Element.table [ padding 3, spacing 10, Element.alignLeft ]
        { data = detail.predictions
        , columns =
            [ { header = mkHeaderName "Invocation time"
              , width = fill
              , view =
                    \prediction ->
                        let
                            invokedAt =
                                prediction.invokedAt
                        in
                        text <| Iso8601.fromTime invokedAt
              }
            , { header = mkHeaderName "Status"
              , width = fill
              , view =
                    \prediction ->
                        Maybe.map (\_ -> "Failed") prediction.statusReason
                            |> Maybe.withDefault "Succeeded"
                            |> text
              }
            , { header = mkHeaderName "Results"
              , width = fill
              , view =
                    \prediction ->
                        Maybe.map
                            (\uri ->
                                link []
                                    { url = uri
                                    , label = text "Download"
                                    }
                            )
                            prediction.outputLocation
                            |> Maybe.withDefault (row [] [])
              }
            ]
        }


titleBar : String -> Element msg
titleBar s =
    row
        [ width fill
        , height (fillPortion 1)
        , padding 10
        , Background.color (rgb255 0 255 255)
        , Font.bold
        , Font.italic
        , Font.size 32
        ]
        [ text s ]


boldKvPair : String -> String -> List (Element msg)
boldKvPair s1 s2 =
    [ Element.el
        [ Font.bold
        ]
        (text s1)
    , Element.el [] (text s2)
    ]


modelDetailColumn : List (Element msg) -> Element msg
modelDetailColumn =
    column [ height (fillPortion 2), width fill, Element.alignTop, padding 10, spacing 10 ]


granaryModelDetailPairs : ModelDetail -> List (Element msg)
granaryModelDetailPairs detail =
    [ row [ Font.bold ]
        [ Element.el
            [ Border.widthEach
                { bottom = 1
                , left = 0
                , right = 0
                , top = 0
                }
            ]
            (text "Model Details")
        ]
    , row [] <| boldKvPair "Name: " detail.model.name
    , row [] <| boldKvPair "Model ID: " (Uuid.toString detail.model.id)
    , row [] <| boldKvPair "Job Definition: " detail.model.jobDefinition
    , row [] <| boldKvPair "Job Queue: " detail.model.jobQueue
    ]


getErrField : Validation.Error -> String
getErrField err =
    case err.jsonPointer.path of
        [] ->
            "ROOT"

        errs ->
            List.intersperse "." errs
                |> String.concat


makeErr : Validation.Error -> List (Element Msg)
makeErr err =
    case err.details of
        Validation.Required fields ->
            fields
                |> List.map (\s -> row [] [ text "Missing field: ", Element.el [ fontRed ] (text s) ])

        Validation.AlwaysFail ->
            [ row [] [ text "Invalid json" ] ]

        Validation.InvalidType t ->
            [ row []
                [ column []
                    [ row [] [ text (t ++ " in ") ]
                    , row [] [ Element.el [ fontRed ] (getErrField err |> text) ]
                    ]
                ]
            ]

        Validation.RequiredProperty ->
            []

        _ ->
            [ row []
                [ text "I can't tell what else is wrong with "
                , err.jsonPointer.path
                    |> List.head
                    |> Maybe.withDefault "ROOT"
                    |> (Element.el [ fontRed ] << text)
                ]
            ]


getErrorElem : Result (List Validation.Error) JD.Value -> String -> Element Msg
getErrorElem result rawValue =
    case ( result, rawValue ) of
        ( _, "" ) ->
            row [] [ text "Enter JSON for this model's schema" ]

        -- should be a button, but _soon_
        ( Result.Ok _, _ ) ->
            row [] [ Element.el [ fontGreen ] (text "Ok!") ]

        ( Err errs, _ ) ->
            column [ spacing 3 ] (errs |> List.concatMap makeErr)


predictionsPane : ModelDetail -> List (Element Msg)
predictionsPane detail =
    if detail.addingPrediction then
        [ row []
            [ Input.multiline
                []
                { onChange = PredictionInput
                , text = detail.newPredictionRaw
                , placeholder = Input.placeholder [] (text "{}") |> Just
                , label = Input.labelAbove [] (text "Prediction input")
                , spellcheck = True
                }
            ]
        , getErrorElem detail.newPrediction detail.newPredictionRaw
        ]

    else
        [ row [ Font.bold ]
            [ text "Predictions: "
            , newPredictionButton detail.model.id detail.model.validator
            ]
        , predictionsTable detail
        ]


view : Model -> Browser.Document Msg
view model =
    case ( model.secrets, model.modelDetail ) of
        ( Just _, Just detail ) ->
            { title = detail.model.name
            , body =
                [ Element.layout [] <|
                    column [ width fill ]
                        [ titleBar detail.model.name
                        , row [ height fill, width fill ]
                            [ modelDetailColumn <| granaryModelDetailPairs detail
                            , modelDetailColumn <| predictionsPane detail
                            ]
                        ]
                ]
            }

        ( Just _, Nothing ) ->
            { title = "Available Models"
            , body =
                [ Element.layout [] <|
                    column [ width fill ]
                        [ titleBar "Granary"
                        , row
                            [ width fill
                            , height fill
                            , padding 10
                            ]
                            [ modelTable model ]
                        ]
                ]
            }

        ( Nothing, _ ) ->
            { title = "Granary Model Dashboard"
            , body =
                [ Element.layout [] <|
                    column [ spacing 3, Element.centerX, Element.centerY, width Element.shrink ]
                        [ titleBar "Granary"
                        , row [ width fill ]
                            [ Input.text []
                                { onChange = TokenInput
                                , text = model.secretsUnsubmitted |> Maybe.withDefault ""
                                , placeholder = Input.placeholder [] (text "Enter a token") |> Just
                                , label = Input.labelHidden "Token input"
                                }
                            ]
                        , row [ width fill ]
                            [ Input.button
                                [ Element.centerX
                                , Background.color (rgb255 0 255 255)
                                , Border.solid
                                , Border.color (rgb255 0 0 0)
                                , Border.width 2
                                ]
                                { onPress = Just TokenSubmit
                                , label = text "Submit"
                                }
                            ]
                        ]
                ]
            }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = Navigation
        , onUrlChange = UrlChanged
        }
