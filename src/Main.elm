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
import Json.Encode as JE
import Json.Schema as Schema
import Json.Schema.Definitions as Schema
import Json.Schema.Validation as Validation
import Time
import Url
import Uuid as Uuid


type alias PaginatedResponse a =
    { page : Int
    , limit : Int
    , results : List a
    }


type alias ModelDetail =
    { predictions : List GranaryPrediction
    , model : GranaryModel
    , addingPrediction : Bool
    , newPrediction : Result (List Validation.Error) JD.Value
    , newPredictionRaw : String
    }


type alias GranaryToken =
    String


type alias Breadcrumb =
    { url : String
    , name : String
    }


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


type alias StacAsset =
    { href : String
    , title : Maybe String
    , description : Maybe String
    , roles : List String
    , mediaType : String
    }


type alias GranaryPrediction =
    { id : Uuid.Uuid
    , modelId : Uuid.Uuid
    , invokedAt : Time.Posix
    , statusReason : Maybe String
    , results : List StacAsset
    , webhookId : Maybe Uuid.Uuid
    }


type alias PredictionCreate =
    { modelId : Uuid.Uuid
    , arguments : JD.Value
    }


encodePredictionCreate : PredictionCreate -> JE.Value
encodePredictionCreate predCreate =
    JE.object
        [ ( "modelId", Uuid.encode predCreate.modelId )
        , ( "arguments", predCreate.arguments )
        ]


decoderGranaryModel : JD.Decoder GranaryModel
decoderGranaryModel =
    JD.map5
        GranaryModel
        (JD.field "id" Uuid.decoder)
        (JD.field "name" JD.string)
        (JD.field "validator" (JD.field "schema" Schema.decoder))
        (JD.field "jobDefinition" JD.string)
        (JD.field "jobQueue" JD.string)


decoderStacAsset : JD.Decoder StacAsset
decoderStacAsset =
    JD.map5
        StacAsset
        (JD.field "href" JD.string)
        (JD.field "title" <| JD.maybe JD.string)
        (JD.field "description" <| JD.maybe JD.string)
        (JD.field "roles" <| JD.list JD.string)
        (JD.field "type" <| JD.string)


decoderGranaryPrediction : JD.Decoder GranaryPrediction
decoderGranaryPrediction =
    JD.map6
        GranaryPrediction
        (JD.field "id" Uuid.decoder)
        (JD.field "modelId" Uuid.decoder)
        (JD.field "invokedAt" JDE.datetime)
        (JD.field "statusReason" <| JD.maybe JD.string)
        (JD.field "results" <| JD.list decoderStacAsset)
        (JD.field "webhookId" <| JD.maybe Uuid.decoder)


paginatedDecoder : JD.Decoder a -> JD.Decoder (PaginatedResponse a)
paginatedDecoder ofDecoder =
    JD.map3
        PaginatedResponse
        (JD.field "pageSize" JD.int)
        (JD.field "page" JD.int)
        (JD.field "results" <| JD.list ofDecoder)


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



---- UPDATE ----


getPredictionCreate : ModelDetail -> Maybe PredictionCreate
getPredictionCreate detail =
    (Result.toMaybe << .newPrediction) detail
        |> Maybe.map (PredictionCreate detail.model.id)


modelUrl : Uuid.Uuid -> String
modelUrl =
    (++) "http://localhost:8080/api/models/" << Uuid.toString


predictionsUrl : Uuid.Uuid -> String
predictionsUrl =
    (++) "http://localhost:8080/api/predictions?modelId=" << Uuid.toString


fetchModels : Maybe GranaryToken -> Cmd.Cmd Msg
fetchModels token =
    token
        |> Maybe.map
            (\t ->
                B.get "http://localhost:8080/api/models"
                    |> B.withExpect (Http.expectJson GotModels (paginatedDecoder decoderGranaryModel))
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
                    |> B.withExpect (Http.expectJson GotPredictions (paginatedDecoder decoderGranaryPrediction))
                    |> B.withBearerToken t
                    |> B.request
            )
        |> Maybe.withDefault Cmd.none


postPrediction : GranaryToken -> PredictionCreate -> Cmd.Cmd Msg
postPrediction token predictionCreate =
    B.post "http://localhost:8080/api/predictions"
        |> B.withJsonBody (encodePredictionCreate predictionCreate)
        |> B.withBearerToken token
        |> B.withExpect (Http.expectJson CreatedPrediction decoderGranaryPrediction)
        |> B.request


maybePostPrediction : Maybe GranaryToken -> Maybe ModelDetail -> Cmd.Cmd Msg
maybePostPrediction tokenM detailM =
    case ( tokenM, detailM ) of
        ( Just token, Just detail ) ->
            getPredictionCreate detail
                |> Maybe.map (postPrediction token)
                |> Maybe.withDefault Cmd.none

        _ ->
            Cmd.none


type Msg
    = GotModels (Result Http.Error (PaginatedResponse GranaryModel))
    | GotModel (Result Http.Error GranaryModel)
    | GotPredictions (Result Http.Error (PaginatedResponse GranaryPrediction))
    | NewPrediction Uuid.Uuid Schema.Schema
    | Navigation Browser.UrlRequest
    | UrlChanged Url.Url
    | PredictionInput String
    | TokenInput String
    | TokenSubmit
    | PredictionSubmit
    | CreatedPrediction (Result Http.Error GranaryPrediction)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            let
                maybeModelId =
                    String.dropLeft 1 url.path |> Uuid.fromString

                cmdM =
                    maybeModelId
                        |> Maybe.map (fetchModel model.secrets)
            in
            case cmdM of
                Nothing ->
                    ( { model | modelDetail = Nothing }, fetchModels model.secrets )

                Just cmd ->
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
            ( { model | granaryModels = models.results }, Cmd.none )

        GotModels (Err _) ->
            ( model, Cmd.none )

        GotPredictions (Ok predictions) ->
            let
                baseModelDetail =
                    model.modelDetail

                updatedModelDetail =
                    Maybe.map (\rec -> { rec | predictions = predictions.results }) baseModelDetail
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

        CreatedPrediction (Ok _) ->
            ( model
            , Nav.pushUrl model.key
                ("/"
                    ++ (model.modelDetail
                            |> Maybe.map (Uuid.toString << .id << .model)
                            |> Maybe.withDefault ""
                       )
                )
            )

        CreatedPrediction _ ->
            ( model, Cmd.none )

        PredictionSubmit ->
            ( model, maybePostPrediction model.secrets model.modelDetail )



---- VIEW ----


fontRed : Element.Attr d m
fontRed =
    rgb255 255 0 0 |> Font.color


fuschia : Element.Color
fuschia =
    rgb255 255 0 255


homeCrumb : Breadcrumb
homeCrumb =
    Breadcrumb "/" "Home"


modelCrumb : GranaryModel -> Breadcrumb
modelCrumb granaryModel =
    Breadcrumb ("/" ++ Uuid.toString granaryModel.id) granaryModel.name


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
        , Element.focused [ Background.color fuschia ]
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
                        text <|
                            case ( prediction.statusReason, prediction.results ) of
                                ( Nothing, [] ) ->
                                    "In progress"

                                ( Just _, _ ) ->
                                    "Failed"

                                ( _, _ ) ->
                                    "Succeeded"
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


navBar : List Breadcrumb -> Element Msg
navBar links =
    links
        |> List.map
            (\crumb ->
                column [ spacing 3 ]
                    [ link
                        []
                        { url = crumb.url
                        , label = text crumb.name
                        }
                    ]
            )
        |> List.intersperse (Element.text " :> ")
        |> row [ spacing 3, Background.color fuschia, width fill ]


submitButton : Msg -> Element Msg
submitButton msg =
    Input.button
        [ Element.centerX
        , Background.color (rgb255 0 255 255)
        , Border.solid
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        ]
        { onPress = Just msg
        , label = text "Submit"
        }


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
            row [] [ Element.el [] (submitButton PredictionSubmit) ]

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
                        , navBar [ homeCrumb, modelCrumb detail.model ]
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
                        , navBar [ homeCrumb ]
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
                        , row [ width fill ] [ submitButton TokenSubmit ]
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
