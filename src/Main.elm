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


type alias TaskDetail =
    { executions  : List GranaryExecution
    , task : GranaryTask
    , addingExecution : Bool
    , newExecution : Result (List Validation.Error) JD.Value
    , newExecutionRaw : String
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
    , granaryTasks : List GranaryTask
    , taskDetail : Maybe TaskDetail
    , secrets : Maybe GranaryToken
    , secretsUnsubmitted : Maybe GranaryToken
    }


type alias GranaryTask =
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


type alias GranaryExecution =
    { id : Uuid.Uuid
    , taskId : Uuid.Uuid
    , invokedAt : Time.Posix
    , statusReason : Maybe String
    , results : List StacAsset
    , webhookId : Maybe Uuid.Uuid
    }


type alias ExecutionCreate =
    { taskId : Uuid.Uuid
    , arguments : JD.Value
    }


encodeExecutionCreate : ExecutionCreate -> JE.Value
encodeExecutionCreate executionCreate =
    JE.object
        [ ( "taskId", Uuid.encode executionCreate.taskId )
        , ( "arguments", executionCreate.arguments )
        ]


decoderGranaryModel : JD.Decoder GranaryTask
decoderGranaryModel =
    JD.map5
        GranaryTask
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


decoderGranaryExecution : JD.Decoder GranaryExecution
decoderGranaryExecution =
    JD.map6
        GranaryExecution
        (JD.field "id" Uuid.decoder)
        (JD.field "taskId" Uuid.decoder)
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
      , granaryTasks = []
      , taskDetail = Nothing
      , secrets = Nothing
      , secretsUnsubmitted = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


getExecutionCreate : TaskDetail -> Maybe ExecutionCreate
getExecutionCreate detail =
    (Result.toMaybe << .newExecution) detail
        |> Maybe.map (ExecutionCreate detail.task.id)


modelUrl : Uuid.Uuid -> String
modelUrl =
    (++) "https://granary.rasterfoundry.com/api/tasks/" << Uuid.toString


executionsUrl : Uuid.Uuid -> String
executionsUrl =
    (++) "https://granary.rasterfoundry.com/api/executions?taskId=" << Uuid.toString


fetchModels : Maybe GranaryToken -> Cmd.Cmd Msg
fetchModels token =
    token
        |> Maybe.map
            (\t ->
                B.get "https://granary.rasterfoundry.com/api/tasks"
                    |> B.withExpect (Http.expectJson GotTasks (paginatedDecoder decoderGranaryModel))
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
                    |> B.withExpect (Http.expectJson GotTask decoderGranaryModel)
                    |> B.withBearerToken t
                    |> B.request
            )
        |> Maybe.withDefault Cmd.none


fetchExecutions : Maybe GranaryToken -> Uuid.Uuid -> Cmd.Cmd Msg
fetchExecutions token modelId =
    token
        |> Maybe.map
            (\t ->
                executionsUrl modelId
                    |> B.get
                    |> B.withExpect (Http.expectJson GotExecutions (paginatedDecoder decoderGranaryExecution))
                    |> B.withBearerToken t
                    |> B.request
            )
        |> Maybe.withDefault Cmd.none


postExecution : GranaryToken -> ExecutionCreate -> Cmd.Cmd Msg
postExecution token executionCreate =
    B.post "https://granary.rasterfoundry.com/api/executions"
        |> B.withJsonBody (encodeExecutionCreate executionCreate)
        |> B.withBearerToken token
        |> B.withExpect (Http.expectJson CreatedExecution decoderGranaryExecution)
        |> B.request


maybePostExecution : Maybe GranaryToken -> Maybe TaskDetail -> Cmd.Cmd Msg
maybePostExecution tokenM detailM =
    case ( tokenM, detailM ) of
        ( Just token, Just detail ) ->
            getExecutionCreate detail
                |> Maybe.map (postExecution token)
                |> Maybe.withDefault Cmd.none

        _ ->
            Cmd.none


type Msg
    = GotTasks (Result Http.Error (PaginatedResponse GranaryTask))
    | GotTask (Result Http.Error GranaryTask)
    | GotExecutions (Result Http.Error (PaginatedResponse GranaryExecution))
    | NewExecution Uuid.Uuid Schema.Schema
    | Navigation Browser.UrlRequest
    | UrlChanged Url.Url
    | ExecutionInput String
    | TokenInput String
    | TokenSubmit
    | ExecutionSubmit
    | CreatedExecution (Result Http.Error GranaryExecution)


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
                    ( { model | taskDetail = Nothing }, fetchModels model.secrets )

                Just cmd ->
                    ( model, cmd )

        Navigation urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        GotTask (Ok granaryTask) ->
            ( { model
                | granaryTasks = []
                , taskDetail = Just <| TaskDetail [] granaryTask False (Result.Err []) "{}"
              }
            , fetchExecutions model.secrets granaryTask.id
            )

        GotTask (Err _) ->
            ( model, Nav.pushUrl model.key "/" )

        GotTasks (Ok tasks) ->
            ( { model | granaryTasks = tasks.results }, Cmd.none )

        GotTasks (Err _) ->
            ( model, Cmd.none )

        GotExecutions (Ok executions) ->
            let
                baseTaskDetail =
                    model.taskDetail

                updatedTaskDetail =
                    Maybe.map (\rec -> { rec | executions = executions.results }) baseTaskDetail
            in
            ( { model | taskDetail = updatedTaskDetail }, Cmd.none )

        GotExecutions (Err _) ->
            ( model, Cmd.none )

        NewExecution _ _ ->
            let
                baseTaskDetail =
                    model.taskDetail

                updatedTaskDetail =
                    Maybe.map (\rec -> { rec | addingExecution = True }) baseTaskDetail
            in
            ( { model | taskDetail = updatedTaskDetail }, Cmd.none )

        ExecutionInput s ->
            let
                baseTaskDetail =
                    model.taskDetail

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

                updatedTaskDetail =
                    baseTaskDetail
                        |> Maybe.map
                            (\rec ->
                                { rec
                                    | newExecutionRaw = s
                                    , newExecution =
                                        valueDecodeResult
                                            |> Result.andThen
                                                (\value ->
                                                    Schema.validateValue { applyDefaults = True }
                                                        value
                                                        rec.task.validator
                                                )
                                }
                            )
            in
            ( { model | taskDetail = updatedTaskDetail }, Cmd.none )

        TokenInput s ->
            ( { model | secretsUnsubmitted = Just s }, Cmd.none )

        TokenSubmit ->
            ( { model | secrets = model.secretsUnsubmitted, secretsUnsubmitted = Nothing }
            , fetchModels model.secretsUnsubmitted
            )

        CreatedExecution (Ok _) ->
            ( model
            , Nav.pushUrl model.key
                ("/"
                    ++ (model.taskDetail
                            |> Maybe.map (Uuid.toString << .id << .task)
                            |> Maybe.withDefault ""
                       )
                )
            )

        CreatedExecution _ ->
            ( model, Cmd.none )

        ExecutionSubmit ->
            ( model, maybePostExecution model.secrets model.taskDetail )



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


taskCrumb : GranaryTask -> Breadcrumb
taskCrumb granaryModel =
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
        (text s)


mkRowElement : String -> Element msg
mkRowElement s =
    el [ Font.size 16 ] (text s)


taskLink : GranaryTask -> Element msg
taskLink grModel =
    link []
        { url = "/" ++ (grModel.id |> Uuid.toString)
        , label = mkRowElement grModel.name
        }


newExecutionButton : Uuid.Uuid -> Schema.Schema -> Element Msg
newExecutionButton modelId modelSchema =
    Input.button
        [ Background.color <| rgb255 0 255 255
        , Element.focused [ Background.color fuschia ]
        ]
        { onPress = Just (NewExecution modelId modelSchema)
        , label = Element.el [ Font.bold ] (text "New!")
        }


taskTable : Model -> Element Msg
taskTable model =
    Element.table [ padding 3, spacing 10, Element.alignLeft ]
        { data = model.granaryTasks
        , columns =
            [ { header = mkHeaderName "Task name"
              , width = fill
              , view = \granaryModel -> taskLink granaryModel
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


executionsTable : TaskDetail -> Element msg
executionsTable detail =
    Element.table [ padding 3, spacing 10, Element.alignLeft ]
        { data = detail.executions
        , columns =
            [ { header = mkHeaderName "Invocation time"
              , width = fill
              , view =
                    \execution ->
                        let
                            invokedAt =
                                execution.invokedAt
                        in
                        text <| Iso8601.fromTime invokedAt
              }
            , { header = mkHeaderName "Status"
              , width = fill
              , view =
                    \execution ->
                        text <|
                            case ( execution.statusReason, execution.results ) of
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


taskDetailColumn : List (Element msg) -> Element msg
taskDetailColumn =
    column [ height (fillPortion 2), width fill, Element.alignTop, padding 10, spacing 10 ]


granaryTaskDetailPairs : TaskDetail -> List (Element msg)
granaryTaskDetailPairs detail =
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
    , row [] <| boldKvPair "Name: " detail.task.name
    , row [] <| boldKvPair "Task ID: " (Uuid.toString detail.task.id)
    , row [] <| boldKvPair "Job Definition: " detail.task.jobDefinition
    , row [] <| boldKvPair "Job Queue: " detail.task.jobQueue
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
            row [] [ Element.el [] (submitButton ExecutionSubmit) ]

        ( Err errs, _ ) ->
            column [ spacing 3 ] (errs |> List.concatMap makeErr)


executionsPane : TaskDetail -> List (Element Msg)
executionsPane detail =
    if detail.addingExecution then
        [ row []
            [ Input.multiline
                []
                { onChange = ExecutionInput
                , text = detail.newExecutionRaw
                , placeholder = Input.placeholder [] (text "{}") |> Just
                , label = Input.labelAbove [] (text "Execution input")
                , spellcheck = True
                }
            ]
        , getErrorElem detail.newExecution detail.newExecutionRaw
        ]

    else
        [ row [ Font.bold ]
            [ text "Executions: "
            , newExecutionButton detail.task.id detail.task.validator
            ]
        , executionsTable detail
        ]


view : Model -> Browser.Document Msg
view model =
    case ( model.secrets, model.taskDetail ) of
        ( Just _, Just detail ) ->
            { title = detail.task.name
            , body =
                [ Element.layout [] <|
                    column [ width fill ]
                        [ titleBar detail.task.name
                        , navBar [ homeCrumb, taskCrumb detail.task ]
                        , row [ height fill, width fill ]
                            [ taskDetailColumn <| granaryTaskDetailPairs detail
                            , taskDetailColumn <| executionsPane detail
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
                            [ taskTable model ]
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
