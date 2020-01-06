module Main exposing (main)

import BodyBuilder exposing (..)
import BodyBuilder.Attributes as Attributes exposing (..)
import BodyBuilder.Elements.Clickable exposing (..)
import BodyBuilder.Events as Events
import BodyBuilder.Style as Style
import Browser
import Browser.Navigation as Nav
import Calendar exposing (Date, RawDate)
import Color
import DateTime exposing (fromPosix)
import Elegant exposing (SizeUnit, percent, pt, px, vh)
import Elegant.Border as Border
import Elegant.Box as Box
import Elegant.Constants as Constants
import Elegant.Corner as Corner
import Elegant.Cursor as Cursor
import Elegant.Dimensions as Dimensions
import Elegant.Display as Display
import Elegant.Outline as Outline
import Elegant.Padding as Padding
import Elegant.Typography as Typography
import Form exposing (..)
import Graphql.Http exposing (..)
import Graphql.Http.GraphqlError
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import List.Extra exposing (..)
import Modifiers exposing (..)
import PortfolioPerformance.InputObject as InputObject
import PortfolioPerformance.Mutation as Mutation
import PortfolioPerformance.Object as Object
import PortfolioPerformance.Object.Allocation as Allocation
import PortfolioPerformance.Object.PortfolioState as PortfolioState
import RemoteData exposing (RemoteData)
import Task
import Time
import Url
import Utils exposing (findBy, gray, intToMonth, normalizeIntForDate, textToHtml)


type alias Model =
    { inputs : InputObject.PortfolioStateInputType
    , key : Nav.Key
    , url : Url.Url
    , zone : Time.Zone
    , dateNow : DateTime.DateTime
    , startDateInput : Maybe DateTime.DateTime
    , finalBalance : Maybe Int
    }



-- type alias Inputs =
--     { startDate : Maybe DateTime.DateTime
--     , initialBalance : Int
--     , allocations : List Allocation
--     }


type alias PortfolioResult =
    { final_balance : Int
    }



-- type alias Allocation =
--     { symbol : String
--     , percentage : Int
--     }


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | ChangeInitialBalance Int
    | ChangeStartDate DateMsg
    | AdjustTimeZone Time.Zone
    | GetTime Time.Posix
    | ChangeSymbol Int String
    | ChangePercentage Int Int
    | AddAllocation
    | CalculateValueToday
    | GotPortfolio (RemoteData (Graphql.Http.Error (Maybe PortfolioResult)) (Maybe PortfolioResult))


homeView : Model -> Document Msg
homeView model =
    { title = "Portfolio Performance"
    , body =
        div
            [ style
                [ Style.box
                    [ Box.typography
                        [ Typography.fontFamilySansSerif
                        , Typography.size Constants.zeta
                        ]
                    ]
                ]
            ]
            [ view model ]
    }


view : Model -> NodeWithStyle Msg
view model =
    node []
        [ node
            [ style
                [ Style.block []
                , Style.box [ Box.padding [ Padding.horizontal Constants.medium ] ]
                ]
            ]
            ([ buildInputNumber
                (inputLabelPlaceholder "Initial Balance" "1337")
                model.inputs.initial_balance
                ChangeInitialBalance
             , buildDate
                (inputLabelPlaceholder "Start Date" "2013-03-20")
                model.startDateInput
                (DateBetween (fromPosix (Time.millisToPosix 0)) model.dateNow)
                ChangeStartDate
             ]
                ++ buildMutilpleInputText model.inputs.allocations
                ++ (monochromeSquaredButton
                        { background = Color.white
                        , border = Color.black
                        , text = Color.black
                        }
                        "Add"
                        AddAllocation
                        :: (case List.foldl (\a -> \b -> a.percentage + b) 0 model.inputs.allocations of
                                100 ->
                                    [ monochromeSquaredButton
                                        { background = Color.white
                                        , border = Color.black
                                        , text = Color.black
                                        }
                                        "Click here to value you balance today"
                                        CalculateValueToday
                                    ]

                                i ->
                                    [ div [] [ text ("Make sure to have 100 percents in total : " ++ String.fromInt i ++ "%") ] ]
                           )
                   )
                ++ (case model.finalBalance of
                        Just finalBalance ->
                            [ div [] [ text ("Final Balance : " ++ String.fromInt finalBalance ++ "$") ]
                            , div []
                                [ text
                                    (let
                                        valueMade =
                                            finalBalance - model.inputs.initial_balance
                                     in
                                     (if valueMade >= 0 then
                                        "You would have made "

                                      else
                                        "You would have lost "
                                     )
                                        ++ String.fromInt valueMade
                                        ++ "$"
                                    )
                                ]
                            ]

                        Nothing ->
                            []
                   )
            )
        ]


buildMutilpleInputText : List InputObject.AllocationInputType -> List (NodeWithStyle Msg)
buildMutilpleInputText list =
    List.indexedMap buildSymbolAndPercentage list


buildSymbolAndPercentage : Int -> InputObject.AllocationInputType -> NodeWithStyle Msg
buildSymbolAndPercentage i a =
    div []
        [ buildInputText
            (inputLabelPlaceholder "Add a symbol" "AAPL")
            a.symbol
            (ChangeSymbol i)
        , buildInputNumber
            (inputLabelPlaceholder "Add a percentage" "42")
            a.percentage
            (ChangePercentage i)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        inputs =
            model.inputs
    in
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        ChangeInitialBalance nb ->
            ( { model | inputs = { inputs | initial_balance = nb } }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        GetTime time ->
            ( { model | dateNow = fromPosix time }
            , Cmd.none
            )

        ChangeStartDate dateMsg ->
            ( case ( model.startDateInput, dateMsg ) of
                ( Just date, Day day ) ->
                    let
                        newStartDateInput =
                            DateTime.setDay day date
                    in
                    { model | startDateInput = newStartDateInput, inputs = updateStartDateInInputs inputs newStartDateInput }

                ( Just date, Month nb ) ->
                    case intToMonth nb of
                        Just month ->
                            let
                                newStartDateInput =
                                    DateTime.setMonth month date
                            in
                            { model | startDateInput = newStartDateInput, inputs = updateStartDateInInputs inputs newStartDateInput }

                        Nothing ->
                            model

                ( Just date, Year nb ) ->
                    let
                        newStartDateInput =
                            DateTime.setYear nb date
                    in
                    { model | startDateInput = newStartDateInput, inputs = updateStartDateInInputs inputs newStartDateInput }

                ( _, SetDefaultDate ) ->
                    { model | startDateInput = DateTime.fromRawParts { day = 20, month = Time.Mar, year = 2013 } { hours = 0, minutes = 0, seconds = 0, milliseconds = 0 } }

                ( _, RemoveDate ) ->
                    { model | startDateInput = Nothing }

                _ ->
                    model
            , Cmd.none
            )

        ChangeSymbol i str ->
            ( { model | inputs = { inputs | allocations = updateAt i (\a -> { a | symbol = str }) inputs.allocations } }
            , Cmd.none
            )

        ChangePercentage i nb ->
            ( { model | inputs = { inputs | allocations = updateAt i (\a -> { a | percentage = nb }) inputs.allocations } }
            , Cmd.none
            )

        AddAllocation ->
            ( { model | inputs = { inputs | allocations = inputs.allocations ++ [ InputObject.AllocationInputType "" 0 ] } }, Cmd.none )

        CalculateValueToday ->
            ( model
            , createPortfolioState model.inputs
            )

        GotPortfolio receiveData ->
            case receiveData of
                RemoteData.Success maybePortfolioResult ->
                    case maybePortfolioResult of
                        Just portfolioResult ->
                            ( { model | finalBalance = Just portfolioResult.final_balance }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateStartDateInInputs : InputObject.PortfolioStateInputType -> Maybe DateTime.DateTime -> InputObject.PortfolioStateInputType
updateStartDateInInputs inputs maybeStartDateInput =
    case maybeStartDateInput of
        Just startDateInput ->
            { inputs | start_date = stringFromDatetime startDateInput }

        Nothing ->
            inputs


stringFromDatetime : DateTime.DateTime -> String
stringFromDatetime date =
    ((date |> DateTime.getYear) |> String.fromInt)
        ++ "-"
        ++ ((date |> DateTime.getMonth) |> monthToInt |> String.fromInt |> normalizeIntForDate)
        ++ "-"
        ++ ((date |> DateTime.getDay) |> String.fromInt |> normalizeIntForDate)


createPortfolioState : InputObject.PortfolioStateInputType -> Cmd Msg
createPortfolioState inputs =
    sendPortfolioState inputs
        |> Graphql.Http.mutationRequest endPoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotPortfolio)


sendPortfolioState : InputObject.PortfolioStateInputType -> SelectionSet (Maybe PortfolioResult) RootMutation
sendPortfolioState inputs =
    Mutation.create_portfolio_state { portfolio_state = inputs } portfolioResultSelector


portfolioResultSelector : SelectionSet PortfolioResult Object.PortfolioState
portfolioResultSelector =
    SelectionSet.map PortfolioResult
        PortfolioState.final_balance


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- , publishedAt = Just <| Time.millisToPosix 1502323200


initInputs : Nav.Key -> InputObject.PortfolioStateInputType
initInputs key =
    { start_date = "2013-03-20"
    , initial_balance = 0
    , allocations = [ InputObject.AllocationInputType "" 0 ]
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { inputs = initInputs key
      , key = key
      , url = url
      , zone = Time.utc
      , dateNow = fromPosix <| Time.millisToPosix 0
      , startDateInput = Nothing
      , finalBalance = Nothing
      }
    , Cmd.batch [ Task.perform AdjustTimeZone Time.here, Task.perform GetTime Time.now ]
    )


main : Program () Model Msg
main =
    application
        { init = \flags -> \url -> \key -> init flags url key
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = update
        , subscriptions = subscriptions
        , view = homeView
        }


endPoint : String
endPoint =
    "http://localhost:3000/graphql"
