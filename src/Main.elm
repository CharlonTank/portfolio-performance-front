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
import PortfolioPerformance.Object.PricePerTime as PricePerTime
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
    , portfolioResult : Maybe PortfolioResult
    , mutualization : Bool
    }



-- type alias Inputs =
--     { startDate : Maybe DateTime.DateTime
--     , initialBalance : Int
--     , allocations : List Allocation
--     }


type alias PortfolioResult =
    { finalBalance : Int
    , allocations : List AllocationResult
    }


type alias AllocationResult =
    { symbol : String
    , percentage : Int
    , price_per_times : List PricePerTime
    }


type alias PricePerTime =
    { price : Float
    , time : String
    }


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
    | ToggleMutualization


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
                ++ (case model.portfolioResult of
                        Just portfolioResult ->
                            [ showPortfolioResult model.inputs.initial_balance portfolioResult model.mutualization
                            , monochromeSquaredButton
                                { background = Color.white
                                , border = Color.black
                                , text = Color.black
                                }
                                "Toggle mutualization"
                                ToggleMutualization
                            ]

                        Nothing ->
                            []
                   )
            )
        ]


showPortfolioResult : Int -> PortfolioResult -> Bool -> NodeWithStyle Msg
showPortfolioResult initialBalance portfolioResult mutualization =
    if mutualization then
        div []
            [ div [] [ text ("Final Balance : " ++ String.fromInt portfolioResult.finalBalance ++ "$") ]
            , div []
                [ text
                    (let
                        valueMade =
                            portfolioResult.finalBalance - initialBalance
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

    else
        div []
            (List.map (showAllocationResult initialBalance) portfolioResult.allocations)


showAllocationResult : Int -> AllocationResult -> NodeWithStyle Msg
showAllocationResult initialBalance allocation =
    let
        ratio =
            case ( List.Extra.last allocation.price_per_times, List.head allocation.price_per_times ) of
                ( Just last, Just first ) ->
                    first.price / last.price

                _ ->
                    1

        initialAllocationBalance =
            toFloat (initialBalance * allocation.percentage) / 100

        finaAllocationBalance =
            ratio * initialAllocationBalance
    in
    div []
        [ div []
            [ text ("Symbol : " ++ allocation.symbol) ]
        , div []
            [ text ("Initial balance : " ++ String.fromFloat initialAllocationBalance) ]
        , div []
            [ text ("Final balance : " ++ String.fromFloat finaAllocationBalance) ]
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
                            ( { model | portfolioResult = Just portfolioResult }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleMutualization ->
            ( { model | mutualization = not model.mutualization }, Cmd.none )


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
    SelectionSet.map2 PortfolioResult
        PortfolioState.final_balance
        (PortfolioState.allocations allocationSelector)


allocationSelector : SelectionSet AllocationResult Object.Allocation
allocationSelector =
    SelectionSet.map3 AllocationResult
        Allocation.symbol
        Allocation.percentage
        (Allocation.price_per_times pricePerTimeSelector)


pricePerTimeSelector : SelectionSet PricePerTime Object.PricePerTime
pricePerTimeSelector =
    SelectionSet.map2 PricePerTime
        PricePerTime.value
        PricePerTime.time


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
      , portfolioResult = Nothing
      , mutualization = True
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
