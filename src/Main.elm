module Main exposing (main)

import BodyBuilder exposing (..)
import BodyBuilder.Attributes as Attributes exposing (..)
import BodyBuilder.Elements.Clickable exposing (..)
import BodyBuilder.Style as Style
import Browser
import Browser.Navigation as Nav
import Calendar exposing (Date, RawDate)
import Color
import DateTime exposing (fromPosix)
import Elegant exposing (SizeUnit, percent, pt, px, vh)
import Elegant.Block as Block
import Elegant.Box as Box
import Elegant.Constants as Constants
import Elegant.Flex as Flex
import Elegant.Margin as Margin
import Elegant.Padding as Padding
import Elegant.Typography as Typography
import Form exposing (..)
import Graphql.Http exposing (..)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import List.Extra exposing (..)
import Modifiers exposing (..)
import PortfolioPerformance.InputObject as InputObject
import PortfolioPerformance.Mutation as Mutation
import PortfolioPerformance.Object as Object
import PortfolioPerformance.Object.Allocation as Allocation
import PortfolioPerformance.Object.PortfolioState as PortfolioState
import PortfolioPerformance.Object.PricePerTime as PricePerTime
import PortfolioPerformance.Query as Query
import RemoteData exposing (RemoteData)
import Task
import Time
import Url
import Utils
    exposing
        ( intToMonth
        , normalizeIntForDate
        , onOrOff
        , sameDate
        )



--MAIN--


type alias Flags =
    { configBackendEndpoint : String
    , configFrontendEndpoint : String
    }


main : Program Flags Model Msg
main =
    application
        { init = \flags -> \url -> \key -> init flags url key
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = update
        , subscriptions = subscriptions
        , view = homeView
        }



-- MODEL


type alias Model =
    { flags : Flags
    , inputs : InputObject.PortfolioStateInputType
    , key : Nav.Key
    , url : Url.Url
    , dateNow : DateTime.DateTime
    , startDateInput : Maybe DateTime.DateTime
    , portfolioResult : Maybe PortfolioResult
    , mutualization : Bool
    , loading : Bool
    , yearlyRebalancing : Bool
    , finalBalance : FinalBalance
    }


type alias PortfolioResult =
    { allocations : List AllocationResult
    , token : Maybe String
    , startDate : String
    , initialBalance : Int
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


type alias FinalBalance =
    { yearlyRebalanced : Int
    , noRebalanced : Int
    }


initInputs : Nav.Key -> InputObject.PortfolioStateInputType
initInputs key =
    { start_date = "2013-03-20"
    , initial_balance = 0
    , allocations = [ InputObject.AllocationInputType "" 0 ]
    , save = False
    , token = Absent
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        maybeToken =
            getTokenFromPath url.path
    in
    ( { flags = flags
      , inputs = initInputs key
      , key = key
      , url = url
      , dateNow = fromPosix <| Time.millisToPosix 0
      , startDateInput = Nothing
      , portfolioResult = Nothing
      , mutualization = True
      , loading =
            case maybeToken of
                Just _ ->
                    True

                _ ->
                    False
      , yearlyRebalancing = False
      , finalBalance = { yearlyRebalanced = 0, noRebalanced = 0 }
      }
    , Cmd.batch
        (Task.perform GetTime Time.now
            :: (case maybeToken of
                    Just token ->
                        [ fetchPortfolio flags token ]

                    Nothing ->
                        []
               )
        )
    )



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | ChangeInitialBalance Int
    | ChangeStartDate DateMsg
    | GetTime Time.Posix
    | ChangeSymbol Int String
    | ChangePercentage Int Int
    | AddAllocation
    | CalculateValueToday
    | GotPortfolio (RemoteData (Graphql.Http.Error (Maybe PortfolioResult)) (Maybe PortfolioResult))
    | GotSavedPortfolio (RemoteData (Graphql.Http.Error PortfolioResult) PortfolioResult)
    | ToggleMutualization
    | ToggleYearlyRebalancing


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
                    { model | startDateInput = DateTime.fromRawParts defaultDate { hours = 0, minutes = 0, seconds = 0, milliseconds = 0 } }

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
            ( { model | loading = True }
            , createPortfolioState model.flags { inputs | save = True }
            )

        GotPortfolio receiveData ->
            case receiveData of
                RemoteData.Success maybePortfolioResult ->
                    case maybePortfolioResult of
                        Just portfolioResult ->
                            ( { model
                                | portfolioResult = Just portfolioResult
                                , inputs =
                                    { inputs
                                        | token =
                                            case portfolioResult.token of
                                                Just token ->
                                                    Present token

                                                Nothing ->
                                                    Absent
                                    }
                                , loading = False
                                , finalBalance =
                                    { yearlyRebalanced = calculFinalBalance portfolioResult.initialBalance portfolioResult.allocations
                                    , noRebalanced = List.foldl (+) 0 (List.map (calculAllocationFinalBalance portfolioResult.initialBalance) portfolioResult.allocations)
                                    }
                              }
                            , Cmd.batch
                                [ Nav.replaceUrl model.key (Maybe.withDefault "" portfolioResult.token) ]
                            )

                        Nothing ->
                            ( { model | loading = False }, Cmd.none )

                _ ->
                    ( { model | loading = False }, Cmd.none )

        GotSavedPortfolio receiveData ->
            case receiveData of
                RemoteData.Success portfolioResult ->
                    ( { model
                        | portfolioResult = Just portfolioResult
                        , startDateInput = datetimeFromString portfolioResult.startDate
                        , inputs =
                            { inputs
                                | token =
                                    case portfolioResult.token of
                                        Just token ->
                                            Present token

                                        Nothing ->
                                            Absent
                                , start_date = portfolioResult.startDate
                                , initial_balance = portfolioResult.initialBalance
                                , allocations = List.map (\a -> { percentage = a.percentage, symbol = a.symbol }) portfolioResult.allocations
                                , save = True
                            }
                        , loading = False
                        , finalBalance =
                            { yearlyRebalanced = calculFinalBalance portfolioResult.initialBalance portfolioResult.allocations
                            , noRebalanced = List.foldl (+) 0 (List.map (calculAllocationFinalBalance portfolioResult.initialBalance) portfolioResult.allocations)
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | loading = False }, Cmd.none )

        ToggleMutualization ->
            ( { model | mutualization = not model.mutualization }, Cmd.none )

        ToggleYearlyRebalancing ->
            ( { model | yearlyRebalancing = not model.yearlyRebalancing }, Cmd.none )


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


datetimeFromString : String -> Maybe DateTime.DateTime
datetimeFromString str =
    DateTime.fromRawParts
        (case String.split "-" str of
            a :: b :: c :: [] ->
                { year = Maybe.withDefault 2013 (String.toInt a)
                , month = Maybe.withDefault Time.Jan (intToMonth (Maybe.withDefault 1 (String.toInt b)))
                , day = Maybe.withDefault 20 (String.toInt c)
                }

            _ ->
                defaultDate
        )
        { hours = 0, minutes = 0, seconds = 0, milliseconds = 0 }


defaultDate : { day : Int, month : Time.Month, year : Int }
defaultDate =
    { day = 20, month = Time.Mar, year = 2013 }


createPortfolioState : Flags -> InputObject.PortfolioStateInputType -> Cmd Msg
createPortfolioState { configBackendEndpoint } inputs =
    sendPortfolioState inputs
        |> Graphql.Http.mutationRequest (configBackendEndpoint ++ "graphql")
        |> Graphql.Http.send (RemoteData.fromResult >> GotPortfolio)


sendPortfolioState : InputObject.PortfolioStateInputType -> SelectionSet (Maybe PortfolioResult) RootMutation
sendPortfolioState inputs =
    Mutation.create_portfolio_state { portfolio_state = inputs } portfolioResultSelector


portfolioResultSelector : SelectionSet PortfolioResult Object.PortfolioState
portfolioResultSelector =
    SelectionSet.map4 PortfolioResult
        (PortfolioState.allocations allocationSelector)
        PortfolioState.token
        PortfolioState.start_date
        PortfolioState.initial_balance


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


fetchPortfolio : Flags -> String -> Cmd Msg
fetchPortfolio { configBackendEndpoint } token =
    Query.portfolio_state { id = token } portfolioResultSelector
        |> Graphql.Http.queryRequest (configBackendEndpoint ++ "graphql")
        |> Graphql.Http.send (RemoteData.fromResult >> GotSavedPortfolio)


getTokenFromPath : String -> Maybe String
getTokenFromPath path =
    if String.length path > 10 then
        Just (String.dropLeft 1 path)

    else
        Nothing



-- VIEW


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
    flex
        [ style
            [ Style.flexContainerProperties [ Flex.justifyContent Flex.justifyContentCenter ]
            , Style.block [ Block.fullWidth ]
            , Style.box [ Box.margin [ Margin.all Margin.auto ] ]
            ]
        ]
        [ flexItem
            [ style
                [ Style.block [ Block.width (px 600) ]
                , Style.box [ Box.padding [ Padding.horizontal Constants.medium ] ]
                ]
            ]
            ([ h1
                [ style [ Style.blockProperties [ Block.alignCenter ] ] ]
                [ text "Portfolio Performance Tester" ]
             , buildInputNumber
                (inputLabelPlaceholder "Initial Balance ($)" "1337")
                model.inputs.initial_balance
                ChangeInitialBalance
             , buildDate
                (inputLabelPlaceholder "Start Date" "2013-03-20")
                model.startDateInput
                (DateBetween (fromPosix (Time.millisToPosix 0)) model.dateNow)
                ChangeStartDate
             ]
                ++ buildAllocationsForm model.inputs.allocations
                ++ (if model.loading then
                        [ div [] [ text "...Loading..." ] ]

                    else
                        ([ monochromeSquaredButton
                            { background = Color.white
                            , border = Color.black
                            , text = Color.black
                            }
                            "Add another allocation"
                            AddAllocation
                         , br
                         ]
                            ++ (case ( List.foldl (\a -> \b -> a.percentage + b) 0 model.inputs.allocations, model.inputs.initial_balance > 0 ) of
                                    ( 100, True ) ->
                                        [ br
                                        , monochromeSquaredButton
                                            { background = Color.white
                                            , border = Color.black
                                            , text = Color.black
                                            }
                                            "Click here to value your balance today and save it"
                                            CalculateValueToday
                                        ]

                                    ( i, True ) ->
                                        [ div [] [ text ("Make sure to have 100 percents in total : " ++ String.fromInt i ++ "%") ] ]

                                    _ ->
                                        [ div [] [ text "Make sure to put an initial balance" ] ]
                               )
                        )
                            ++ (case model.portfolioResult of
                                    Just portfolioResult ->
                                        [ br
                                        , showPortfolioResult model portfolioResult
                                        , monochromeSquaredButton
                                            { background = Color.white
                                            , border = Color.black
                                            , text = Color.black
                                            }
                                            ("Toggle mutualization: " ++ onOrOff model.mutualization)
                                            ToggleMutualization
                                        , br
                                        , case portfolioResult.token of
                                            Just token ->
                                                text ("Your portfolio is saved at this url : " ++ model.flags.configFrontendEndpoint ++ token)

                                            Nothing ->
                                                div [] []
                                        ]

                                    Nothing ->
                                        []
                               )
                            ++ [ br
                               , monochromeSquaredButton
                                    { background = Color.white
                                    , border = Color.black
                                    , text = Color.black
                                    }
                                    ("Toggle yearly rebalancing: "
                                        ++ onOrOff model.yearlyRebalancing
                                        ++ (if not model.mutualization then
                                                " - Warning: not working when mutualization is off"

                                            else
                                                ""
                                           )
                                    )
                                    ToggleYearlyRebalancing
                               ]
                   )
            )
        ]


showPortfolioResult : Model -> PortfolioResult -> NodeWithStyle Msg
showPortfolioResult { finalBalance, yearlyRebalancing, mutualization } portfolioResult =
    let
        finalBalancedCalculated =
            if yearlyRebalancing then
                finalBalance.yearlyRebalanced

            else
                finalBalance.noRebalanced
    in
    if mutualization then
        div []
            [ div []
                [ text
                    ("Final Balance : "
                        ++ String.fromInt finalBalancedCalculated
                        ++ "$"
                    )
                ]
            , div []
                [ text
                    (let
                        valueMade =
                            finalBalancedCalculated - portfolioResult.initialBalance
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
            (List.map (showAllocationResult portfolioResult.initialBalance) portfolioResult.allocations)


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

        finalAllocationBalance =
            ratio * initialAllocationBalance

        profit =
            round (finalAllocationBalance - initialAllocationBalance)
    in
    div []
        [ div []
            [ text ("Symbol : " ++ allocation.symbol) ]
        , div []
            [ text ("Initial balance : " ++ String.fromFloat initialAllocationBalance ++ "$") ]
        , div []
            [ text ("Final balance : " ++ String.fromInt (round finalAllocationBalance) ++ "$") ]
        , div []
            [ text
                ("Profit : "
                    ++ (if profit >= 0 then
                            "+"

                        else
                            "-"
                       )
                    ++ String.fromInt profit
                    ++ "$"
                )
            ]

        -- Show all the prices each year
        -- , div [] (List.map (\ppt -> div [] [ text ("date: " ++ ppt.time ++ " price: " ++ String.fromFloat ppt.price) ]) (pricePerTimesWithDifferentYears allocation.price_per_times))
        ]


buildAllocationsForm : List InputObject.AllocationInputType -> List (NodeWithStyle Msg)
buildAllocationsForm list =
    List.indexedMap buildSymbolAndPercentageForm list


buildSymbolAndPercentageForm : Int -> InputObject.AllocationInputType -> NodeWithStyle Msg
buildSymbolAndPercentageForm i a =
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



-- Calculation


calculFinalBalance : Int -> List AllocationResult -> Int
calculFinalBalance initialBalance allocations =
    let
        allocationsWithDifferentYears =
            List.map (\a -> { a | price_per_times = pricePerTimesWithDifferentYears a.price_per_times }) allocations

        transposedPrices =
            transpose
                (List.map (\a -> a.price_per_times) allocationsWithDifferentYears)
    in
    calculTransposed allocations (List.reverse transposedPrices) initialBalance


calculTransposed : List AllocationResult -> List (List PricePerTime) -> Int -> Int
calculTransposed allocations transposedPrices initialBalance =
    case transposedPrices of
        ppt1 :: l ->
            case l of
                ppt2 :: _ ->
                    let
                        newInitialBalance =
                            List.foldl (+) 0 (calculNewYearlyBalance (zip allocations (transpose [ ppt1, ppt2 ])) initialBalance)
                    in
                    calculTransposed allocations l newInitialBalance

                _ ->
                    initialBalance

        _ ->
            initialBalance


calculNewYearlyBalance : List ( AllocationResult, List PricePerTime ) -> Int -> List Int
calculNewYearlyBalance list initialBalance =
    List.map
        (\( { percentage }, pricePerTimeDuo ) ->
            case pricePerTimeDuo of
                old :: new :: [] ->
                    let
                        ratio =
                            new.price / old.price

                        initialAllocationBalance =
                            toFloat (initialBalance * percentage) / 100

                        finalAllocationBalance =
                            ratio * initialAllocationBalance
                    in
                    round finalAllocationBalance

                _ ->
                    0
        )
        list


calculAllocationFinalBalance : Int -> AllocationResult -> Int
calculAllocationFinalBalance initialBalance allocation =
    let
        ratio =
            case ( List.Extra.last allocation.price_per_times, List.head allocation.price_per_times ) of
                ( Just last, Just first ) ->
                    first.price / last.price

                _ ->
                    1

        initialAllocationBalance =
            toFloat (initialBalance * allocation.percentage) / 100

        finalAllocationBalance =
            ratio * initialAllocationBalance
    in
    round finalAllocationBalance


pricePerTimesWithDifferentYears : List PricePerTime -> List PricePerTime
pricePerTimesWithDifferentYears listPrices =
    let
        newList =
            List.map Tuple.first <|
                groupWhile (\a b -> sameDate a.time b.time) listPrices
    in
    newList
        ++ (case ( last newList, last listPrices ) of
                ( Just newListStartPrice, Just startPrice ) ->
                    if newListStartPrice.time == startPrice.time then
                        []

                    else
                        [ startPrice ]

                _ ->
                    []
           )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
