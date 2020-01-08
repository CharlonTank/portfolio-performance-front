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
import Utils exposing (findBy, gray, intToMonth, normalizeIntForDate, textToHtml)



--MAIN--


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



-- MODEL


type alias Model =
    { inputs : InputObject.PortfolioStateInputType
    , key : Nav.Key
    , url : Url.Url
    , dateNow : DateTime.DateTime
    , startDateInput : Maybe DateTime.DateTime
    , portfolioResult : Maybe PortfolioResult
    , mutualization : Bool
    }


type alias PortfolioResult =
    { finalBalance : Int
    , allocations : List AllocationResult
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


initInputs : Nav.Key -> InputObject.PortfolioStateInputType
initInputs key =
    { start_date = "2013-03-20"
    , initial_balance = 0
    , allocations = [ InputObject.AllocationInputType "" 0 ]
    , save = False
    , token = Absent
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { inputs = initInputs key
      , key = key
      , url = url
      , dateNow = fromPosix <| Time.millisToPosix 0
      , startDateInput = Nothing
      , portfolioResult = Nothing
      , mutualization = True
      }
    , Cmd.batch
        (Task.perform GetTime Time.now
            :: (case getTokenFromPath url.path of
                    Just token ->
                        [ fetchPortfolio token ]

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
            ( model
            , createPortfolioState { inputs | save = True }
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
                              }
                            , Cmd.batch
                                [ Nav.replaceUrl model.key (Maybe.withDefault "" portfolioResult.token) ]
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
                      }
                    , Cmd.none
                    )

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


createPortfolioState : InputObject.PortfolioStateInputType -> Cmd Msg
createPortfolioState inputs =
    sendPortfolioState inputs
        |> Graphql.Http.mutationRequest (backendEndPoint ++ "graphql")
        |> Graphql.Http.send (RemoteData.fromResult >> GotPortfolio)


sendPortfolioState : InputObject.PortfolioStateInputType -> SelectionSet (Maybe PortfolioResult) RootMutation
sendPortfolioState inputs =
    Mutation.create_portfolio_state { portfolio_state = inputs } portfolioResultSelector


portfolioResultSelector : SelectionSet PortfolioResult Object.PortfolioState
portfolioResultSelector =
    SelectionSet.map5 PortfolioResult
        PortfolioState.final_balance
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


fetchPortfolio : String -> Cmd Msg
fetchPortfolio token =
    Query.portfolio_state { id = token } portfolioResultSelector
        |> Graphql.Http.queryRequest (backendEndPoint ++ "graphql")
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
                        "Add another allocation"
                        AddAllocation
                        :: (case List.foldl (\a -> \b -> a.percentage + b) 0 model.inputs.allocations of
                                100 ->
                                    [ monochromeSquaredButton
                                        { background = Color.white
                                        , border = Color.black
                                        , text = Color.black
                                        }
                                        "Click here to value your balance today and save it"
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
                            , case portfolioResult.token of
                                Just token ->
                                    text ("Your portfolio is saved at this url : " ++ frontendEndPoint ++ token)

                                Nothing ->
                                    div [] []
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
                            portfolioResult.finalBalance - portfolioResult.initialBalance
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

        finalAllocationBalance =
            ratio * initialAllocationBalance
    in
    div []
        [ div []
            [ text ("Symbol : " ++ allocation.symbol) ]
        , div []
            [ text ("Initial balance : " ++ String.fromFloat initialAllocationBalance) ]
        , div []
            [ text ("Final balance : " ++ String.fromFloat finalAllocationBalance) ]
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- ENV


backendEndPoint : String
backendEndPoint =
    "https://portfolio-performance-api.herokuapp.com/"


frontendEndPoint : String
frontendEndPoint =
    "https://adoring-bohr-d3c27b.netlify.com/"
