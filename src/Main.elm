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
import List.Extra exposing (..)
import Modifiers exposing (..)
import Task
import Time
import Url
import Utils exposing (findBy, gray, intToMonth, textToHtml)


type alias Model =
    { inputs : Inputs
    , key : Nav.Key
    , url : Url.Url
    , zone : Time.Zone
    , dateNow : DateTime.DateTime
    }


type alias Inputs =
    { startDate : Maybe DateTime.DateTime
    , initialBalance : Int
    , allocations : List Allocation
    }


type alias Allocation =
    { symbol : String
    , percentage : Int
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
                model.inputs.initialBalance
                ChangeInitialBalance
             , buildDate
                (inputLabelPlaceholder "Start Date" "2013-03-20")
                model.inputs.startDate
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
            )
        ]


buildMutilpleInputText : List Allocation -> List (NodeWithStyle Msg)
buildMutilpleInputText list =
    List.indexedMap buildSymbolAndPercentage list


buildSymbolAndPercentage : Int -> Allocation -> NodeWithStyle Msg
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
            ( { model | inputs = { inputs | initialBalance = nb } }
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
            ( case ( model.inputs.startDate, dateMsg ) of
                ( Just date, Day day ) ->
                    { model | inputs = { inputs | startDate = DateTime.setDay day date } }

                ( Just date, Month nb ) ->
                    case intToMonth nb of
                        Just month ->
                            { model | inputs = { inputs | startDate = DateTime.setMonth month date } }

                        Nothing ->
                            model

                ( Just date, Year nb ) ->
                    { model | inputs = { inputs | startDate = DateTime.setYear nb date } }

                ( _, SetDefaultDate ) ->
                    { model | inputs = { inputs | startDate = DateTime.fromRawParts { day = 20, month = Time.Mar, year = 2013 } { hours = 0, minutes = 0, seconds = 0, milliseconds = 0 } } }

                ( _, RemoveDate ) ->
                    { model | inputs = { inputs | startDate = Nothing } }

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
            ( { model | inputs = { inputs | allocations = inputs.allocations ++ [ Allocation "" 0 ] } }, Cmd.none )

        CalculateValueToday ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- , publishedAt = Just <| Time.millisToPosix 1502323200


initInputs : Nav.Key -> Inputs
initInputs key =
    { startDate = Nothing
    , initialBalance = 0
    , allocations = [ Allocation "" 0 ]
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { inputs = initInputs key
      , key = key
      , url = url
      , zone = Time.utc
      , dateNow = fromPosix <| Time.millisToPosix 0
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
