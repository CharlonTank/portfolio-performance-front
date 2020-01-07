-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PortfolioPerformance.Query exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import PortfolioPerformance.InputObject
import PortfolioPerformance.Interface
import PortfolioPerformance.Object
import PortfolioPerformance.Scalar
import PortfolioPerformance.ScalarCodecs
import PortfolioPerformance.Union


type alias AllocationRequiredArguments =
    { id : String }


{-| Returns a Allocation
-}
allocation : AllocationRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.Allocation -> SelectionSet decodesTo RootQuery
allocation requiredArgs object_ =
    Object.selectionForCompositeField "allocation" [ Argument.required "id" requiredArgs.id Encode.string ] object_ identity


type alias AllocationsOptionalArguments =
    { page : OptionalArgument Int
    , per_page : OptionalArgument Int
    , filter : OptionalArgument String
    , order_by : OptionalArgument String
    }


{-| Returns a Allocation
-}
allocations : (AllocationsOptionalArguments -> AllocationsOptionalArguments) -> SelectionSet decodesTo PortfolioPerformance.Object.Allocation -> SelectionSet (List decodesTo) RootQuery
allocations fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { page = Absent, per_page = Absent, filter = Absent, order_by = Absent }

        optionalArgs =
            [ Argument.optional "page" filledInOptionals.page Encode.int, Argument.optional "per_page" filledInOptionals.per_page Encode.int, Argument.optional "filter" filledInOptionals.filter Encode.string, Argument.optional "order_by" filledInOptionals.order_by Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "allocations" optionalArgs object_ (identity >> Decode.list)


{-| Returns the current user
-}
me : SelectionSet decodesTo PortfolioPerformance.Object.User -> SelectionSet (Maybe decodesTo) RootQuery
me object_ =
    Object.selectionForCompositeField "me" [] object_ (identity >> Decode.nullable)


type alias PortfolioStateRequiredArguments =
    { id : String }


{-| Returns a PortfolioState
-}
portfolio_state : PortfolioStateRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.PortfolioState -> SelectionSet decodesTo RootQuery
portfolio_state requiredArgs object_ =
    Object.selectionForCompositeField "portfolio_state" [ Argument.required "id" requiredArgs.id Encode.string ] object_ identity


type alias PortfolioStatesOptionalArguments =
    { page : OptionalArgument Int
    , per_page : OptionalArgument Int
    , filter : OptionalArgument String
    , order_by : OptionalArgument String
    }


{-| Returns a PortfolioState
-}
portfolio_states : (PortfolioStatesOptionalArguments -> PortfolioStatesOptionalArguments) -> SelectionSet decodesTo PortfolioPerformance.Object.PortfolioState -> SelectionSet (List decodesTo) RootQuery
portfolio_states fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { page = Absent, per_page = Absent, filter = Absent, order_by = Absent }

        optionalArgs =
            [ Argument.optional "page" filledInOptionals.page Encode.int, Argument.optional "per_page" filledInOptionals.per_page Encode.int, Argument.optional "filter" filledInOptionals.filter Encode.string, Argument.optional "order_by" filledInOptionals.order_by Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "portfolio_states" optionalArgs object_ (identity >> Decode.list)


type alias PricePerTimeRequiredArguments =
    { id : String }


{-| Returns a PricePerTime
-}
price_per_time : PricePerTimeRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.PricePerTime -> SelectionSet decodesTo RootQuery
price_per_time requiredArgs object_ =
    Object.selectionForCompositeField "price_per_time" [ Argument.required "id" requiredArgs.id Encode.string ] object_ identity


type alias PricePerTimesOptionalArguments =
    { page : OptionalArgument Int
    , per_page : OptionalArgument Int
    , filter : OptionalArgument String
    , order_by : OptionalArgument String
    }


{-| Returns a PricePerTime
-}
price_per_times : (PricePerTimesOptionalArguments -> PricePerTimesOptionalArguments) -> SelectionSet decodesTo PortfolioPerformance.Object.PricePerTime -> SelectionSet (List decodesTo) RootQuery
price_per_times fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { page = Absent, per_page = Absent, filter = Absent, order_by = Absent }

        optionalArgs =
            [ Argument.optional "page" filledInOptionals.page Encode.int, Argument.optional "per_page" filledInOptionals.per_page Encode.int, Argument.optional "filter" filledInOptionals.filter Encode.string, Argument.optional "order_by" filledInOptionals.order_by Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "price_per_times" optionalArgs object_ (identity >> Decode.list)


type alias SubscribedQueriesOptionalArguments =
    { page : OptionalArgument Int
    , per_page : OptionalArgument Int
    , filter : OptionalArgument String
    , order_by : OptionalArgument String
    }


{-| Returns a SubscribedQuery
-}
subscribed_queries : (SubscribedQueriesOptionalArguments -> SubscribedQueriesOptionalArguments) -> SelectionSet decodesTo PortfolioPerformance.Object.SubscribedQuery -> SelectionSet (List decodesTo) RootQuery
subscribed_queries fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { page = Absent, per_page = Absent, filter = Absent, order_by = Absent }

        optionalArgs =
            [ Argument.optional "page" filledInOptionals.page Encode.int, Argument.optional "per_page" filledInOptionals.per_page Encode.int, Argument.optional "filter" filledInOptionals.filter Encode.string, Argument.optional "order_by" filledInOptionals.order_by Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "subscribed_queries" optionalArgs object_ (identity >> Decode.list)


type alias SubscribedQueryRequiredArguments =
    { id : String }


{-| Returns a SubscribedQuery
-}
subscribed_query : SubscribedQueryRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.SubscribedQuery -> SelectionSet decodesTo RootQuery
subscribed_query requiredArgs object_ =
    Object.selectionForCompositeField "subscribed_query" [ Argument.required "id" requiredArgs.id Encode.string ] object_ identity


type alias UserRequiredArguments =
    { id : String }


{-| Returns a User
-}
user : UserRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.User -> SelectionSet decodesTo RootQuery
user requiredArgs object_ =
    Object.selectionForCompositeField "user" [ Argument.required "id" requiredArgs.id Encode.string ] object_ identity


type alias UsersOptionalArguments =
    { page : OptionalArgument Int
    , per_page : OptionalArgument Int
    , filter : OptionalArgument String
    , order_by : OptionalArgument String
    }


{-| Returns a User
-}
users : (UsersOptionalArguments -> UsersOptionalArguments) -> SelectionSet decodesTo PortfolioPerformance.Object.User -> SelectionSet (List decodesTo) RootQuery
users fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { page = Absent, per_page = Absent, filter = Absent, order_by = Absent }

        optionalArgs =
            [ Argument.optional "page" filledInOptionals.page Encode.int, Argument.optional "per_page" filledInOptionals.per_page Encode.int, Argument.optional "filter" filledInOptionals.filter Encode.string, Argument.optional "order_by" filledInOptionals.order_by Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "users" optionalArgs object_ (identity >> Decode.list)


type alias WebsocketConnectionRequiredArguments =
    { id : String }


{-| Returns a WebsocketConnection
-}
websocket_connection : WebsocketConnectionRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.WebsocketConnection -> SelectionSet decodesTo RootQuery
websocket_connection requiredArgs object_ =
    Object.selectionForCompositeField "websocket_connection" [ Argument.required "id" requiredArgs.id Encode.string ] object_ identity


type alias WebsocketConnectionsOptionalArguments =
    { page : OptionalArgument Int
    , per_page : OptionalArgument Int
    , filter : OptionalArgument String
    , order_by : OptionalArgument String
    }


{-| Returns a WebsocketConnection
-}
websocket_connections : (WebsocketConnectionsOptionalArguments -> WebsocketConnectionsOptionalArguments) -> SelectionSet decodesTo PortfolioPerformance.Object.WebsocketConnection -> SelectionSet (List decodesTo) RootQuery
websocket_connections fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { page = Absent, per_page = Absent, filter = Absent, order_by = Absent }

        optionalArgs =
            [ Argument.optional "page" filledInOptionals.page Encode.int, Argument.optional "per_page" filledInOptionals.per_page Encode.int, Argument.optional "filter" filledInOptionals.filter Encode.string, Argument.optional "order_by" filledInOptionals.order_by Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "websocket_connections" optionalArgs object_ (identity >> Decode.list)
