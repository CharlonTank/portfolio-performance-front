-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PortfolioPerformance.Mutation exposing (..)

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


type alias BulkCreateAllocationRequiredArguments =
    { allocation : List (Maybe PortfolioPerformance.InputObject.AllocationInputType) }


{-| creates some Allocations
-}
bulk_create_allocation : BulkCreateAllocationRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.Allocation -> SelectionSet (Maybe (List (Maybe decodesTo))) RootMutation
bulk_create_allocation requiredArgs object_ =
    Object.selectionForCompositeField "bulk_create_allocation" [ Argument.required "allocation" requiredArgs.allocation (PortfolioPerformance.InputObject.encodeAllocationInputType |> Encode.maybe |> Encode.list) ] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias BulkCreatePortfolioStateRequiredArguments =
    { portfolio_state : List (Maybe PortfolioPerformance.InputObject.PortfolioStateInputType) }


{-| creates some PortfolioStates
-}
bulk_create_portfolio_state : BulkCreatePortfolioStateRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.PortfolioState -> SelectionSet (Maybe (List (Maybe decodesTo))) RootMutation
bulk_create_portfolio_state requiredArgs object_ =
    Object.selectionForCompositeField "bulk_create_portfolio_state" [ Argument.required "portfolio_state" requiredArgs.portfolio_state (PortfolioPerformance.InputObject.encodePortfolioStateInputType |> Encode.maybe |> Encode.list) ] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias BulkCreateSubscribedQueryRequiredArguments =
    { subscribed_query : List (Maybe PortfolioPerformance.InputObject.SubscribedQueryInputType) }


{-| creates some SubscribedQueries
-}
bulk_create_subscribed_query : BulkCreateSubscribedQueryRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.SubscribedQuery -> SelectionSet (Maybe (List (Maybe decodesTo))) RootMutation
bulk_create_subscribed_query requiredArgs object_ =
    Object.selectionForCompositeField "bulk_create_subscribed_query" [ Argument.required "subscribed_query" requiredArgs.subscribed_query (PortfolioPerformance.InputObject.encodeSubscribedQueryInputType |> Encode.maybe |> Encode.list) ] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias BulkCreateUserRequiredArguments =
    { user : List (Maybe PortfolioPerformance.InputObject.UserInputType) }


{-| creates some Users
-}
bulk_create_user : BulkCreateUserRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.User -> SelectionSet (Maybe (List (Maybe decodesTo))) RootMutation
bulk_create_user requiredArgs object_ =
    Object.selectionForCompositeField "bulk_create_user" [ Argument.required "user" requiredArgs.user (PortfolioPerformance.InputObject.encodeUserInputType |> Encode.maybe |> Encode.list) ] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias BulkCreateWebsocketConnectionRequiredArguments =
    { websocket_connection : List (Maybe PortfolioPerformance.InputObject.WebsocketConnectionInputType) }


{-| creates some WebsocketConnections
-}
bulk_create_websocket_connection : BulkCreateWebsocketConnectionRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.WebsocketConnection -> SelectionSet (Maybe (List (Maybe decodesTo))) RootMutation
bulk_create_websocket_connection requiredArgs object_ =
    Object.selectionForCompositeField "bulk_create_websocket_connection" [ Argument.required "websocket_connection" requiredArgs.websocket_connection (PortfolioPerformance.InputObject.encodeWebsocketConnectionInputType |> Encode.maybe |> Encode.list) ] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias BulkUpdateAllocationRequiredArguments =
    { allocation : List (Maybe PortfolioPerformance.InputObject.AllocationInputType) }


{-| Updates some Allocations
-}
bulk_update_allocation : BulkUpdateAllocationRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.Allocation -> SelectionSet (Maybe (List (Maybe decodesTo))) RootMutation
bulk_update_allocation requiredArgs object_ =
    Object.selectionForCompositeField "bulk_update_allocation" [ Argument.required "allocation" requiredArgs.allocation (PortfolioPerformance.InputObject.encodeAllocationInputType |> Encode.maybe |> Encode.list) ] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias BulkUpdatePortfolioStateRequiredArguments =
    { portfolio_state : List (Maybe PortfolioPerformance.InputObject.PortfolioStateInputType) }


{-| Updates some PortfolioStates
-}
bulk_update_portfolio_state : BulkUpdatePortfolioStateRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.PortfolioState -> SelectionSet (Maybe (List (Maybe decodesTo))) RootMutation
bulk_update_portfolio_state requiredArgs object_ =
    Object.selectionForCompositeField "bulk_update_portfolio_state" [ Argument.required "portfolio_state" requiredArgs.portfolio_state (PortfolioPerformance.InputObject.encodePortfolioStateInputType |> Encode.maybe |> Encode.list) ] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias BulkUpdateSubscribedQueryRequiredArguments =
    { subscribed_query : List (Maybe PortfolioPerformance.InputObject.SubscribedQueryInputType) }


{-| Updates some SubscribedQueries
-}
bulk_update_subscribed_query : BulkUpdateSubscribedQueryRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.SubscribedQuery -> SelectionSet (Maybe (List (Maybe decodesTo))) RootMutation
bulk_update_subscribed_query requiredArgs object_ =
    Object.selectionForCompositeField "bulk_update_subscribed_query" [ Argument.required "subscribed_query" requiredArgs.subscribed_query (PortfolioPerformance.InputObject.encodeSubscribedQueryInputType |> Encode.maybe |> Encode.list) ] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias BulkUpdateUserRequiredArguments =
    { user : List (Maybe PortfolioPerformance.InputObject.UserInputType) }


{-| Updates some Users
-}
bulk_update_user : BulkUpdateUserRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.User -> SelectionSet (Maybe (List (Maybe decodesTo))) RootMutation
bulk_update_user requiredArgs object_ =
    Object.selectionForCompositeField "bulk_update_user" [ Argument.required "user" requiredArgs.user (PortfolioPerformance.InputObject.encodeUserInputType |> Encode.maybe |> Encode.list) ] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias BulkUpdateWebsocketConnectionRequiredArguments =
    { websocket_connection : List (Maybe PortfolioPerformance.InputObject.WebsocketConnectionInputType) }


{-| Updates some WebsocketConnections
-}
bulk_update_websocket_connection : BulkUpdateWebsocketConnectionRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.WebsocketConnection -> SelectionSet (Maybe (List (Maybe decodesTo))) RootMutation
bulk_update_websocket_connection requiredArgs object_ =
    Object.selectionForCompositeField "bulk_update_websocket_connection" [ Argument.required "websocket_connection" requiredArgs.websocket_connection (PortfolioPerformance.InputObject.encodeWebsocketConnectionInputType |> Encode.maybe |> Encode.list) ] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias CreateAllocationRequiredArguments =
    { allocation : PortfolioPerformance.InputObject.AllocationInputType }


{-| Creates a Allocation
-}
create_allocation : CreateAllocationRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.Allocation -> SelectionSet (Maybe decodesTo) RootMutation
create_allocation requiredArgs object_ =
    Object.selectionForCompositeField "create_allocation" [ Argument.required "allocation" requiredArgs.allocation PortfolioPerformance.InputObject.encodeAllocationInputType ] object_ (identity >> Decode.nullable)


type alias CreatePortfolioStateRequiredArguments =
    { portfolio_state : PortfolioPerformance.InputObject.PortfolioStateInputType }


{-| Creates a PortfolioState
-}
create_portfolio_state : CreatePortfolioStateRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.PortfolioState -> SelectionSet (Maybe decodesTo) RootMutation
create_portfolio_state requiredArgs object_ =
    Object.selectionForCompositeField "create_portfolio_state" [ Argument.required "portfolio_state" requiredArgs.portfolio_state PortfolioPerformance.InputObject.encodePortfolioStateInputType ] object_ (identity >> Decode.nullable)


type alias CreateSubscribedQueryRequiredArguments =
    { subscribed_query : PortfolioPerformance.InputObject.SubscribedQueryInputType }


{-| Creates a SubscribedQuery
-}
create_subscribed_query : CreateSubscribedQueryRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.SubscribedQuery -> SelectionSet (Maybe decodesTo) RootMutation
create_subscribed_query requiredArgs object_ =
    Object.selectionForCompositeField "create_subscribed_query" [ Argument.required "subscribed_query" requiredArgs.subscribed_query PortfolioPerformance.InputObject.encodeSubscribedQueryInputType ] object_ (identity >> Decode.nullable)


type alias CreateUserRequiredArguments =
    { user : PortfolioPerformance.InputObject.UserInputType }


{-| Creates a User
-}
create_user : CreateUserRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.User -> SelectionSet (Maybe decodesTo) RootMutation
create_user requiredArgs object_ =
    Object.selectionForCompositeField "create_user" [ Argument.required "user" requiredArgs.user PortfolioPerformance.InputObject.encodeUserInputType ] object_ (identity >> Decode.nullable)


type alias CreateWebsocketConnectionRequiredArguments =
    { websocket_connection : PortfolioPerformance.InputObject.WebsocketConnectionInputType }


{-| Creates a WebsocketConnection
-}
create_websocket_connection : CreateWebsocketConnectionRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.WebsocketConnection -> SelectionSet (Maybe decodesTo) RootMutation
create_websocket_connection requiredArgs object_ =
    Object.selectionForCompositeField "create_websocket_connection" [ Argument.required "websocket_connection" requiredArgs.websocket_connection PortfolioPerformance.InputObject.encodeWebsocketConnectionInputType ] object_ (identity >> Decode.nullable)


type alias DestroyAllocationRequiredArguments =
    { id : String }


{-| Destroys a Allocation
-}
destroy_allocation : DestroyAllocationRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.Allocation -> SelectionSet (Maybe decodesTo) RootMutation
destroy_allocation requiredArgs object_ =
    Object.selectionForCompositeField "destroy_allocation" [ Argument.required "id" requiredArgs.id Encode.string ] object_ (identity >> Decode.nullable)


type alias DestroyPortfolioStateRequiredArguments =
    { id : String }


{-| Destroys a PortfolioState
-}
destroy_portfolio_state : DestroyPortfolioStateRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.PortfolioState -> SelectionSet (Maybe decodesTo) RootMutation
destroy_portfolio_state requiredArgs object_ =
    Object.selectionForCompositeField "destroy_portfolio_state" [ Argument.required "id" requiredArgs.id Encode.string ] object_ (identity >> Decode.nullable)


type alias DestroySubscribedQueryRequiredArguments =
    { id : String }


{-| Destroys a SubscribedQuery
-}
destroy_subscribed_query : DestroySubscribedQueryRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.SubscribedQuery -> SelectionSet (Maybe decodesTo) RootMutation
destroy_subscribed_query requiredArgs object_ =
    Object.selectionForCompositeField "destroy_subscribed_query" [ Argument.required "id" requiredArgs.id Encode.string ] object_ (identity >> Decode.nullable)


type alias DestroyUserRequiredArguments =
    { id : String }


{-| Destroys a User
-}
destroy_user : DestroyUserRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.User -> SelectionSet (Maybe decodesTo) RootMutation
destroy_user requiredArgs object_ =
    Object.selectionForCompositeField "destroy_user" [ Argument.required "id" requiredArgs.id Encode.string ] object_ (identity >> Decode.nullable)


type alias DestroyWebsocketConnectionRequiredArguments =
    { id : String }


{-| Destroys a WebsocketConnection
-}
destroy_websocket_connection : DestroyWebsocketConnectionRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.WebsocketConnection -> SelectionSet (Maybe decodesTo) RootMutation
destroy_websocket_connection requiredArgs object_ =
    Object.selectionForCompositeField "destroy_websocket_connection" [ Argument.required "id" requiredArgs.id Encode.string ] object_ (identity >> Decode.nullable)


type alias UpdateAllocationOptionalArguments =
    { id : OptionalArgument String }


type alias UpdateAllocationRequiredArguments =
    { allocation : PortfolioPerformance.InputObject.AllocationInputType }


{-| Updates a Allocation
-}
update_allocation : (UpdateAllocationOptionalArguments -> UpdateAllocationOptionalArguments) -> UpdateAllocationRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.Allocation -> SelectionSet (Maybe decodesTo) RootMutation
update_allocation fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { id = Absent }

        optionalArgs =
            [ Argument.optional "id" filledInOptionals.id Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "update_allocation" (optionalArgs ++ [ Argument.required "allocation" requiredArgs.allocation PortfolioPerformance.InputObject.encodeAllocationInputType ]) object_ (identity >> Decode.nullable)


type alias UpdatePortfolioStateOptionalArguments =
    { id : OptionalArgument String }


type alias UpdatePortfolioStateRequiredArguments =
    { portfolio_state : PortfolioPerformance.InputObject.PortfolioStateInputType }


{-| Updates a PortfolioState
-}
update_portfolio_state : (UpdatePortfolioStateOptionalArguments -> UpdatePortfolioStateOptionalArguments) -> UpdatePortfolioStateRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.PortfolioState -> SelectionSet (Maybe decodesTo) RootMutation
update_portfolio_state fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { id = Absent }

        optionalArgs =
            [ Argument.optional "id" filledInOptionals.id Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "update_portfolio_state" (optionalArgs ++ [ Argument.required "portfolio_state" requiredArgs.portfolio_state PortfolioPerformance.InputObject.encodePortfolioStateInputType ]) object_ (identity >> Decode.nullable)


type alias UpdateSubscribedQueryOptionalArguments =
    { id : OptionalArgument String }


type alias UpdateSubscribedQueryRequiredArguments =
    { subscribed_query : PortfolioPerformance.InputObject.SubscribedQueryInputType }


{-| Updates a SubscribedQuery
-}
update_subscribed_query : (UpdateSubscribedQueryOptionalArguments -> UpdateSubscribedQueryOptionalArguments) -> UpdateSubscribedQueryRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.SubscribedQuery -> SelectionSet (Maybe decodesTo) RootMutation
update_subscribed_query fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { id = Absent }

        optionalArgs =
            [ Argument.optional "id" filledInOptionals.id Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "update_subscribed_query" (optionalArgs ++ [ Argument.required "subscribed_query" requiredArgs.subscribed_query PortfolioPerformance.InputObject.encodeSubscribedQueryInputType ]) object_ (identity >> Decode.nullable)


type alias UpdateUserOptionalArguments =
    { id : OptionalArgument String }


type alias UpdateUserRequiredArguments =
    { user : PortfolioPerformance.InputObject.UserInputType }


{-| Updates a User
-}
update_user : (UpdateUserOptionalArguments -> UpdateUserOptionalArguments) -> UpdateUserRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.User -> SelectionSet (Maybe decodesTo) RootMutation
update_user fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { id = Absent }

        optionalArgs =
            [ Argument.optional "id" filledInOptionals.id Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "update_user" (optionalArgs ++ [ Argument.required "user" requiredArgs.user PortfolioPerformance.InputObject.encodeUserInputType ]) object_ (identity >> Decode.nullable)


type alias UpdateWebsocketConnectionOptionalArguments =
    { id : OptionalArgument String }


type alias UpdateWebsocketConnectionRequiredArguments =
    { websocket_connection : PortfolioPerformance.InputObject.WebsocketConnectionInputType }


{-| Updates a WebsocketConnection
-}
update_websocket_connection : (UpdateWebsocketConnectionOptionalArguments -> UpdateWebsocketConnectionOptionalArguments) -> UpdateWebsocketConnectionRequiredArguments -> SelectionSet decodesTo PortfolioPerformance.Object.WebsocketConnection -> SelectionSet (Maybe decodesTo) RootMutation
update_websocket_connection fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { id = Absent }

        optionalArgs =
            [ Argument.optional "id" filledInOptionals.id Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "update_websocket_connection" (optionalArgs ++ [ Argument.required "websocket_connection" requiredArgs.websocket_connection PortfolioPerformance.InputObject.encodeWebsocketConnectionInputType ]) object_ (identity >> Decode.nullable)
