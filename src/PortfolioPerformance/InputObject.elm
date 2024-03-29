-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PortfolioPerformance.InputObject exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import PortfolioPerformance.Interface
import PortfolioPerformance.Object
import PortfolioPerformance.Scalar
import PortfolioPerformance.ScalarCodecs
import PortfolioPerformance.Union


buildAllocationInputType : AllocationInputTypeRequiredFields -> AllocationInputType
buildAllocationInputType required =
    { symbol = required.symbol, percentage = required.percentage }


type alias AllocationInputTypeRequiredFields =
    { symbol : String
    , percentage : Int
    }


{-| Type for the AllocationInputType input object.
-}
type alias AllocationInputType =
    { symbol : String
    , percentage : Int
    }


{-| Encode a AllocationInputType into a value that can be used as an argument.
-}
encodeAllocationInputType : AllocationInputType -> Value
encodeAllocationInputType input =
    Encode.maybeObject
        [ ( "symbol", Encode.string input.symbol |> Just ), ( "percentage", Encode.int input.percentage |> Just ) ]


buildPortfolioStateInputType : PortfolioStateInputTypeRequiredFields -> (PortfolioStateInputTypeOptionalFields -> PortfolioStateInputTypeOptionalFields) -> PortfolioStateInputType
buildPortfolioStateInputType required fillOptionals =
    let
        optionals =
            fillOptionals
                { token = Absent }
    in
    { allocations = required.allocations, initial_balance = required.initial_balance, start_date = required.start_date, save = required.save, token = optionals.token }


type alias PortfolioStateInputTypeRequiredFields =
    { allocations : List AllocationInputType
    , initial_balance : Int
    , start_date : String
    , save : Bool
    }


type alias PortfolioStateInputTypeOptionalFields =
    { token : OptionalArgument String }


{-| Type for the PortfolioStateInputType input object.
-}
type alias PortfolioStateInputType =
    { allocations : List AllocationInputType
    , initial_balance : Int
    , start_date : String
    , save : Bool
    , token : OptionalArgument String
    }


{-| Encode a PortfolioStateInputType into a value that can be used as an argument.
-}
encodePortfolioStateInputType : PortfolioStateInputType -> Value
encodePortfolioStateInputType input =
    Encode.maybeObject
        [ ( "allocations", (encodeAllocationInputType |> Encode.list) input.allocations |> Just ), ( "initial_balance", Encode.int input.initial_balance |> Just ), ( "start_date", Encode.string input.start_date |> Just ), ( "save", Encode.bool input.save |> Just ), ( "token", Encode.string |> Encode.optional input.token ) ]


buildSubscribedQueryInputType : (SubscribedQueryInputTypeOptionalFields -> SubscribedQueryInputTypeOptionalFields) -> SubscribedQueryInputType
buildSubscribedQueryInputType fillOptionals =
    let
        optionals =
            fillOptionals
                { websocket_connection_id = Absent, result_hash = Absent, query = Absent }
    in
    { websocket_connection_id = optionals.websocket_connection_id, result_hash = optionals.result_hash, query = optionals.query }


type alias SubscribedQueryInputTypeOptionalFields =
    { websocket_connection_id : OptionalArgument String
    , result_hash : OptionalArgument String
    , query : OptionalArgument String
    }


{-| Type for the SubscribedQueryInputType input object.
-}
type alias SubscribedQueryInputType =
    { websocket_connection_id : OptionalArgument String
    , result_hash : OptionalArgument String
    , query : OptionalArgument String
    }


{-| Encode a SubscribedQueryInputType into a value that can be used as an argument.
-}
encodeSubscribedQueryInputType : SubscribedQueryInputType -> Value
encodeSubscribedQueryInputType input =
    Encode.maybeObject
        [ ( "websocket_connection_id", Encode.string |> Encode.optional input.websocket_connection_id ), ( "result_hash", Encode.string |> Encode.optional input.result_hash ), ( "query", Encode.string |> Encode.optional input.query ) ]


buildUserInputType : (UserInputTypeOptionalFields -> UserInputTypeOptionalFields) -> UserInputType
buildUserInputType fillOptionals =
    let
        optionals =
            fillOptionals
                { websocket_connection_ids = Absent, first_name = Absent, last_name = Absent, email = Absent }
    in
    { websocket_connection_ids = optionals.websocket_connection_ids, first_name = optionals.first_name, last_name = optionals.last_name, email = optionals.email }


type alias UserInputTypeOptionalFields =
    { websocket_connection_ids : OptionalArgument (List (Maybe String))
    , first_name : OptionalArgument String
    , last_name : OptionalArgument String
    , email : OptionalArgument String
    }


{-| Type for the UserInputType input object.
-}
type alias UserInputType =
    { websocket_connection_ids : OptionalArgument (List (Maybe String))
    , first_name : OptionalArgument String
    , last_name : OptionalArgument String
    , email : OptionalArgument String
    }


{-| Encode a UserInputType into a value that can be used as an argument.
-}
encodeUserInputType : UserInputType -> Value
encodeUserInputType input =
    Encode.maybeObject
        [ ( "websocket_connection_ids", (Encode.string |> Encode.maybe |> Encode.list) |> Encode.optional input.websocket_connection_ids ), ( "first_name", Encode.string |> Encode.optional input.first_name ), ( "last_name", Encode.string |> Encode.optional input.last_name ), ( "email", Encode.string |> Encode.optional input.email ) ]


buildWebsocketConnectionInputType : (WebsocketConnectionInputTypeOptionalFields -> WebsocketConnectionInputTypeOptionalFields) -> WebsocketConnectionInputType
buildWebsocketConnectionInputType fillOptionals =
    let
        optionals =
            fillOptionals
                { subscribed_query_ids = Absent, user_id = Absent, connection_identifier = Absent }
    in
    { subscribed_query_ids = optionals.subscribed_query_ids, user_id = optionals.user_id, connection_identifier = optionals.connection_identifier }


type alias WebsocketConnectionInputTypeOptionalFields =
    { subscribed_query_ids : OptionalArgument (List (Maybe String))
    , user_id : OptionalArgument String
    , connection_identifier : OptionalArgument String
    }


{-| Type for the WebsocketConnectionInputType input object.
-}
type alias WebsocketConnectionInputType =
    { subscribed_query_ids : OptionalArgument (List (Maybe String))
    , user_id : OptionalArgument String
    , connection_identifier : OptionalArgument String
    }


{-| Encode a WebsocketConnectionInputType into a value that can be used as an argument.
-}
encodeWebsocketConnectionInputType : WebsocketConnectionInputType -> Value
encodeWebsocketConnectionInputType input =
    Encode.maybeObject
        [ ( "subscribed_query_ids", (Encode.string |> Encode.maybe |> Encode.list) |> Encode.optional input.subscribed_query_ids ), ( "user_id", Encode.string |> Encode.optional input.user_id ), ( "connection_identifier", Encode.string |> Encode.optional input.connection_identifier ) ]
