-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PortfolioPerformance.Object.WebsocketConnection exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import PortfolioPerformance.InputObject
import PortfolioPerformance.Interface
import PortfolioPerformance.Object
import PortfolioPerformance.Scalar
import PortfolioPerformance.ScalarCodecs
import PortfolioPerformance.Union


connection_identifier : SelectionSet (Maybe String) PortfolioPerformance.Object.WebsocketConnection
connection_identifier =
    Object.selectionForField "(Maybe String)" "connection_identifier" [] (Decode.string |> Decode.nullable)


created_at : SelectionSet (Maybe String) PortfolioPerformance.Object.WebsocketConnection
created_at =
    Object.selectionForField "(Maybe String)" "created_at" [] (Decode.string |> Decode.nullable)


id : SelectionSet String PortfolioPerformance.Object.WebsocketConnection
id =
    Object.selectionForField "String" "id" [] Decode.string


subscribed_queries : SelectionSet decodesTo PortfolioPerformance.Object.SubscribedQuery -> SelectionSet (Maybe (List (Maybe decodesTo))) PortfolioPerformance.Object.WebsocketConnection
subscribed_queries object_ =
    Object.selectionForCompositeField "subscribed_queries" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


subscribed_query_ids : SelectionSet (Maybe (List (Maybe String))) PortfolioPerformance.Object.WebsocketConnection
subscribed_query_ids =
    Object.selectionForField "(Maybe (List (Maybe String)))" "subscribed_query_ids" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


updated_at : SelectionSet (Maybe String) PortfolioPerformance.Object.WebsocketConnection
updated_at =
    Object.selectionForField "(Maybe String)" "updated_at" [] (Decode.string |> Decode.nullable)


user : SelectionSet decodesTo PortfolioPerformance.Object.User -> SelectionSet (Maybe decodesTo) PortfolioPerformance.Object.WebsocketConnection
user object_ =
    Object.selectionForCompositeField "user" [] object_ (identity >> Decode.nullable)


user_id : SelectionSet (Maybe String) PortfolioPerformance.Object.WebsocketConnection
user_id =
    Object.selectionForField "(Maybe String)" "user_id" [] (Decode.string |> Decode.nullable)