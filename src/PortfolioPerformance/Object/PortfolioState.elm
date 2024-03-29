-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PortfolioPerformance.Object.PortfolioState exposing (..)

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


allocations : SelectionSet decodesTo PortfolioPerformance.Object.Allocation -> SelectionSet (List decodesTo) PortfolioPerformance.Object.PortfolioState
allocations object_ =
    Object.selectionForCompositeField "allocations" [] object_ (identity >> Decode.list)


id : SelectionSet String PortfolioPerformance.Object.PortfolioState
id =
    Object.selectionForField "String" "id" [] Decode.string


initial_balance : SelectionSet Int PortfolioPerformance.Object.PortfolioState
initial_balance =
    Object.selectionForField "Int" "initial_balance" [] Decode.int


start_date : SelectionSet String PortfolioPerformance.Object.PortfolioState
start_date =
    Object.selectionForField "String" "start_date" [] Decode.string


token : SelectionSet (Maybe String) PortfolioPerformance.Object.PortfolioState
token =
    Object.selectionForField "(Maybe String)" "token" [] (Decode.string |> Decode.nullable)
