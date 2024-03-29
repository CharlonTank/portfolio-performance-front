-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PortfolioPerformance.Object.Allocation exposing (..)

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


percentage : SelectionSet Int PortfolioPerformance.Object.Allocation
percentage =
    Object.selectionForField "Int" "percentage" [] Decode.int


price_per_times : SelectionSet decodesTo PortfolioPerformance.Object.PricePerTime -> SelectionSet (List decodesTo) PortfolioPerformance.Object.Allocation
price_per_times object_ =
    Object.selectionForCompositeField "price_per_times" [] object_ (identity >> Decode.list)


symbol : SelectionSet String PortfolioPerformance.Object.Allocation
symbol =
    Object.selectionForField "String" "symbol" [] Decode.string
