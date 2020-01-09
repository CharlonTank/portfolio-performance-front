module OrientedLayout exposing (..)

import BodyBuilder as B exposing (NodeWithStyle)
import BodyBuilder.Attributes as A
import BodyBuilder.Events as E
import BodyBuilder.Extra as Layout
import BodyBuilder.Style as Style
import Browser
import Browser.Navigation as Nav
import Color
import DateTime
import Dict
import Elegant exposing (px)
import Elegant.Background as Background
import Elegant.Block as Block
import Elegant.Border as Border
import Elegant.Box as Box
import Elegant.Constants as Constants
import Elegant.Display as Display
import Elegant.Extra
    exposing
        ( alignCenter
        , block
        , blockProperties
        , blockWithWidth
        , bold
        , border
        , box
        , cursorPointer
        , displayBlock
        , fillHeight
        , flexContainerProperties
        , flexItemProperties
        , fontSize
        , grow
        , padding
        , paddingAll
        , paddingBottom
        , paddingHorizontal
        , paddingTop
        , paddingVertical
        , textColor
        , typoSize
        , typography
        )
import Elegant.Flex as Flex
import Elegant.Margin as Margin
import Elegant.Overflow as Overflow
import Elegant.Padding as Padding
import Elegant.Position as Position
import Elegant.Shadow as Shadow
import Elegant.Typography as Typography
import Html
import Ionicon.Android
import Ionicon.Ios
import Ionicon.Social
import Json.Decode
import Json.Encode
import List.Extra
import Modifiers exposing (Modifier)
import Task
import Time exposing (Weekday(..))
import Url
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


type alias Cell msg =
    { size : List (Modifier Flex.FlexItemDetails)
    , attrs : List (A.FlexItemAttributes msg -> A.FlexItemAttributes msg)
    , content : NodeWithStyle msg
    }


centeredContent : Flex.FlexContainerDetails -> Flex.FlexContainerDetails
centeredContent =
    Flex.align Flex.alignCenter


rowWithOptions :
    -- List (Flex.FlexContainerCoordinate -> Flex.FlexContainerCoordinate)
    List (Flex.FlexContainerDetails -> Flex.FlexContainerDetails)
    -> List (A.FlexContainerAttributes msg -> A.FlexContainerAttributes msg)
    -> List (Cell msg)
    -> NodeWithStyle msg
rowWithOptions =
    layoutWithOptions Flex.row


centeredRow :
    List
        (A.FlexContainerAttributes msg
         -> A.FlexContainerAttributes msg
        )
    -> List (Cell msg)
    -> NodeWithStyle msg
centeredRow =
    rowWithOptions [ centeredContent ]


row :
    List
        (A.FlexContainerAttributes msg
         -> A.FlexContainerAttributes msg
        )
    -> List (Cell msg)
    -> NodeWithStyle msg
row =
    rowWithOptions []


layoutWithOptions direction flexAttrs attrs flexItems =
    B.flex
        ([ displayBlock
         , fillHeight
         , flexContainerProperties
            (Flex.direction direction :: flexAttrs)
         ]
            ++ attrs
        )
        (flexItems
            |> List.map
                (\flexItem ->
                    B.flexItem
                        (flexItem.attrs
                            ++ [ flexItemProperties
                                    flexItem.size
                               ]
                        )
                        [ flexItem.content ]
                )
        )


columnWithOptions :
    -- List (Flex.FlexContainerCoordinate -> Flex.FlexContainerCoordinate)
    List (Flex.FlexContainerDetails -> Flex.FlexContainerDetails)
    -> List (A.FlexContainerAttributes msg -> A.FlexContainerAttributes msg)
    -> List (Cell msg)
    -> NodeWithStyle msg
columnWithOptions =
    layoutWithOptions Flex.column


centeredColumn :
    List
        (A.FlexContainerAttributes msg
         -> A.FlexContainerAttributes msg
        )
    -> List (Cell msg)
    -> NodeWithStyle msg
centeredColumn =
    columnWithOptions [ centeredContent ]


column :
    List
        (A.FlexContainerAttributes msg
         -> A.FlexContainerAttributes msg
        )
    -> List (Cell msg)
    -> NodeWithStyle msg
column =
    columnWithOptions []


cell valType attrs content =
    Cell valType attrs content


pxCell pxSize =
    cell [ Flex.basis (px pxSize), Flex.shrink 0 ]


fractionCell val =
    cell [ Flex.grow val ]


fillCell =
    fractionCell 1


autoCell =
    cell [ Flex.basisAuto ]


fill : Cell msg
fill =
    fillCell [] B.none



-- mediumGap =
--     Flex.gap Constants.medium
-- largeGap =
--     Flex.gap Constants.large
-- horizontallyCentered content =
--     columnWithOptions
--         [ Flex.alignItems (Flex.alignWrapper Flex.center)
--         ]
--         []
--         [ autoCell [] content ]
-- type alias CellResponsive msg =
--     { repeatable : Flex.Repeatable
--     , attrs : List (A.FlexItemAttributes msg -> A.FlexItemAttributes msg)
--     , content : NodeWithStyle msg
--     }
-- responsiveRowWithOptions : List (Flex.FlexContainerCoordinate -> Flex.FlexContainerCoordinate) -> List (Flex.FlexContainerDetails -> Flex.FlexContainerDetails) -> List (CellResponsive msg) -> NodeWithStyle msg
-- responsiveRowWithOptions flexAttrs attrs flexItems =
--     B.flex
--         ([ displayBlock
--          , fillHeight
--             --  , flexContainerProperties
--             --     [ Flex.columns
--             --         ([ Flex.template
--             --             (flexItems
--             --                 |> List.map .repeatable
--             --             )
--             --          , Flex.align Flex.stretch
--             --          ]
--             flexAttrs
--          -- )
--          -- ]
--          ]
--             ++ attrs
--         )
--         (flexItems |> List.map (\flexItem -> B.flexItem flexItem.attrs [ flexItem.content ]))
-- responsiveCell valType attrs content =
--     CellResponsive valType attrs content
-- minMaxCell val1 val2 =
--     responsiveCell (Flex.minmax val1 val2)
