module Utils exposing (findBy, gray, intToMonth, textToHtml)

import BodyBuilder exposing (NodeWithStyle, br, text)
import Color
import Time


findBy : (a -> b) -> b -> List a -> Maybe a
findBy insideDataFun data =
    List.filter (\e -> insideDataFun e == data)
        >> List.head


gray : Color.Color
gray =
    Color.grayscale 0.9


textToHtml : String -> List (NodeWithStyle msg)
textToHtml =
    String.split "\n"
        >> List.foldr (\e accu -> [ text e, br ] ++ accu) []


intToMonth : Int -> Maybe Time.Month
intToMonth nb =
    case nb of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing
