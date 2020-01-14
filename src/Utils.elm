module Utils exposing
    ( findBy
    , frontendUrl
    , gray
    , intToMonth
    , normalizeIntForDate
    , onOrOff
    , sameDate
    , textToHtml
    )

import BodyBuilder exposing (NodeWithStyle, br, text)
import Color
import Time
import Url


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


normalizeIntForDate : String -> String
normalizeIntForDate str =
    if String.length str == 1 then
        "0" ++ str

    else
        str


onOrOff : Bool -> String
onOrOff bool =
    if bool then
        "on"

    else
        "off"


yearAndMonthFromDate : String -> { year : Int, month : Int }
yearAndMonthFromDate str =
    case String.split "-" str of
        a :: b :: _ ->
            { year = Maybe.withDefault 2013 (String.toInt a)
            , month = Maybe.withDefault 1 (String.toInt b)
            }

        _ ->
            { year = 2013
            , month = 1
            }


sameDate : String -> String -> Bool
sameDate a b =
    (yearAndMonthFromDate a).year == (yearAndMonthFromDate b).year


stringFromProtocol : Url.Protocol -> String
stringFromProtocol protocol =
    case protocol of
        Url.Https ->
            "https"

        Url.Http ->
            "http"


stringFromPort : Maybe Int -> String
stringFromPort maybePort =
    case maybePort of
        Just port_ ->
            ":" ++ String.fromInt port_

        Nothing ->
            ""


frontendUrl : Url.Url -> String
frontendUrl url =
    stringFromProtocol url.protocol ++ "://" ++ url.host ++ stringFromPort url.port_ ++ "/"
