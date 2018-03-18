module SkyWriter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, placeholder)
import Html.Events exposing (onClick, onInput)
import Http exposing (getString)
import Svg exposing (svg, circle)
import Svg.Attributes exposing (cx, cy, r, fill, stroke, width, height, viewBox)
import Result exposing (withDefault)
import Xml
import Xml.Decode
import Xml.Encode
import Xml.Query exposing (collect, tag, tags, int, float)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { name : String
    , xml : Xml.Value
    , stars : List Star
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" (Xml.Encode.null) [], Cmd.none )



-- UPDATE


type Msg
    = Refresh
    | NewXml (Result Http.Error String)


type alias Star =
    { mag : Float, ra : Float, de : Float }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( model, getXmlData )

        NewXml (Ok xmlstring) ->
            ( let
                xml =
                    withDefault Xml.Encode.null <|
                        Xml.Decode.decode xmlstring
              in
                { model
                    | xml = xml
                    , stars = xmlToStars xml
                }
            , Cmd.none
            )

        NewXml (Err err) ->
            ( model, Cmd.none )


getXmlData : Cmd Msg
getXmlData =
    let
        forCORS =
            "https://cors-anywhere.herokuapp.com/"

        url =
            "http://server1.sky-map.org/getstars.jsp?ra=12&de=45&angle=40&max_stars=1000&max_vmag=8"

        request =
            getString (forCORS ++ url)
    in
        Http.send NewXml request


xmlToStars : Xml.Value -> List Star
xmlToStars xml =
    let
        xmls =
            tags "star" xml
    in
        List.map3 (\m r d -> Star m r d)
            (collect (tag "mag" num) xmls)
            (collect (tag "ra" num) xmls)
            (collect (tag "de" num) xmls)


num : Xml.Value -> Result String Float
num val =
    case int val of
        Ok i ->
            Ok (toFloat i)

        Err _ ->
            float val


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "backgroundColor", "#000" )
            , ( "height", "100%" )
            ]
        ]
        [ div []
            [ input [ placeholder "This does nothing" ] []
            , button [ onClick Refresh ] [ text "Go" ]
            ]
        , svg
            [ width "740", height "740", viewBox "0 0 740 740" ]
            --            (List.map drawStar model.stars)
            (drawMeridians (0 / 4))
        ]


drawMeridians : Float -> List (Svg.Svg msg)
drawMeridians lambda =
    let
        mer theta =
            circle
                [ cx "370"
                , cy <| renorm (cos lambda / (sin lambda - cos theta)) 1
                , r <| toString <| sqrt <| 10000 * (1 - cos theta ^ 2) / (cos lambda - cos theta) ^ 2
                , stroke "white"
                , fill "none"
                ]
                []
    in
        List.map mer
            (List.map (\n -> pi / 2 * (0 + (toFloat n) / 12)) <| List.range 1 6)


renorm : Float -> Float -> String
renorm x xn =
    toString (740 * (1 / 2 + x / xn))


drawStar : Star -> Svg.Svg msg
drawStar { mag, ra, de } =
    circle
        [ cx (renorm (ra - 12) 5)
        , cy (renorm (de - 45) 40)
        , r (toString (8 - mag))
        , fill "white"
        ]
        []
