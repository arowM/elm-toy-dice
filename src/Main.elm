module Main exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes exposing (attribute, class)
import Html.Events as Events
import Keyboard
import Random


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { state = StateRoling
      , max = 10
      , stopAt = Nothing
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { state : State
    , max : Int
    , stopAt : Maybe Int
    }


type State
    = StateRoling
    | StateStopped



-- UPDATE


type Msg
    = ClickRoll
    | ClickStopRoll
    | SetRandom Int
    | KeyDown Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickRoll ->
            ( { model
                | state = StateRoling
                , stopAt = Nothing
              }
            , Cmd.none
            )

        ClickStopRoll ->
            ( model
            , Random.generate SetRandom <| Random.int 0 model.max
            )

        SetRandom n ->
            ( { model
                | stopAt = Just n
                , state = StateStopped
              }
            , Cmd.none
            )

        KeyDown _ ->
            ( model
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    pack_
        [ class "vh100"
        , class "column"
        ]
        [ Html.header
            [ class "wrap"
            , class "row"
            , class "justifyCenter"
            , class "header"
            ]
            [ wrappedText "Elm TRPG Dice"
            ]
        , wrap_
            [ class "scrollY"
            , class "expanded"
            ]
            [ wrap
                [ pack_
                    [ class "drumWindow"
                    ]
                    [ pack_
                        (List.concat
                            [ [ class "drum_top"
                              ]
                            , case model.stopAt of
                                Just n ->
                                    [ Attributes.attribute "data-margin" <| toString n
                                    ]

                                Nothing ->
                                    []
                            ]
                        )
                        []
                    , pack_
                        (List.concat
                            [ [ class "drum"
                              ]
                            , case model.state of
                                StateStopped ->
                                    [ class "drum-stopped"
                                    ]

                                _ ->
                                    []
                            ]
                        )
                      <|
                        List.map renderDrumRow <| List.range 0 model.max ++ List.range 0 model.max
                    ]
                ]
            , wrap <|
                case model.state of
                    StateRoling ->
                        [ Html.button
                            [ Attributes.type_ "button"
                            , Events.onClick ClickStopRoll
                            , class "wrap"
                            , class "button"
                            ]
                            [ Html.text "とめる"
                            ]
                        ]

                    StateStopped ->
                        [ Html.button
                            [ Attributes.type_ "button"
                            , Events.onClick ClickRoll
                            , class "wrap"
                            , class "button"
                            ]
                            [ Html.text "まわす"
                            ]
                        ]
            ]
        ]


renderDrumRow : Int -> Html msg
renderDrumRow n =
    pack_
        [ class "drum_row"
        , class "row"
        , class "alignCenter"
        , class "justifyCenter"
        ]
        [ Html.span
            []
            [ Html.text <| toString n
            ]
        ]


-- Helper View functions


pack : List (Html msg) -> Html msg
pack =
    pack_ []


pack_ : List (Attribute msg) -> List (Html msg) -> Html msg
pack_ attrs children =
    Html.div
        (class "pack" :: attrs)
        children


wrap : List (Html msg) -> Html msg
wrap =
    wrap_ []


wrap_ : List (Attribute msg) -> List (Html msg) -> Html msg
wrap_ attrs children =
    Html.div
        (class "wrap" :: attrs)
        children


wrappedText : String -> Html msg
wrappedText str =
    wrap
        [ Html.text str
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown



-- Helper functions


ariaSelected : Bool -> Attribute Msg
ariaSelected b =
    attribute "aria-selected" <|
        if b then
            "true"
        else
            "false"


ariaHidden : Bool -> Attribute Msg
ariaHidden b =
    attribute "aria-hidden" <|
        if b then
            "true"
        else
            "false"


role : String -> Attribute Msg
role =
    attribute "role"


result : (l -> x) -> (r -> x) -> Result l r -> x
result onErr onOk res =
    case res of
        Err l ->
            onErr l

        Ok r ->
            onOk r


maybe : a -> (b -> a) -> Maybe b -> a
maybe def f =
    Maybe.withDefault def << Maybe.map f
