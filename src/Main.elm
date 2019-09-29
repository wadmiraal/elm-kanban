module Main exposing (main)

import Browser exposing (element)
import Browser.Events
import Debug
import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Json.Decode as D
import Markdown


main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MODEL


type alias Model =
    { columns : List Column
    , newCardName : String
    , newCardDescription : String
    , newColumnName : String
    , updating : Bool
    }


type alias Column =
    { cards : List Card
    , id : Int
    , name : String
    , updating : Bool
    }


type alias Card =
    { description : String
    , id : Int
    , name : String
    , updating : Bool
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { columns = []
      , newCardName = ""
      , newCardDescription = ""
      , newColumnName = ""
      , updating = False
      }
    , Cmd.none
    )


getNextId : List { a | id : Int } -> Int
getNextId list =
    let
        lastId =
            List.foldl Basics.max 0 (List.map (\c -> c.id) list)
    in
    lastId + 1


getNextCardId : List Column -> Int
getNextCardId columns =
    let
        allCards =
            List.concatMap (\column -> column.cards) columns
    in
    getNextId allCards



-- UPDATE


type Msg
    = AddColumn
    | AddCard Int
    | CancelUpdating
    | DoNothing
    | MarkCardForUpdating Int
    | MarkColumnForUpdating Int
    | StoreCardName String
    | StoreCardDescription String
    | StoreColumnName String
    | UpdateCard
    | UpdateColumn


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        updateSilent =
            \m ->
                ( m, Cmd.none )
    in
    case msg of
        AddColumn ->
            updateSilent { model | columns = List.append model.columns [ newColumn (getNextId model.columns) ] }

        AddCard columnId ->
            let
                map =
                    \c ->
                        if c.id == columnId then
                            { c | cards = List.append c.cards [ newCard (getNextCardId model.columns) ] }

                        else
                            c
            in
            updateSilent { model | columns = List.map map model.columns }

        -- Mark the column identified by id as being updated. Store its name as the default value.
        MarkColumnForUpdating columnId ->
            let
                map =
                    \c ->
                        if c.id == columnId then
                            { c | updating = True }

                        else
                            { c | updating = False }

                columnName =
                    case List.head (List.filter (\c -> c.id == columnId) model.columns) of
                        Nothing ->
                            -- Failsafe
                            ""

                        Just c ->
                            c.name
            in
            updateSilent
                { model
                    | columns = List.map map model.columns
                    , newColumnName = columnName
                    , updating = True
                }

        -- Update the flagged column's name with the one stored.
        UpdateColumn ->
            let
                map =
                    \c ->
                        if c.updating then
                            { c | name = model.newColumnName, updating = False }

                        else
                            c
            in
            updateSilent { model | columns = List.map map model.columns, updating = False }

        -- Store the new column name, as it's being typed.
        StoreColumnName name ->
            updateSilent { model | newColumnName = name }

        -- Mark the card identified by id as being updated. Store its name and description as the default values.
        MarkCardForUpdating cardId ->
            let
                internalMap =
                    \c ->
                        if c.id == cardId then
                            { c | updating = True }

                        else
                            { c | updating = False }

                map =
                    \column -> { column | cards = List.map internalMap column.cards }

                card =
                    List.head (List.filter (\c -> c.id == cardId) (List.concatMap (\column -> column.cards) model.columns))
            in
            updateSilent
                { model
                    | columns = List.map map model.columns
                    , newCardName =
                        case card of
                            Nothing ->
                                ""

                            Just c ->
                                c.name
                    , newCardDescription =
                        case card of
                            Nothing ->
                                ""

                            Just c ->
                                c.description
                    , updating = True
                }

        -- Cancel the updating of a card or column.
        CancelUpdating ->
            let
                internalMap =
                    \card -> { card | updating = False }

                map =
                    \column -> { column | cards = List.map internalMap column.cards, updating = False }
            in
            updateSilent { model | columns = List.map map model.columns, updating = False }

        -- Update the flagged card's name and description with the ones stored.
        UpdateCard ->
            let
                internalMap =
                    \card ->
                        if card.updating then
                            { card | name = model.newCardName, description = model.newCardDescription, updating = False }

                        else
                            card

                map =
                    \column -> { column | cards = List.map internalMap column.cards }
            in
            updateSilent { model | columns = List.map map model.columns, updating = False }

        -- Store the new card name as it's being typed.
        StoreCardName name ->
            updateSilent { model | newCardName = name }

        -- Store the new card description as it's being typed.
        StoreCardDescription description ->
            updateSilent { model | newCardDescription = description }

        -- Don't do anything. Used in subscriptions.
        DoNothing ->
            updateSilent model


{-| Helper function for creating a new column.
-}
newColumn : Int -> Column
newColumn newId =
    { id = newId, name = "New column", cards = [], updating = False }


{-| Helper function for creating a new card.
-}
newCard : Int -> Card
newCard newId =
    { id = newId, name = "New card", description = "", updating = False }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.updating then
        Browser.Events.onKeyDown cancelOnEsc

    else
        Sub.none


{-| Even handler. Trigger a "cancel update" message if the Esc key is pressed.
-}
cancelOnEsc : D.Decoder Msg
cancelOnEsc =
    let
        isEsc =
            \keyValue ->
                case keyValue of
                    "Escape" ->
                        CancelUpdating

                    _ ->
                        DoNothing

        pressedKey =
            D.field "key" D.string
    in
    D.map isEsc pressedKey



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Keyed.node "div"
            [ class "columns" ]
          <|
            List.map (viewKeyedColumn model) model.columns
                ++ [ ( "add", div [ class "column column--add-new" ] [ button [ onClick AddColumn ] [ text "+ Add new column" ] ] ) ]
        ]


{-| Wrapper function around `viewColumn`. Adds support for both Html.Keyed and
Html.Lazy for performance optimization.
-}
viewKeyedColumn : Model -> Column -> ( String, Html Msg )
viewKeyedColumn model column =
    ( String.fromInt column.id
    , lazy2 viewColumn model column
    )


viewColumn : Model -> Column -> Html Msg
viewColumn model column =
    div [ class "column" ]
        [ viewColumnHeader model column
        , Keyed.node "div" [ class "column__cards" ] (List.map (viewKeyedCard model) column.cards)
        , button [ class "column__add-card", onClick (AddCard column.id) ] [ text "Add new card" ]
        ]


viewColumnHeader : Model -> Column -> Html Msg
viewColumnHeader model column =
    div [ class "column__header" ]
        [ if column.updating then
            div [ class "column__name column__name--updating" ]
                [ form [ onSubmit UpdateColumn ]
                    [ input [ type_ "text", value model.newColumnName, onInput StoreColumnName ] []
                    , button [ type_ "submit" ] [ text "Update" ]
                    , span [ onClick CancelUpdating ] [ text "Cancel" ]
                    ]
                ]

          else
            div [ class "column__name", onClick (MarkColumnForUpdating column.id) ]
                [ h2 [] [ text column.name ]
                ]
        ]


{-| Wrapper function around `viewCard`. Adds support for both Html.Keyed and
Html.Lazy for performance optimization.
-}
viewKeyedCard : Model -> Card -> ( String, Html Msg )
viewKeyedCard model card =
    ( String.fromInt card.id
    , lazy2 viewCard model card
    )


viewCard : Model -> Card -> Html Msg
viewCard model card =
    if card.updating then
        div [ class "card card--updating" ]
            [ form [ onSubmit UpdateCard ]
                [ div [ class "card__name" ]
                    [ input [ type_ "text", value model.newCardName, onInput StoreCardName ] []
                    ]
                , div [ class "card__description" ]
                    [ textarea [ onInput StoreCardDescription ] [ text model.newCardDescription ]
                    ]
                , button [ type_ "submit" ] [ text "Update" ]
                , span [ onClick CancelUpdating ] [ text "Cancel" ]
                ]
            ]

    else
        div [ class "card", onClick (MarkCardForUpdating card.id) ]
            [ div [ class "card__name" ]
                [ h3 [] [ text card.name ]
                ]
            , div [ class "card__description" ] (Markdown.toHtml Nothing card.description)
            ]
