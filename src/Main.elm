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
    { columns : List ( Int, Column )
    , newCardName : String
    , newCardDescription : String
    , newColumnName : String
    , updating : Bool
    }


type alias Column =
    { cards : List ( Int, Card )
    , name : String
    , updating : Bool
    }


type alias Card =
    { description : String
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


getNextId : List ( Int, a ) -> Int
getNextId list =
    let
        lastId =
            List.foldl Basics.max 0 (List.map (\( id, _ ) -> id) list)
    in
    lastId + 1


getNextCardId : List ( Int, Column ) -> Int
getNextCardId columns =
    let
        allCards =
            List.concatMap (\( _, column ) -> column.cards) columns
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
            updateSilent { model | columns = model.columns ++ [ ( getNextId model.columns, newColumn ) ] }

        AddCard columnId ->
            let
                map =
                    \( id, c ) ->
                        if id == columnId then
                            ( id, { c | cards = c.cards ++ [ ( getNextCardId model.columns, newCard ) ] } )

                        else
                            ( id, c )
            in
            updateSilent { model | columns = List.map map model.columns }

        -- Mark the column identified by id as being updated. Store its name as the default value.
        MarkColumnForUpdating columnId ->
            let
                map =
                    \( id, c ) ->
                        if id == columnId then
                            ( id, { c | updating = True } )

                        else
                            ( id, { c | updating = False } )

                columnName =
                    case List.head (List.filter (\( id, c ) -> id == columnId) model.columns) of
                        Nothing ->
                            -- Failsafe
                            ""

                        Just ( _, c ) ->
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
                    \( id, c ) ->
                        if c.updating then
                            ( id, { c | name = model.newColumnName, updating = False } )

                        else
                            ( id, c )
            in
            updateSilent { model | columns = List.map map model.columns, updating = False }

        -- Store the new column name, as it's being typed.
        StoreColumnName name ->
            updateSilent { model | newColumnName = name }

        -- Mark the card identified by id as being updated. Store its name and description as the default values.
        MarkCardForUpdating cardId ->
            let
                internalMap =
                    \( id, c ) ->
                        if id == cardId then
                            ( id, { c | updating = True } )

                        else
                            ( id, { c | updating = False } )

                map =
                    \( columnId, column ) -> ( columnId, { column | cards = List.map internalMap column.cards } )

                card =
                    List.head (List.filter (\( id, _ ) -> id == cardId) (List.concatMap (\( _, column ) -> column.cards) model.columns))
            in
            updateSilent
                { model
                    | columns = List.map map model.columns
                    , newCardName =
                        case card of
                            Nothing ->
                                ""

                            Just ( _, c ) ->
                                c.name
                    , newCardDescription =
                        case card of
                            Nothing ->
                                ""

                            Just ( _, c ) ->
                                c.description
                    , updating = True
                }

        -- Cancel the updating of a card or column.
        CancelUpdating ->
            let
                internalMap =
                    \( cardId, card ) -> ( cardId, { card | updating = False } )

                map =
                    \( columnId, column ) -> ( columnId, { column | cards = List.map internalMap column.cards, updating = False } )
            in
            updateSilent { model | columns = List.map map model.columns, updating = False }

        -- Update the flagged card's name and description with the ones stored.
        UpdateCard ->
            let
                internalMap =
                    \( cardId, card ) ->
                        if card.updating then
                            ( cardId, { card | name = model.newCardName, description = model.newCardDescription, updating = False } )

                        else
                            ( cardId, card )

                map =
                    \( columnId, column ) -> ( columnId, { column | cards = List.map internalMap column.cards } )
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
newColumn : Column
newColumn =
    { name = "New column", cards = [], updating = False }


{-| Helper function for creating a new card.
-}
newCard : Card
newCard =
    { name = "New card", description = "", updating = False }



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
            (List.map (viewKeyedColumn model) model.columns
                ++ [ ( "add", div [ class "column column--add-new" ] [ button [ onClick AddColumn ] [ text "+ Add new column" ] ] ) ]
            )
        ]


{-| Wrapper function around `viewColumn`. Adds support for both Html.Keyed and
Html.Lazy for performance optimization.
-}
viewKeyedColumn : Model -> ( Int, Column ) -> ( String, Html Msg )
viewKeyedColumn model item =
    let
        ( id, _ ) =
            item
    in
    ( String.fromInt id
    , lazy2 viewColumn model item
    )


viewColumn : Model -> ( Int, Column ) -> Html Msg
viewColumn model item =
    let
        ( id, column ) =
            item
    in
    div [ class "column" ]
        [ viewColumnHeader model item
        , Keyed.node "div" [ class "column__cards" ] (List.map (viewKeyedCard model) column.cards)
        , button [ class "column__add-card", onClick (AddCard id) ] [ text "Add new card" ]
        ]


viewColumnHeader : Model -> ( Int, Column ) -> Html Msg
viewColumnHeader model item =
    let
        ( id, column ) =
            item
    in
    div [ class "column__header" ]
        [ if column.updating then
            div [ class "column__name column__name--updating" ]
                [ form [ onSubmit UpdateColumn ]
                    [ input [ type_ "text", value model.newColumnName, onInput StoreColumnName ] []
                    , button [ type_ "submit" ] [ text "Update" ]
                    , span [ class "cancel-link", onClick CancelUpdating ] [ text "Cancel" ]
                    ]
                ]

          else
            div [ class "column__name", onClick (MarkColumnForUpdating id) ]
                [ h2 [] [ text column.name ]
                ]
        ]


{-| Wrapper function around `viewCard`. Adds support for both Html.Keyed and
Html.Lazy for performance optimization.
-}
viewKeyedCard : Model -> ( Int, Card ) -> ( String, Html Msg )
viewKeyedCard model item =
    let
        ( id, _ ) =
            item
    in
    ( String.fromInt id
    , lazy2 viewCard model item
    )


viewCard : Model -> ( Int, Card ) -> Html Msg
viewCard model ( id, card ) =
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
                , span [ class "cancel-link", onClick CancelUpdating ] [ text "Cancel" ]
                ]
            ]

    else
        div [ class "card", onClick (MarkCardForUpdating id) ]
            [ div [ class "card__name" ]
                [ h3 [] [ text card.name ]
                ]
            , div [ class "card__description" ] (Markdown.toHtml Nothing card.description)
            ]
