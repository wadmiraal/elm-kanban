module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Markdown


main =
    Browser.sandbox { init = initialModel, view = view, update = update }



-- MODEL


type alias Model =
    { columns : List Column
    , newCardName : String
    , newCardDescription : String
    , newColumnName : String
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


initialModel : Model
initialModel =
    { columns = []
    , newCardName = ""
    , newCardDescription = ""
    , newColumnName = ""
    }


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
    | CancelMarkCardForUpdating
    | CancelMarkColumnForUpdating
    | MarkCardForUpdating Int
    | MarkColumnForUpdating Int
    | StoreCardName String
    | StoreCardDescription String
    | StoreColumnName String
    | UpdateCard
    | UpdateColumn


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- Add simple dummy column. Can be modified afterwards.
        AddColumn ->
            { model | columns = List.append model.columns [ newColumn (getNextId model.columns) ] }

        -- Add simple dummy card to the column identified by id. Can be modified afterwards.
        AddCard id ->
            let
                map =
                    \c ->
                        if c.id == id then
                            { c | cards = List.append c.cards [ newCard (getNextCardId model.columns) ] }

                        else
                            c
            in
            { model | columns = List.map map model.columns }

        -- Mark the column identified by id as being updated. Store its name as the default value.
        MarkColumnForUpdating id ->
            let
                map =
                    \c ->
                        if c.id == id then
                            { c | updating = True }

                        else
                            { c | updating = False }
            in
            { model
                | columns = List.map map model.columns
                , newColumnName =
                    case List.head (List.filter (\c -> c.id == id) model.columns) of
                        Nothing ->
                            -- Failsafe
                            ""

                        Just c ->
                            c.name
            }

        -- Cancel the updating of the column.
        CancelMarkColumnForUpdating ->
            let
                map =
                    \c -> { c | updating = False }
            in
            { model | columns = List.map map model.columns }

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
            { model | columns = List.map map model.columns }

        -- Store the typed column name.
        StoreColumnName name ->
            { model | newColumnName = name }

        -- Mark the card identified by id as being updated. Store its name as the default value.
        MarkCardForUpdating id ->
            let
                internalMap =
                    \c ->
                        if c.id == id then
                            { c | updating = True }

                        else
                            { c | updating = False }

                map =
                    \column -> { column | cards = List.map internalMap column.cards }

                card =
                    List.head (List.filter (\c -> c.id == id) (List.concatMap (\column -> column.cards) model.columns))
            in
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
            }

        -- Cancel the updating of the column.
        CancelMarkCardForUpdating ->
            let
                internalMap =
                    \card -> { card | updating = False }

                map =
                    \column -> { column | cards = List.map internalMap column.cards }
            in
            { model | columns = List.map map model.columns }

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
            { model | columns = List.map map model.columns }

        -- Store the typed card name.
        StoreCardName name ->
            { model | newCardName = name }

        -- Store the typed card description.
        StoreCardDescription description ->
            { model | newCardDescription = description }


newColumn : Int -> Column
newColumn newId =
    { id = newId, name = "New column", cards = [], updating = False }


newCard : Int -> Card
newCard newId =
    { id = newId, name = "New card", description = "", updating = False }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "columns" ]
            [ Keyed.node "div" [] (List.map (viewKeyedColumn model) model.columns)
            , div [ class "column column--add-new" ]
                [ button [ onClick AddColumn ] [ text "Add new column" ]
                ]
            ]
        ]


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
        , button [ onClick (AddCard column.id) ] [ text "Add new card" ]
        ]


viewColumnHeader : Model -> Column -> Html Msg
viewColumnHeader model column =
    div [ class "column__header" ]
        [ if column.updating then
            div [ class "column__name column__name--updating" ]
                [ input [ type_ "text", value model.newColumnName, onInput StoreColumnName ] []
                , button [ onClick UpdateColumn ] [ text "Update" ]
                , span [ onClick CancelMarkColumnForUpdating ] [ text "Cancel" ]
                ]

          else
            div [ class "column__name", onClick (MarkColumnForUpdating column.id) ]
                [ h2 [] [ text column.name ]
                ]
        ]


viewKeyedCard : Model -> Card -> ( String, Html Msg )
viewKeyedCard model card =
    ( String.fromInt card.id
    , lazy2 viewCard model card
    )


viewCard : Model -> Card -> Html Msg
viewCard model card =
    div [ class "card" ]
        [ if card.updating then
            div []
                [ div [ class "card__name card__name--updating" ]
                    [ input [ type_ "text", value model.newCardName, onInput StoreCardName ] []
                    ]
                , div [ class "card__description card__description--updating" ]
                    [ textarea [ onInput StoreCardDescription ] [ text model.newCardDescription ]
                    ]
                , button [ onClick UpdateCard ] [ text "Update" ]
                , span [ onClick CancelMarkCardForUpdating ] [ text "Cancel" ]
                ]

          else
            div [ onClick (MarkCardForUpdating card.id) ]
                [ div [ class "card__name" ]
                    [ h3 [] [ text card.name ]
                    ]
                , div [ class "card__description" ] (Markdown.toHtml Nothing card.description)
                ]
        ]
