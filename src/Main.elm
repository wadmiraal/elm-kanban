module Main exposing (main)

import Browser exposing (element)
import Browser.Events
import Debug
import Html exposing (..)
import Html.Attributes exposing (attribute, class, type_, value)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Json.Decode as D
import List.Extra
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
    , dragging : Bool
    , dragOverDropId : Maybe ( Int, Int )
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
    , dragging : Bool
    , name : String
    , updating : Bool
    }


type alias Position =
    { x : Int
    , y : Int
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { columns = []
      , dragging = False
      , dragOverDropId = Nothing
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
    | CancelDragging
    | CancelUpdating
    | DoNothing
    | DragOverTarget ( Int, Int )
    | DropCard ( Int, Int )
    | LeaveDragTarget
    | MarkCardForDragging Int
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

        -- Mark the card identified by id as being dragged.
        MarkCardForDragging cardId ->
            let
                internalMap =
                    \( id, c ) ->
                        if id == cardId then
                            ( id, { c | dragging = True } )

                        else
                            ( id, { c | dragging = False } )

                map =
                    \( columnId, column ) -> ( columnId, { column | cards = List.map internalMap column.cards } )
            in
            updateSilent { model | columns = List.map map model.columns, dragging = True }

        -- Drag the card over a droppable zone.
        DragOverTarget dropId ->
            updateSilent { model | dragOverDropId = Just dropId }

        LeaveDragTarget ->
            updateSilent { model | dragOverDropId = Nothing }

        -- Drop the card over a droppable zone.
        DropCard ( columnId, cardId ) ->
            let
                card =
                    List.concatMap (\( _, column ) -> column.cards) model.columns
                        |> List.filter (\( _, c ) -> c.dragging)
                        |> List.head

                columnsWithoutCard =
                    let
                        map =
                            \( id, column ) ->
                                ( id
                                , { column | cards = List.filter (\( _, c ) -> c.dragging == False) column.cards }
                                )
                    in
                    List.map map model.columns

                newColumns =
                    let
                        map =
                            \( id, column ) ->
                                case card of
                                    -- Failsafe; should never happen.
                                    Nothing ->
                                        ( id, column )

                                    Just c ->
                                        if id == columnId then
                                            if cardId == -1 then
                                                -- Add to the beginning of the list.
                                                ( id
                                                , { column | cards = c :: column.cards }
                                                )

                                            else
                                                -- Add after the card with cardId. Partition the list, and concat them back together with the card between them.
                                                let
                                                    ( ids, _ ) =
                                                        List.unzip column.cards

                                                    split =
                                                        List.Extra.splitWhen (\id2 -> id2 == cardId) ids

                                                    cards =
                                                        case split of
                                                            -- Failsafe; should never happen.
                                                            Nothing ->
                                                                column.cards

                                                            Just ( head, _ ) ->
                                                                let
                                                                    length =
                                                                        List.length head + 1
                                                                in
                                                                List.take length column.cards ++ [ c ] ++ List.drop length column.cards
                                                in
                                                ( id
                                                , { column
                                                    | cards = cards
                                                  }
                                                )

                                        else
                                            ( id, column )
                    in
                    List.map map columnsWithoutCard
            in
            updateSilent { model | columns = newColumns, dragOverDropId = Nothing }

        -- Cancel the dragging of a card or column.
        CancelDragging ->
            let
                internalMap =
                    \( cardId, card ) -> ( cardId, { card | dragging = False } )

                map =
                    \( columnId, column ) -> ( columnId, { column | cards = List.map internalMap column.cards } )
            in
            updateSilent { model | columns = List.map map model.columns, dragging = False }

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
    { name = "New card", description = "", dragging = False, updating = False }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.updating then
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
        Browser.Events.onKeyDown (D.map isEsc pressedKey)

    else
        Sub.none



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

        dropTarget =
            if model.dragging then
                ( "1st-drop-target", viewCardDropTarget ( id, -1 ) model )

            else
                ( "1st-drop-target", text "" )
    in
    div [ class "column" ]
        [ viewColumnHeader model item
        , Keyed.node "div" [ class "column__cards" ] (dropTarget :: List.map (viewKeyedCard model id) column.cards)
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
viewKeyedCard : Model -> Int -> ( Int, Card ) -> ( String, Html Msg )
viewKeyedCard model columnId item =
    let
        ( id, _ ) =
            item
    in
    ( String.fromInt id
    , lazy3 viewCard model columnId item
    )


viewCard : Model -> Int -> ( Int, Card ) -> Html Msg
viewCard model columnId ( id, card ) =
    let
        dropId =
            ( columnId, id )

        dropTarget =
            if model.dragging && not card.dragging then
                -- A card cannot be dragged to its own position... Too complicated.
                viewCardDropTarget dropId model

            else
                text ""

        cardNode =
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

            else if card.dragging then
                div [ class "card card--dragging", attribute "draggable" "true", onDragEnd CancelDragging ]
                    [ div [ class "card__name" ]
                        [ h3 [] [ text card.name ]
                        ]
                    , div [ class "card__description" ] (Markdown.toHtml Nothing card.description)
                    ]

            else
                div [ class "card", attribute "draggable" "true", onDragStart (MarkCardForDragging id) ]
                    [ div [ class "card__name", onClick (MarkCardForUpdating id) ]
                        [ h3 [] [ text card.name ]
                        ]
                    , div [ class "card__description", onClick (MarkCardForUpdating id) ] (Markdown.toHtml Nothing card.description)
                    ]
    in
    div []
        [ cardNode
        , dropTarget
        ]


viewCardDropTarget : ( Int, Int ) -> Model -> Html Msg
viewCardDropTarget dropId model =
    let
        baseClass =
            "card-drop-target"

        classList =
            case model.dragOverDropId of
                Nothing ->
                    baseClass

                Just currDropId ->
                    if currDropId == dropId then
                        baseClass ++ " card-drop-target--hovering"

                    else
                        baseClass
    in
    div
        [ class classList
        , attribute "droppable" "true"
        , onDragEnter (DragOverTarget dropId)
        , onDragLeave LeaveDragTarget
        , onDragOver DoNothing
        , onDrop (DropCard dropId)
        ]
        []


onDragEnter : Msg -> Attribute Msg
onDragEnter msg =
    custom "dragenter" <|
        D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }


onDragLeave : Msg -> Attribute Msg
onDragLeave msg =
    custom "dragleave" <|
        D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }


onDragOver : Msg -> Attribute Msg
onDragOver msg =
    custom "dragover" <|
        D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }


onDragStart : Msg -> Attribute Msg
onDragStart msg =
    on "dragstart" (D.succeed msg)


onDragEnd : Msg -> Attribute Msg
onDragEnd msg =
    on "dragend" (D.succeed msg)


onDrop : Msg -> Attribute Msg
onDrop msg =
    custom "drop" <|
        D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
