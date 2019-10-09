port module Main exposing (main)

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


{-| Firefox has an issue. We need to manipulate the drag event via a port for it to work.
-}
port dragstart : D.Value -> Cmd msg


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
    , dragging : Bool
    , dragOverDropId : Maybe ( Int, Int )
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
    , dragging : Bool
    , id : Int
    , name : String
    , updating : Bool
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
    | CancelDragging
    | CancelUpdating
    | DoNothing
    | DragOverTarget ( Int, Int )
    | DropCard ( Int, Int )
    | LeaveDragTarget
    | MarkCardForDragging ( Int, Int ) D.Value
    | MarkCardForUpdating ( Int, Int )
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

        updateDragStart =
            \m e ->
                ( m, dragstart e )
    in
    case msg of
        AddColumn ->
            updateSilent { model | columns = model.columns ++ [ newColumn (getNextId model.columns) ] }

        AddCard columnId ->
            let
                map =
                    \column ->
                        if column.id == columnId then
                            { column | cards = column.cards ++ [ newCard (getNextCardId model.columns) ] }

                        else
                            column
            in
            updateSilent { model | columns = List.map map model.columns }

        -- Mark the column identified by id as being updated. Store its name as the default value.
        MarkColumnForUpdating columnId ->
            let
                map =
                    \column ->
                        if column.id == columnId then
                            { column | updating = True }

                        else
                            { column | updating = False }

                columnName =
                    case List.head (List.filter (\column -> column.id == columnId) model.columns) of
                        Nothing ->
                            -- Failsafe
                            ""

                        Just column ->
                            column.name
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
                    \column ->
                        if column.updating then
                            { column | name = model.newColumnName, updating = False }

                        else
                            column
            in
            updateSilent { model | columns = List.map map model.columns, updating = False }

        -- Store the new column name, as it's being typed.
        StoreColumnName name ->
            updateSilent { model | newColumnName = name }

        -- Mark the card identified by id as being updated. Store its name and description as the default values.
        MarkCardForUpdating ( columnId, cardId ) ->
            let
                internalMap =
                    \c ->
                        if c.id == cardId then
                            { c | updating = True }

                        else
                            { c | updating = False }

                map =
                    \column ->
                        if column.id == columnId then
                            { column | cards = List.map internalMap column.cards }

                        else
                            column

                card =
                    List.concatMap (\column -> column.cards) model.columns
                        |> List.filter (\c -> c.id == cardId)
                        |> List.head
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
                    \column ->
                        { column | cards = List.map internalMap column.cards, updating = False }
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
                    \column ->
                        { column | cards = List.map internalMap column.cards }
            in
            updateSilent { model | columns = List.map map model.columns, updating = False }

        -- Store the new card name as it's being typed.
        StoreCardName name ->
            updateSilent { model | newCardName = name }

        -- Store the new card description as it's being typed.
        StoreCardDescription description ->
            updateSilent { model | newCardDescription = description }

        -- Mark the card identified by id as being dragged.
        MarkCardForDragging ( columnId, cardId ) event ->
            let
                internalMap =
                    \card ->
                        if card.id == cardId then
                            { card | dragging = True }

                        else
                            { card | dragging = False }

                map =
                    \column ->
                        if column.id == columnId then
                            { column | cards = List.map internalMap column.cards }

                        else
                            column
            in
            updateDragStart { model | columns = List.map map model.columns, dragging = True } event

        -- Drag the card over a droppable zone.
        DragOverTarget dropId ->
            updateSilent { model | dragOverDropId = Just dropId }

        LeaveDragTarget ->
            updateSilent { model | dragOverDropId = Nothing }

        -- Drop the card over a droppable zone.
        DropCard ( columnId, targetCardId ) ->
            let
                droppedCard =
                    List.concatMap (\column -> column.cards) model.columns
                        |> List.filter (\c -> c.dragging)
                        |> List.head

                columnsWithoutCard =
                    let
                        map =
                            \column ->
                                { column | cards = List.filter (\c -> c.dragging == False) column.cards }
                    in
                    List.map map model.columns

                newColumns =
                    let
                        map =
                            \column ->
                                if column.id == columnId then
                                    case droppedCard of
                                        -- Failsafe; should never happen. If it does, simply leave the column untouched.
                                        Nothing ->
                                            column

                                        Just card ->
                                            if targetCardId == -1 then
                                                -- Add to the beginning of the list.
                                                { column | cards = card :: column.cards }

                                            else
                                                -- Add after the card with targetCardId. Partition the list, and concat them back together with the card between them.
                                                let
                                                    ( head, _ ) =
                                                        Maybe.withDefault ( [], [] ) (List.Extra.splitWhen (\c -> c.id == targetCardId) column.cards)

                                                    cards =
                                                        let
                                                            length =
                                                                List.length head + 1
                                                        in
                                                        List.take length column.cards ++ [ card ] ++ List.drop length column.cards
                                                in
                                                { column | cards = cards }

                                else
                                    column
                    in
                    List.map map columnsWithoutCard
            in
            updateSilent { model | columns = newColumns, dragOverDropId = Nothing }

        -- Cancel the dragging of a card or column.
        CancelDragging ->
            let
                internalMap =
                    \card -> { card | dragging = False }

                map =
                    \column -> { column | cards = List.map internalMap column.cards }
            in
            updateSilent { model | columns = List.map map model.columns, dragging = False }

        -- Don't do anything.
        DoNothing ->
            updateSilent model


{-| Helper function for creating a new column.
-}
newColumn : Int -> Column
newColumn id =
    { id = id, name = "New column", cards = [], updating = False }


{-| Helper function for creating a new card.
-}
newCard : Int -> Card
newCard id =
    { id = id, name = "New card", description = "", dragging = False, updating = False }



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
viewKeyedColumn : Model -> Column -> ( String, Html Msg )
viewKeyedColumn model column =
    ( String.fromInt column.id
    , lazy2 viewColumn model column
    )


viewColumn : Model -> Column -> Html Msg
viewColumn model column =
    let
        dropTarget =
            if model.dragging then
                ( "1st-drop-target", viewCardDropTarget ( column.id, -1 ) model )

            else
                ( "1st-drop-target", text "" )
    in
    div [ class "column" ]
        [ viewColumnHeader model column
        , Keyed.node "div" [ class "column__cards" ] (dropTarget :: List.map (viewKeyedCard model column.id) column.cards)
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
                    , span [ class "cancel-link", onClick CancelUpdating ] [ text "Cancel" ]
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
viewKeyedCard : Model -> Int -> Card -> ( String, Html Msg )
viewKeyedCard model columnId card =
    ( String.fromInt card.id
    , lazy3 viewCard model columnId card
    )


viewCard : Model -> Int -> Card -> Html Msg
viewCard model columnId card =
    let
        id =
            ( columnId, card.id )

        dropTarget =
            if model.dragging && not card.dragging then
                -- A card cannot be dragged to its own position... Too complicated.
                viewCardDropTarget id model

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


onDragStart : (D.Value -> Msg) -> Attribute Msg
onDragStart msg =
    on "dragstart" (D.map msg D.value)


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
