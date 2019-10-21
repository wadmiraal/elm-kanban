----------------------------------------------------------
--
-- Main.elm
-- A simple Kanban app written in Elm.
-- Copyright (c) 2019 Wouter Admiraal <wad@wadmiraal.net>
-- Distributed under the MIT License, see LICENSE.txt
--
----------------------------------------------------------


port module Main exposing (main)

import Browser exposing (element)
import Browser.Dom
import Browser.Events
import Debug
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, required, title, type_, value)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as Pipeline
import Json.Encode as E
import List.Extra
import Markdown
import Task



-- PORTS
-- We require ports to communicate with JavaScript, in order to use APIs that
-- Elm doesn't support yet.
--
-- We have 2 ports:
--
-- 1.  A port to write data to the browser's localStorage.
-- 2.  A port to interact with a ondragstart event in Firefox. This is needed to
--     make the HTML5 drag and drop API work in Firefox.


{-| Send data to JS for storage.
-}
port store : String -> Cmd msg


{-| Firefox has an issue with the new HTML5 drag and drop API. We need to
manipulate the drag event and set some data on it. However, we cannot do this in
Elm. We need a port for this.
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
-- We have a "root" model record, which stores all the data of our application.
-- We also define `Column` and `Card` types, which represent the Kanban columns
-- and cards.


{-| Our "root" model has several properties, which are needed to propagate data
throughout certain events or user interactions.

  - `dragCardOverDropId`: we store the ID of drop targets as they are hovered,
    which allows us to update the CSS classes of those targets accordingly.
  - `dragColumnOverDropId`: we store the ID of drop targets as they are hovered,
    which allows us to update the CSS classes of those targets accordingly.
  - `draggingCard`: store whether any card is currently being dragged.
  - `draggingColumn`: store whether any column is currently being dragged.
  - `newCardName`, `newCardDescription` and `newColumnName`: store the values
    as the user types them, so we can set them when the form is subnitted.
  - `updating`: store whether any card or column is currently being updated.
    We use this to listen to the ESC key being pressed, which then cancels the
    update. But if this value is False, we don't bother listening to any
    keyboard events.

-}
type alias Model =
    { columns : List Column
    , draggingCard : Bool
    , draggingColumn : Bool
    , dragCardOverDropId : Maybe ( Int, Int )
    , dragColumnOverDropId : Maybe Int
    , newCardDescription : String
    , newCardName : String
    , newColumnName : String
    , updating : Bool
    }


{-| Our Column records store all data related to their state, as well as 2 extra
flags:

  - `updating`: allows us to know the column is currently being updated, and
    enables us to render it differently (with a form).
  - `dragging`: allows us to know if the column is being dragged, and enables us
    to bind different event listeners to enable/prevent certain user interactions.

-}
type alias Column =
    { cards : List Card
    , dragging : Bool
    , id : Int
    , name : String
    , updating : Bool
    }


{-| Our Card records store all data related to their state, as well as 2 extra
flags:

  - `updating`: allows us to know the card is currently being updated, and enables
    us to render it differently (with a form).
  - `dragging`: allows us to know if the card is being dragged, and enables us to
    bind different event listeners to enable/prevent certain user interactions.

-}
type alias Card =
    { description : String
    , dragging : Bool
    , id : Int
    , name : String
    , updating : Bool
    }


init : Maybe String -> ( Model, Cmd msg )
init flags =
    let
        defaultModel =
            { columns = []
            , draggingCard = False
            , draggingColumn = False
            , dragCardOverDropId = Nothing
            , dragColumnOverDropId = Nothing
            , newCardName = ""
            , newCardDescription = ""
            , newColumnName = ""
            , updating = False
            }

        model =
            case flags of
                Nothing ->
                    defaultModel

                Just data ->
                    D.decodeString modelDecoder data
                        |> Result.withDefault defaultModel
    in
    ( model
    , Cmd.none
    )


{-| Helper function to get the next available ID, by looping over all existing
elements, and retrieving the highest existing ID, and adding 1.
-}
getNextId : List { a | id : Int } -> Int
getNextId list =
    let
        lastId =
            List.foldl Basics.max 0 (List.map (\c -> c.id) list)
    in
    lastId + 1


{-| Helper function for getting the next available card ID. Before using
`getNextId`, we flatten all card lists.
-}
getNextCardId : List Column -> Int
getNextCardId columns =
    let
        allCards =
            List.concatMap (\column -> column.cards) columns
    in
    getNextId allCards


{-| Encoder function. Encode the current model to a representation we can store.
We don't just store the whole model, as a snapshot. We only store what is
relevant.
-}
modelEncoder : Model -> E.Value
modelEncoder model =
    let
        cardEncoder =
            \card ->
                E.object
                    [ ( "id", E.int card.id )
                    , ( "name", E.string card.name )
                    , ( "description", E.string card.description )
                    ]

        columnEncoder =
            \column ->
                E.object
                    [ ( "id", E.int column.id )
                    , ( "name", E.string column.name )
                    , ( "cards", E.list identity (List.map cardEncoder column.cards) )
                    ]
    in
    E.object
        [ ( "columns", E.list identity (List.map columnEncoder model.columns) )
        , ( "schemaVersion", E.string "1.0" )
        ]


{-| Decoder function. Decodes the given JSON data to extract the information,
and construct a model which will serve as the initial app state.
-}
modelDecoder : D.Decoder Model
modelDecoder =
    let
        cardDecoder =
            D.succeed Card
                |> Pipeline.required "description" D.string
                -- dragging
                |> Pipeline.hardcoded False
                |> Pipeline.required "id" D.int
                |> Pipeline.required "name" D.string
                -- updating
                |> Pipeline.hardcoded False

        columnDecoder =
            D.succeed Column
                |> Pipeline.required "cards" (D.list cardDecoder)
                -- dragging
                |> Pipeline.hardcoded False
                |> Pipeline.required "id" D.int
                |> Pipeline.required "name" D.string
                -- updating
                |> Pipeline.hardcoded False
    in
    D.succeed Model
        |> Pipeline.required "columns" (D.list columnDecoder)
        -- draggingCard
        |> Pipeline.hardcoded False
        -- draggingColumn
        |> Pipeline.hardcoded False
        -- dragCardOverDropId
        |> Pipeline.hardcoded Nothing
        -- dragColumnOverDropId
        |> Pipeline.hardcoded Nothing
        -- newCardDescription
        |> Pipeline.hardcoded ""
        -- newCardName
        |> Pipeline.hardcoded ""
        -- newColumnName
        |> Pipeline.hardcoded ""
        -- updating
        |> Pipeline.hardcoded False



-- UPDATE


type Msg
    = AddColumn
    | AddCard Int
    | CancelDragging
    | CancelUpdating
    | DoNothing
    | DragCardOutOfTarget
    | DragCardOverTarget ( Int, Int )
    | DragColumnOutOfTarget
    | DragColumnOverTarget Int
    | DropCard ( Int, Int )
    | DropColumn Int
    | MarkCardForDragging ( Int, Int ) D.Value
    | MarkCardForUpdating ( Int, Int ) String
    | MarkColumnForDragging Int D.Value
    | MarkColumnForUpdating Int String
    | StoreCardName String
    | StoreCardDescription String
    | StoreColumnName String
    | UpdateCard
    | UpdateColumn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        -- Helper function to "silently" update the model. Simply dispatches
        --a Cmd.none command.
        updateSilent =
            \m ->
                ( m, Cmd.none )

        -- Helper function to persist the model in a store.
        updatePersist =
            \m ->
                ( m
                , modelEncoder m
                    |> E.encode 0
                    |> store
                )

        -- Helper function for using ondragstart events in Firefox. Use the
        -- `dragstart` port to manipulate the event.
        updateDragStart =
            \m e ->
                ( m, dragstart e )

        -- Helper function for focusing an input field. This needs to
        -- "broadcast" another message, for which we simply use `DoNothing`.
        updateAndFocus =
            \m domId ->
                ( m, Task.attempt (always DoNothing) (Browser.Dom.focus domId) )
    in
    case msg of
        ------------------------------------------------------------------------
        -- Adding new data:
        ------------------------------------------------------------------------
        AddColumn ->
            updatePersist { model | columns = model.columns ++ [ newColumn (getNextId model.columns) "New column" ] }

        AddCard columnId ->
            let
                map =
                    \column ->
                        if column.id == columnId then
                            { column | cards = column.cards ++ [ newCard (getNextCardId model.columns) "New card" ] }

                        else
                            column
            in
            updatePersist { model | columns = List.map map model.columns }

        ------------------------------------------------------------------------
        -- Updating column name:
        ------------------------------------------------------------------------
        MarkColumnForUpdating columnId domId ->
            let
                map =
                    \c ->
                        if c.id == columnId then
                            { c | updating = True }

                        else
                            { c | updating = False }

                column =
                    List.filter (\c -> c.id == columnId) model.columns
                        |> List.head
                        -- Provide a default, although it should never happen.
                        |> Maybe.withDefault (newColumn 0 "")
            in
            updateAndFocus
                { model
                    | columns = List.map map model.columns
                    , newColumnName = column.name
                    , updating = True
                }
                domId

        UpdateColumn ->
            let
                map =
                    \column ->
                        if column.updating then
                            { column | name = model.newColumnName, updating = False }

                        else
                            column
            in
            updatePersist { model | columns = List.map map model.columns, updating = False }

        StoreColumnName name ->
            updateSilent { model | newColumnName = name }

        ------------------------------------------------------------------------
        -- Updating card name and description:
        ------------------------------------------------------------------------
        MarkCardForUpdating ( columnId, cardId ) domId ->
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
                        -- Provide a default, although it should never happen.
                        |> Maybe.withDefault (newCard 0 "")
            in
            updateAndFocus
                { model
                    | columns = List.map map model.columns
                    , newCardName = card.name
                    , newCardDescription = card.description
                    , updating = True
                }
                domId

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
            updatePersist { model | columns = List.map map model.columns, updating = False }

        StoreCardName name ->
            updateSilent { model | newCardName = name }

        StoreCardDescription description ->
            updateSilent { model | newCardDescription = description }

        ------------------------------------------------------------------------
        -- Dragging and dropping columns:
        ------------------------------------------------------------------------
        MarkColumnForDragging columnId event ->
            let
                map =
                    \column ->
                        if column.id == columnId then
                            { column | dragging = True }

                        else
                            { column | dragging = False }
            in
            updateDragStart { model | columns = List.map map model.columns, draggingColumn = True } event

        DragColumnOverTarget dropId ->
            updateSilent { model | dragColumnOverDropId = Just dropId }

        DragColumnOutOfTarget ->
            updateSilent { model | dragColumnOverDropId = Nothing }

        DropColumn targetColumnId ->
            let
                -- Start by finding the dropped column.
                draggedColumn =
                    List.filter (\c -> c.dragging) model.columns
                        |> List.head
            in
            case draggedColumn of
                -- Failsafe; should never happen. If it does, simply leave
                -- the columns untouched.
                Nothing ->
                    updateSilent { model | dragColumnOverDropId = Nothing }

                Just column ->
                    if column.id == targetColumnId then
                        -- The column was dropped over itself. Simply leave
                        -- the columns untouched.
                        updateSilent { model | dragColumnOverDropId = Nothing }

                    else
                        let
                            -- We need to process the columns. Start by removing
                            -- the column from the list.
                            columnsWithoutDraggedColumn =
                                List.filter (\c -> c.dragging == False) model.columns
                        in
                        if targetColumnId == -1 then
                            -- Add to the beginning of the list.
                            updatePersist
                                { model
                                    | dragColumnOverDropId = Nothing
                                    , columns = column :: columnsWithoutDraggedColumn
                                }

                        else
                            -- Add after the column with targetColumnId.
                            -- Partition the list, and concat them
                            -- back together with the column between them.
                            let
                                ( head, _ ) =
                                    List.Extra.splitWhen (\c -> c.id == targetColumnId) columnsWithoutDraggedColumn
                                        -- Provide a default, although it should never happen.
                                        |> Maybe.withDefault ( [], [] )

                                length =
                                    List.length head + 1

                                newColumns =
                                    List.take length columnsWithoutDraggedColumn ++ [ column ] ++ List.drop length columnsWithoutDraggedColumn
                            in
                            updatePersist
                                { model
                                    | dragColumnOverDropId = Nothing
                                    , columns = newColumns
                                }

        ------------------------------------------------------------------------
        -- Dragging and dropping cards:
        ------------------------------------------------------------------------
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
            updateDragStart { model | columns = List.map map model.columns, draggingCard = True } event

        DragCardOverTarget dropId ->
            updateSilent { model | dragCardOverDropId = Just dropId }

        DragCardOutOfTarget ->
            updateSilent { model | dragCardOverDropId = Nothing }

        DropCard ( targetColumnId, targetCardId ) ->
            let
                -- Start by finding the dropped card.
                draggedCard =
                    List.concatMap (\column -> column.cards) model.columns
                        |> List.filter (\c -> c.dragging)
                        |> List.head
            in
            case draggedCard of
                -- Failsafe; should never happen. If it does, simply leave
                -- the columns untouched.
                Nothing ->
                    updateSilent { model | dragCardOverDropId = Nothing }

                Just card ->
                    if card.id == targetCardId then
                        -- The card was dropped over itself. Simply leave
                        -- the columns untouched.
                        updateSilent { model | dragCardOverDropId = Nothing }

                    else
                        let
                            -- We need to process the columns. Start by removing
                            -- the card from its original column.
                            columnsWithoutCard =
                                let
                                    map =
                                        \column ->
                                            { column | cards = List.filter (\c -> c.dragging == False) column.cards }
                                in
                                List.map map model.columns

                            -- Use these new "clean" columns to insert the card
                            -- in its designated target location.
                            newColumns =
                                let
                                    map =
                                        \column ->
                                            if column.id == targetColumnId then
                                                if targetCardId == -1 then
                                                    -- Add to the beginning of the list.
                                                    { column | cards = card :: column.cards }

                                                else
                                                    -- Add after the card with targetCardId.
                                                    -- Partition the list, and concat them
                                                    -- back together with the card between them.
                                                    let
                                                        ( head, _ ) =
                                                            List.Extra.splitWhen (\c -> c.id == targetCardId) column.cards
                                                                -- Provide a default, although it should never happen.
                                                                |> Maybe.withDefault ( [], [] )

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
                        updatePersist { model | columns = newColumns, dragCardOverDropId = Nothing }

        ------------------------------------------------------------------------
        -- Misc:
        ------------------------------------------------------------------------
        CancelUpdating ->
            let
                internalMap =
                    \card -> { card | updating = False }

                map =
                    \column ->
                        { column | cards = List.map internalMap column.cards, updating = False }
            in
            updateSilent { model | columns = List.map map model.columns, updating = False }

        CancelDragging ->
            let
                internalMap =
                    \card -> { card | dragging = False }

                map =
                    \column -> { column | cards = List.map internalMap column.cards, dragging = False }
            in
            updateSilent
                { model
                    | columns = List.map map model.columns
                    , draggingCard = False
                    , draggingColumn = False
                }

        DoNothing ->
            updateSilent model


{-| Helper function for creating a new column.
-}
newColumn : Int -> String -> Column
newColumn id name =
    { id = id, name = name, cards = [], dragging = False, updating = False }


{-| Helper function for creating a new card.
-}
newCard : Int -> String -> Card
newCard id name =
    { id = id, name = name, description = "", dragging = False, updating = False }



-- SUBSCRIPTIONS
-- We have only 1 subscription: we listen to keyboard strokes whenever we're
-- updating a card or a column. If we detect the ESC key is pressed, we cancel
-- the updating. If we're not updating anything, we don't listen to keyboard
-- events either.


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
    let
        columnNodes =
            List.map (viewKeyedColumn model) model.columns

        columnNodesWithTargets =
            if model.draggingColumn then
                let
                    dropTargets =
                        ( "drop-target-0", viewColumnDropTarget -1 model )
                            :: List.map
                                (\c ->
                                    ( "drop-target-" ++ String.fromInt c.id
                                    , viewColumnDropTarget c.id model
                                    )
                                )
                                model.columns
                in
                List.Extra.interweave dropTargets columnNodes

            else
                columnNodes

        children =
            columnNodesWithTargets
                ++ [ ( "add"
                     , div [ class "column column--add-new" ]
                        [ button [ onClick AddColumn ] [ text "+ Add new column" ]
                        ]
                     )
                   ]
    in
    div []
        [ Keyed.node "div" [ class "columns" ] children
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
        cardNodes =
            List.map (viewKeyedCard model column.id) column.cards

        cardNodesWithTargets =
            if model.draggingCard then
                let
                    dropTargets =
                        ( "drop-target-0", viewCardDropTarget ( column.id, -1 ) model )
                            :: List.map
                                (\c ->
                                    ( "drop-target-" ++ String.fromInt c.id
                                    , viewCardDropTarget ( column.id, c.id ) model
                                    )
                                )
                                column.cards
                in
                List.Extra.interweave dropTargets cardNodes

            else
                cardNodes

        columnHeader =
            div [ class "column__header" ]
                [ if column.updating then
                    div [ class "column__name column__name--updating" ]
                        [ form [ onSubmit UpdateColumn ]
                            [ input [ Html.Attributes.id "column-name", type_ "text", required True, value model.newColumnName, onInput StoreColumnName ] []
                            , button [ type_ "submit" ] [ text "Update" ]
                            , span [ class "cancel-link", onClick CancelUpdating ] [ text "Cancel" ]
                            ]
                        ]

                  else if column.dragging then
                    div [ class "column__name" ]
                        [ h2 [] [ text column.name ]
                        ]

                  else
                    div [ class "column__name", onClick (MarkColumnForUpdating column.id "column-name"), title "Click to update column name" ]
                        [ h2 [] [ text column.name ]
                        ]
                ]

        columnWrappingDiv =
            if column.dragging then
                div [ class "column column--dragging", attribute "draggable" "true", onDragEnd CancelDragging ]

            else if column.updating then
                div [ class "column" ]

            else
                div [ class "column", attribute "draggable" "true", onDragStart (MarkColumnForDragging column.id) ]
    in
    columnWrappingDiv
        [ columnHeader
        , Keyed.node "div" [ class "column__cards" ] cardNodesWithTargets
        , button [ class "column__add-card", onClick (AddCard column.id) ] [ text "Add new card" ]
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
    in
    if card.updating then
        div [ class "card card--updating" ]
            [ form [ onSubmit UpdateCard ]
                [ div [ class "card__name" ]
                    [ input [ Html.Attributes.id "card-name", type_ "text", required True, value model.newCardName, onInput StoreCardName ] []
                    ]
                , div [ class "card__description" ]
                    [ textarea [ Html.Attributes.id "card-description", onInput StoreCardDescription ] [ text model.newCardDescription ]
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
            [ div [ class "card__name", onClick (MarkCardForUpdating id "card-name"), title "Click to update card name and description" ]
                [ h3 [] [ text card.name ]
                ]
            , div [ class "card__description", onClick (MarkCardForUpdating id "card-description"), title "Click to update card name and description" ] (Markdown.toHtml Nothing card.description)
            ]


viewColumnDropTarget : Int -> Model -> Html Msg
viewColumnDropTarget dropId model =
    let
        hovering =
            case model.dragColumnOverDropId of
                Nothing ->
                    False

                Just currDropId ->
                    dropId == currDropId
    in
    div
        [ classList
            [ ( "drop-target drop-target--column", True )
            , ( "drop-target--hovering", hovering )
            ]
        , onDragLeave DragColumnOutOfTarget
        , attribute "droppable" "true"
        , onDragEnter (DragColumnOverTarget dropId)
        , onDragOver DoNothing
        , onDrop (DropColumn dropId)
        ]
        []


viewCardDropTarget : ( Int, Int ) -> Model -> Html Msg
viewCardDropTarget dropId model =
    let
        hovering =
            case model.dragCardOverDropId of
                Nothing ->
                    False

                Just currDropId ->
                    dropId == currDropId
    in
    div
        [ classList
            [ ( "drop-target drop-target--card", True )
            , ( "drop-target--hovering", hovering )
            ]
        , onDragLeave DragCardOutOfTarget
        , attribute "droppable" "true"
        , onDragEnter (DragCardOverTarget dropId)
        , onDragOver DoNothing
        , onDrop (DropCard dropId)
        ]
        []



-- EVENTS
-- We have some custom event handlers in order to handle HTML5 drag and drop
-- events.


{-| We must make sure these events don't propagate, otherwise they prevent the
"ondrop" even from triggering correctly.
See <https://stackoverflow.com/questions/21339924/drop-event-not-firing-in-chrome>
-}
onDragEnter : Msg -> Attribute Msg
onDragEnter msg =
    custom "dragenter" <|
        D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }


{-| We must make sure these events don't propagate, otherwise they prevent the
"ondrop" even from triggering correctly.
See <https://stackoverflow.com/questions/21339924/drop-event-not-firing-in-chrome>
-}
onDragLeave : Msg -> Attribute Msg
onDragLeave msg =
    custom "dragleave" <|
        D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }


{-| Even though we don't really use "ondragover" events, we still need to make
sure they don't propagate. This is necessary, otherwise they prevent the
"ondrop" event from triggering correctly.
See <https://stackoverflow.com/questions/21339924/drop-event-not-firing-in-chrome>
-}
onDragOver : Msg -> Attribute Msg
onDragOver msg =
    custom "dragover" <|
        D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }


{-| The ondragstart event is special, in that it needs a Msg constructor. The
reason for this is we need to pass it the "raw" JS event as a parameter, which
can then be used in `update` to be passed to the `dragstart` port. This is
necessary to fix an issue in Firefox.
Because this event bubbles up, and cards are children of columns, we need to
prevent this event from propagating.
-}
onDragStart : (D.Value -> Msg) -> Attribute Msg
onDragStart msg =
    stopPropagationOn "dragstart" (D.map (\m -> ( m, True )) (D.map msg D.value))


onDragEnd : Msg -> Attribute Msg
onDragEnd msg =
    on "dragend" (D.succeed msg)


{-| We must make sure these events don't propagate, otherwise they might trigger
unwanted browser behaviors.
See <https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/Drag_operations#drop>
-}
onDrop : Msg -> Attribute Msg
onDrop msg =
    custom "drop" <|
        D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
