module View exposing (..)

import Dict
import Html
import Html.Attributes as Attr
import Html.Events as Evt
import Model
import Object


view : Model.Model -> Html.Html Model.Msg
view model =
    case model of
        Model.Failure msg ->
            Html.text msg

        Model.Success runningModel ->
            Html.div []
                [ viewContent runningModel ]


viewContent : Model.RunningModel -> Html.Html Model.Msg
viewContent runningModel =
    let
        definitionNames =
            Dict.keys runningModel.swagger.definitions
    in
    Html.div []
        [ viewObjectSelector definitionNames
        , viewToggleAllButton
        , viewMaybeNote runningModel.note
        , viewMaybeObject runningModel.object
        , viewCopyButton runningModel.object
        , viewYaml runningModel.object
        ]


attrList : String -> Html.Attribute msg
attrList id =
    Attr.attribute "list" id


viewObjectSelector : List String -> Html.Html Model.Msg
viewObjectSelector names =
    let
        dataId =
            "typeSelector"
    in
    Html.div []
        [ Html.table [ Attr.class "typeSelectorTable" ]
            [ Html.tbody []
                [ Html.tr []
                    [ Html.td [ Attr.class "typeSelectorLabel" ]
                        [ Html.text "Resource type:"
                        ]
                    , Html.td []
                        [ Html.input [ Attr.class "typeSelector", attrList dataId, Evt.onInput Model.UpdateSelectedType ] []
                        , Html.datalist [ Attr.id dataId ]
                            (List.map
                                (\n -> Html.option [ Attr.value n ] [ Html.text n ])
                                names
                            )
                        ]
                    ]
                ]
            ]
        ]


viewToggleAllButton : Html.Html Model.Msg
viewToggleAllButton =
    let
        hideText =
            String.fromChar '⬁'

        expandText =
            String.fromChar '⬂'
    in
    Html.div [ Attr.class "toggleAllButton" ]
        [ Html.span [ Evt.onClick (Model.ToggleAll False) ]
            [ Html.text hideText ]
        , Html.span [ Evt.onClick (Model.ToggleAll True) ]
            [ Html.text expandText ]
        ]


viewMaybeNote : Maybe String -> Html.Html Model.Msg
viewMaybeNote maybeNote =
    case maybeNote of
        Nothing ->
            Html.span [ Attr.style "display" "none" ] []

        Just note ->
            Html.div [ Attr.class "note", Evt.onClick (Model.UpdateNote Nothing) ]
                [ Html.text note ]


viewMaybeObject : Maybe Object.Object -> Html.Html Model.Msg
viewMaybeObject maybeObject =
    case maybeObject of
        Nothing ->
            Html.span [ Attr.style "display" "none" ] []

        Just object ->
            Html.div [ Attr.class "form" ]
                [ viewObject object ]


viewObject : Object.Object -> Html.Html Model.Msg
viewObject object =
    case object of
        Object.Object fields ->
            Html.table []
                [ Html.tbody [] (List.map viewField fields)
                ]

        Object.Array objects ->
            let
                objectHtmls =
                    List.map viewObject objects

                objectRows =
                    List.indexedMap
                        (\i html ->
                            Html.tr []
                                [ Html.td [ Attr.class "fieldNameCell" ]
                                    [ Html.div [ Attr.class "fieldName" ]
                                        [ Html.div [ Attr.class "arrayIndex" ]
                                            [ Html.text ("[" ++ String.fromInt i ++ "]")
                                            ]
                                        ]
                                    ]
                                , Html.td [ Attr.class "fieldVlaueCell" ]
                                    [ Html.div [ Attr.class "fieldValue" ]
                                        [ html
                                        ]
                                    ]
                                ]
                        )
                        objectHtmls
            in
            Html.table []
                [ Html.tbody [] objectRows ]

        Object.Value key value ->
            Html.input [ Attr.id key, Attr.value value, Evt.onInput (Model.UpdateValue key) ] []

        Object.InfoOnly description ->
            Html.div [ Attr.class "infoOnly" ]
                [ Html.text "Unsupported free-form object:"
                , Html.br [] []
                , Html.text description
                ]


viewField : Object.Field -> Html.Html Model.Msg
viewField field =
    case field of
        Object.Field description name object ->
            let
                childElement =
                    if description.visible then
                        viewObject object

                    else
                        Html.text "..."

                expandText =
                    if description.visible then
                        String.fromChar '⬁'

                    else
                        String.fromChar '⬂'

                expandElement =
                    if Object.objectIsExpandable object then
                        Html.div [ Attr.class "expandButton", Evt.onClick (Model.Toggle description.key) ]
                            [ Html.text expandText ]

                    else
                        Html.span [ Attr.style "display" "none" ] []

                addRow =
                    if Object.objectIsListType object && description.visible then
                        Html.div []
                            [ Html.div [ Attr.class "addButton", Evt.onClick (Model.AddElement description.key) ]
                                [ Html.text "+" ]
                            , Html.div [ Attr.class "removeButton", Evt.onClick (Model.RemoveElement description.key) ]
                                [ Html.text "-" ]
                            ]

                    else
                        Html.span [ Attr.style "display" "none" ] []
            in
            Html.tr []
                [ Html.td [ Attr.id description.key, Attr.title description.description, Attr.class "fieldNameCell" ]
                    [ Html.div [ Attr.class "fieldName" ]
                        [ Html.span [ Evt.onClick (Model.UpdateNote (Just (name ++ ": " ++ description.description))) ]
                            [ Html.text name
                            ]
                        , expandElement
                        ]
                    ]
                , Html.td [ Attr.class "fieldValueCell" ]
                    [ Html.div [ Attr.class "fieldValue" ]
                        [ childElement
                        , addRow
                        ]
                    ]
                ]

        Object.AdditionalProperty description name object ->
            Html.tr []
                [ Html.td [ Attr.id description.key, Attr.title description.description, Attr.class "fieldNameCell" ]
                    [ Html.div [ Attr.class "fieldName" ]
                        [ Html.input [ Attr.id description.key, Attr.value name, Evt.onInput (Model.UpdateName description.key) ] []
                        ]
                    ]
                , Html.td [ Attr.class "fieldValueCell" ]
                    [ Html.div [ Attr.class "fieldValue" ]
                        [ viewObject object ]
                    ]
                ]


viewCopyButton : Maybe Object.Object -> Html.Html Model.Msg
viewCopyButton object =
    Html.div [ Attr.class "copyButton" ]
        [ Html.button [ Attr.value "copy", Evt.onClick (Model.CopyYaml object) ]
            [ Html.text "copy" ]
        ]


viewYaml : Maybe Object.Object -> Html.Html Model.Msg
viewYaml maybeObject =
    let
        yaml =
            maybeObject
                |> Maybe.map (\o -> "---\n" ++ Object.objectToYaml 0 o)
                |> Maybe.withDefault ""

        ( rows, cols ) =
            dimensions yaml
    in
    Html.div [ Attr.class "yaml" ]
        [ Html.textarea [ Attr.attribute "rows" (String.fromInt rows), Attr.attribute "cols" (String.fromInt cols), Attr.attribute "disabled" "disabled" ]
            [ Html.text (String.trim yaml ++ "\n")
            ]
        ]


dimensions : String -> ( Int, Int )
dimensions content =
    let
        lines =
            String.split "\n" content

        rows =
            1 + List.length lines

        cols =
            String.length (longest lines)
    in
    ( rows, cols )


longest : List String -> String
longest =
    List.foldr
        (\x y ->
            if String.length x > String.length y then
                x

            else
                y
        )
        ""
