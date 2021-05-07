module Update exposing (..)

import Copy
import Dict
import Json.Schema.Definitions as Defs
import Model
import Object


update : Model.Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
update msg model =
    let
        _ =
            Debug.log "message" msg
    in
    case msg of
        Model.NoOp ->
            ( model, Cmd.none )

        Model.CopyYaml maybeObject ->
            let
                yaml =
                    maybeObject
                        |> Maybe.map (\o -> "---\n" ++ Object.objectToYaml 0 o)
                        |> Maybe.withDefault ""
            in
            ( model, Copy.copy yaml )

        Model.UpdateSelectedType updatedSelectedType ->
            case model of
                Model.Success runningModel ->
                    let
                        object =
                            objectFromType updatedSelectedType runningModel.swagger.definitions
                    in
                    ( Model.Success { runningModel | object = object }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Model.Toggle key ->
            case model of
                Model.Success runningModel ->
                    let
                        object =
                            runningModel.object |> Maybe.map (\o -> toggleObject key o)
                    in
                    ( Model.Success { runningModel | object = object }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Model.UpdateValue key value ->
            case model of
                Model.Success runningModel ->
                    let
                        object =
                            runningModel.object |> Maybe.map (\o -> updateObjectValue key value o)
                    in
                    ( Model.Success { runningModel | object = object }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Model.UpdateName key name ->
            case model of
                Model.Success runningModel ->
                    let
                        object =
                            runningModel.object |> Maybe.map (\o -> updateObjectName key name o)
                    in
                    ( Model.Success { runningModel | object = object }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Model.UpdateNote note ->
            case model of
                Model.Success runningModel ->
                    ( Model.Success { runningModel | note = note }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Model.AddElement key ->
            case model of
                Model.Success runningModel ->
                    let
                        object =
                            runningModel.object |> Maybe.map (\o -> addElementToObject key o)
                    in
                    ( Model.Success { runningModel | object = object }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Model.RemoveElement key ->
            case model of
                Model.Success runningModel ->
                    let
                        object =
                            runningModel.object |> Maybe.map (\o -> removeElementFromObject key o)
                    in
                    ( Model.Success { runningModel | object = object }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Model.ToggleAll show ->
            case model of
                Model.Success runningModel ->
                    case runningModel.object of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            let
                                object =
                                    runningModel.object |> Maybe.map (\o -> showAllObject show o)
                            in
                            ( Model.Success { runningModel | object = object }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


showAllObject : Bool -> Object.Object -> Object.Object
showAllObject show object =
    case object of
        Object.Value _ _ ->
            object

        Object.Array objects ->
            Object.Array (List.map (showAllObject show) objects)

        Object.Object fields ->
            Object.Object (List.map (showAllField show) fields)

        Object.InfoOnly _ ->
            object


showAllField : Bool -> Object.Field -> Object.Field
showAllField show field =
    case field of
        Object.Field description name object ->
            if Object.objectIsExpandable object then
                Object.Field { description | visible = show } name (showAllObject show object)

            else
                field

        Object.AdditionalProperty description name object ->
            if Object.objectIsExpandable object then
                Object.AdditionalProperty { description | visible = show } name (showAllObject show object)

            else
                field


objectFromType : String -> Dict.Dict String Defs.Schema -> Maybe Object.Object
objectFromType typeString definitions =
    if recursiveType typeString then
        let
            definition =
                Dict.get typeString definitions

            description =
                descriptionFromSchema definition
        in
        Just (Object.InfoOnly description)

    else
        let
            definition =
                Dict.get typeString definitions
        in
        definition |> Maybe.andThen (\d -> fromSchema typeString d definitions)


descriptionFromSchema : Maybe Defs.Schema -> String
descriptionFromSchema maybeSchema =
    case maybeSchema of
        Nothing ->
            ""

        Just schema ->
            case schema of
                Defs.BooleanSchema b ->
                    if b then
                        "True"

                    else
                        "False"

                Defs.ObjectSchema subschema ->
                    subschema.description |> Maybe.withDefault ""


recursiveType : String -> Bool
recursiveType typeName =
    case typeName of
        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceDefinition" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceDefinitionList" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceDefinitionSpec" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceDefinitionVersion" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceValidation" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.JSONSchemaProps" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1beta1.CustomResourceDefinition" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1beta1.CustomResourceDefinitionList" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1beta1.CustomResourceDefinitionSpec" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1beta1.CustomResourceDefinitionVersion" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1beta1.CustomResourceValidation" ->
            True

        "io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1beta1.JSONSchemaProps" ->
            True

        _ ->
            False


fromSchema : String -> Defs.Schema -> Dict.Dict String Defs.Schema -> Maybe Object.Object
fromSchema typeString schema definitions =
    let
        baseObject =
            Object.fromSchema "" schema definitions

        gvk =
            parseTypeString typeString

        objectWithApiVersion =
            updateObjectValue ".apiVersion" (String.join "/" [ gvk.group, gvk.version ]) baseObject

        object =
            updateObjectValue ".kind" gvk.kind objectWithApiVersion
    in
    Just object


type alias GVK =
    { group : String, version : String, kind : String }


parseTypeString : String -> GVK
parseTypeString typeString =
    let
        parts =
            String.split "." typeString

        partsR =
            List.reverse parts

        kind =
            Maybe.withDefault "" (List.head partsR)

        version =
            Maybe.withDefault "" (List.head (List.drop 1 partsR))

        group =
            Maybe.withDefault "" (List.head (List.drop 2 partsR))

        baseGVK =
            { group = group, version = version, kind = kind }
    in
    case group of
        "apps" ->
            baseGVK

        "batch" ->
            baseGVK

        "core" ->
            baseGVK

        "extensions" ->
            baseGVK

        "policy" ->
            baseGVK

        "flowcontrol" ->
            { baseGVK | group = group ++ ".apiserver.k8s.io" }

        "internal" ->
            { baseGVK | group = group ++ ".apiserver.k8s.io" }

        "rbac" ->
            { baseGVK | group = group ++ ".authorization.k8s.io" }

        _ ->
            { baseGVK | group = group ++ ".k8s.io" }


updateObjectValue : String -> String -> Object.Object -> Object.Object
updateObjectValue updateKey newValue object =
    case object of
        Object.Value key _ ->
            if updateKey == key then
                Object.Value key newValue

            else
                object

        Object.Array objects ->
            Object.Array (List.map (updateObjectValue updateKey newValue) objects)

        Object.Object fields ->
            Object.Object (List.map (updateFieldValue updateKey newValue) fields)

        Object.InfoOnly _ ->
            object


updateFieldValue : String -> String -> Object.Field -> Object.Field
updateFieldValue updateKey newValue field =
    case field of
        Object.Field description name object ->
            if String.startsWith description.key updateKey then
                Object.Field description name (updateObjectValue updateKey newValue object)

            else
                field

        Object.AdditionalProperty description name object ->
            if String.startsWith description.key updateKey then
                Object.AdditionalProperty description name (updateObjectValue updateKey newValue object)

            else
                field


updateObjectName : String -> String -> Object.Object -> Object.Object
updateObjectName updateKey newName object =
    case object of
        Object.Value _ _ ->
            object

        Object.Array objects ->
            Object.Array (List.map (updateObjectName updateKey newName) objects)

        Object.Object fields ->
            Object.Object (List.map (updateFieldName updateKey newName) fields)

        Object.InfoOnly _ ->
            object


updateFieldName : String -> String -> Object.Field -> Object.Field
updateFieldName updateKey newName field =
    case field of
        Object.Field description name object ->
            if String.startsWith description.key updateKey then
                Object.Field description name (updateObjectName updateKey newName object)

            else
                field

        Object.AdditionalProperty description name object ->
            if updateKey == description.key then
                Object.AdditionalProperty description newName object

            else if String.startsWith description.key updateKey then
                Object.AdditionalProperty description name (updateObjectName updateKey newName object)

            else
                field


toggleObject : String -> Object.Object -> Object.Object
toggleObject updateKey object =
    case object of
        Object.Value _ _ ->
            object

        Object.Array objects ->
            Object.Array (List.map (toggleObject updateKey) objects)

        Object.Object fields ->
            Object.Object (List.map (toggleField updateKey) fields)

        Object.InfoOnly _ ->
            object


toggleField : String -> Object.Field -> Object.Field
toggleField updateKey field =
    case field of
        Object.Field description name object ->
            if updateKey == description.key then
                Object.Field { description | visible = flip description.visible } name object

            else if String.startsWith description.key updateKey then
                Object.Field description name (toggleObject updateKey object)

            else
                field

        Object.AdditionalProperty description name object ->
            if updateKey == description.key then
                Object.AdditionalProperty { description | visible = flip description.visible } name object

            else if String.startsWith description.key updateKey then
                Object.AdditionalProperty description name (toggleObject updateKey object)

            else
                field


addElementToObject : String -> Object.Object -> Object.Object
addElementToObject updateKey object =
    case object of
        Object.Value _ _ ->
            object

        Object.Array objects ->
            Object.Array (List.map (addElementToObject updateKey) objects)

        Object.Object fields ->
            Object.Object (List.map (addElementToField updateKey) fields)

        Object.InfoOnly _ ->
            object


addElementToField : String -> Object.Field -> Object.Field
addElementToField updateKey field =
    case field of
        Object.Field description name object ->
            if updateKey == description.key then
                let
                    newPrefix =
                        description.key ++ "." ++ (Object.objectLength object |> String.fromInt)

                    newObject =
                        Object.objectAddElement newPrefix object
                in
                Object.Field description name newObject

            else if String.startsWith description.key updateKey then
                Object.Field description name (addElementToObject updateKey object)

            else
                field

        Object.AdditionalProperty description name object ->
            if updateKey == description.key then
                let
                    newPrefix =
                        description.key ++ "." ++ (Object.objectLength object |> String.fromInt)

                    newObject =
                        Object.objectAddElement newPrefix object
                in
                Object.AdditionalProperty description name newObject

            else if String.startsWith description.key updateKey then
                Object.AdditionalProperty description name (addElementToObject updateKey object)

            else
                field


removeElementFromObject : String -> Object.Object -> Object.Object
removeElementFromObject updateKey object =
    case object of
        Object.Value _ _ ->
            object

        Object.Array objects ->
            Object.Array (List.map (removeElementFromObject updateKey) objects)

        Object.Object fields ->
            Object.Object (List.map (removeElementFromField updateKey) fields)

        Object.InfoOnly _ ->
            object


removeElementFromField : String -> Object.Field -> Object.Field
removeElementFromField updateKey field =
    case field of
        Object.Field description name object ->
            if updateKey == description.key then
                let
                    newObject =
                        Object.objectRemoveElement object
                in
                Object.Field description name newObject

            else if String.startsWith description.key updateKey then
                Object.Field description name (removeElementFromObject updateKey object)

            else
                field

        Object.AdditionalProperty description name object ->
            if updateKey == description.key then
                let
                    newObject =
                        Object.objectRemoveElement object
                in
                Object.AdditionalProperty description name newObject

            else if String.startsWith description.key updateKey then
                Object.AdditionalProperty description name (removeElementFromObject updateKey object)

            else
                field


flip : Bool -> Bool
flip b =
    if b then
        False

    else
        True
