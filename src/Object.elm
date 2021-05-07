module Object exposing (..)

import Dict
import Html exposing (object)
import Json.Schema.Definitions as Defs


type alias Description =
    { key : String
    , description : String
    , type_ : String
    , visible : Bool
    }


type Field
    = Field Description String Object
    | AdditionalProperty Description String Object


type Object
    = Object (List Field)
    | Array (List Object)
    | Value String String
    | InfoOnly String


fieldName : Field -> String
fieldName field =
    case field of
        Field _ name _ ->
            name

        AdditionalProperty _ name _ ->
            name


fieldKey : Field -> String
fieldKey field =
    case field of
        Field description _ _ ->
            description.key

        AdditionalProperty description _ _ ->
            description.key


fieldIsAdditionalProperties : Field -> Bool
fieldIsAdditionalProperties field =
    case field of
        AdditionalProperty _ _ _ ->
            True

        _ ->
            False


fieldHasValue : Field -> Bool
fieldHasValue field =
    case field of
        Field _ _ object ->
            objectHasValue object

        AdditionalProperty _ _ object ->
            objectHasValue object


fieldDup : Field -> Field
fieldDup field =
    case field of
        Field description name object ->
            let
                updatedObject =
                    objectDup object
            in
            Field description name updatedObject

        AdditionalProperty description _ object ->
            let
                updatedObject =
                    objectDup object
            in
            AdditionalProperty description "" updatedObject


fieldReKey : String -> Field -> Field
fieldReKey newPrefix field =
    case field of
        Field description name object ->
            let
                len =
                    String.length newPrefix

                newKey =
                    newPrefix ++ String.dropLeft len description.key

                updatedObject =
                    objectReKey newPrefix object
            in
            Field { description | key = newKey } name updatedObject

        AdditionalProperty description _ object ->
            let
                len =
                    String.length newPrefix

                newKey =
                    newPrefix ++ String.dropLeft len description.key

                updatedObject =
                    objectReKey newPrefix object
            in
            AdditionalProperty { description | key = newKey } "" updatedObject


objectIsExpandable : Object -> Bool
objectIsExpandable object =
    case object of
        Object _ ->
            True

        Array _ ->
            True

        Value _ _ ->
            False

        InfoOnly _ ->
            True


objectHasValue : Object -> Bool
objectHasValue object =
    case object of
        Value _ value ->
            value /= ""

        Object fields ->
            List.any fieldHasValue fields

        Array elements ->
            List.any objectHasValue elements

        InfoOnly _ ->
            False


objectHasAdditionalProperties : Object -> Bool
objectHasAdditionalProperties object =
    case object of
        Object fields ->
            List.any fieldIsAdditionalProperties fields

        _ ->
            False


objectIsListType : Object -> Bool
objectIsListType object =
    case object of
        Value _ _ ->
            False

        Array _ ->
            True

        Object fields ->
            List.any fieldIsAdditionalProperties fields

        InfoOnly _ ->
            False


objectAddElement : String -> Object -> Object
objectAddElement newPrefix object =
    case object of
        Value _ _ ->
            object

        Array list ->
            case list of
                [] ->
                    Array list

                x :: _ ->
                    let
                        newObject =
                            objectReKey newPrefix (objectDup x)
                    in
                    Array (List.append list [ newObject ])

        Object fields ->
            case fields of
                [] ->
                    Object fields

                x :: _ ->
                    let
                        newField =
                            fieldReKey newPrefix (fieldDup x)
                    in
                    Object (List.append fields [ newField ])

        InfoOnly _ ->
            object


objectRemoveElement : Object -> Object
objectRemoveElement object =
    case object of
        Value _ _ ->
            object

        Array list ->
            if List.length list == 1 then
                object

            else
                Array (list |> List.reverse |> List.drop 1 |> List.reverse)

        Object fields ->
            if List.length fields == 1 then
                object

            else
                Object (fields |> List.reverse |> List.drop 1 |> List.reverse)

        InfoOnly _ ->
            object


objectDup : Object -> Object
objectDup object =
    case object of
        Value key _ ->
            Value key ""

        Array list ->
            Array (List.map objectDup list)

        Object fields ->
            Object (List.map fieldDup fields)

        InfoOnly description ->
            InfoOnly description


objectLength : Object -> Int
objectLength object =
    case object of
        Value _ _ ->
            0

        Array list ->
            List.length list

        Object fields ->
            List.length fields

        InfoOnly _ ->
            0


objectReKey : String -> Object -> Object
objectReKey newPrefix object =
    case object of
        Value key value ->
            let
                len =
                    String.length newPrefix

                newKey =
                    newPrefix ++ String.dropLeft len key
            in
            Value newKey value

        Array list ->
            Array (List.map (objectReKey newPrefix) list)

        Object fields ->
            Object (List.map (fieldReKey newPrefix) fields)

        InfoOnly _ ->
            object


fromSchema : String -> Defs.Schema -> Dict.Dict String Defs.Schema -> Object
fromSchema parentKey schema schemas =
    case schema of
        Defs.BooleanSchema _ ->
            Value (parentKey ++ "-unknown") "unknown"

        Defs.ObjectSchema subschema ->
            fromSubschema parentKey subschema schemas


fromSubschema : String -> Defs.SubSchema -> Dict.Dict String Defs.Schema -> Object
fromSubschema parentKey subschema schemas =
    case subschema.type_ of
        Defs.SingleType st ->
            case st of
                Defs.ObjectType ->
                    let
                        fields =
                            propertiesToFields parentKey subschema.properties schemas

                        additionalProperty =
                            Maybe.map (\p -> schemaToAdditionalProperty parentKey "0" p schemas) subschema.additionalProperties

                        allFields =
                            case additionalProperty of
                                Nothing ->
                                    fields

                                Just propField ->
                                    List.append fields [ propField ]
                    in
                    if List.length allFields == 0 then
                        let
                            description =
                                Maybe.withDefault "" subschema.description
                        in
                        InfoOnly description

                    else
                        Object allFields

                Defs.ArrayType ->
                    Array (itemsToObjects parentKey subschema.items schemas)

                _ ->
                    Value parentKey ""

        Defs.AnyType ->
            let
                ref =
                    subschema.ref |> Maybe.map (String.dropLeft (String.length "#/definitions/"))

                definition =
                    ref |> Maybe.andThen (\a -> Dict.get a schemas)
            in
            case definition of
                Nothing ->
                    Array []

                Just schema ->
                    fromSchema parentKey schema schemas

        _ ->
            Value "unused" ""


itemsToObjects : String -> Defs.Items -> Dict.Dict String Defs.Schema -> List Object
itemsToObjects parentKey items schemas =
    case items of
        Defs.NoItems ->
            []

        Defs.ItemDefinition schema ->
            [ fromSchema (parentKey ++ ".0") schema schemas ]

        Defs.ArrayOfItems list ->
            List.map (\schema -> fromSchema parentKey schema schemas) list


propertiesToFields : String -> Maybe Defs.Schemata -> Dict.Dict String Defs.Schema -> List Field
propertiesToFields parentKey maybeProperties schemas =
    case maybeProperties of
        Nothing ->
            []

        Just (Defs.Schemata properties) ->
            let
                fields =
                    List.map (\( name, schema ) -> schemaToField parentKey name schema schemas) properties
            in
            List.sortWith compareFields fields


compareFields : Field -> Field -> Order
compareFields left right =
    let
        leftName =
            fieldName left

        rightName =
            fieldName right
    in
    compare leftName rightName


schemaToField : String -> String -> Defs.Schema -> Dict.Dict String Defs.Schema -> Field
schemaToField parentKey name schema schemas =
    let
        key =
            parentKey ++ "." ++ name

        object =
            fromSchema key schema schemas

        visible =
            flip (objectIsExpandable object)
                || (case name of
                        "spec" ->
                            True

                        "data" ->
                            True

                        "binaryData" ->
                            True

                        "stringData" ->
                            True

                        _ ->
                            False
                   )

        description =
            { description = descriptionFromSchema schema
            , key = key
            , type_ = typeFromSchema schema
            , visible = visible
            }
    in
    Field description name object


schemaToAdditionalProperty : String -> String -> Defs.Schema -> Dict.Dict String Defs.Schema -> Field
schemaToAdditionalProperty parentKey name schema schemas =
    let
        key =
            parentKey ++ "." ++ name

        description =
            { description = descriptionFromSchema schema
            , key = key
            , type_ = typeFromSchema schema
            , visible = False
            }
    in
    AdditionalProperty description "" (fromSchema key schema schemas)


descriptionFromSchema : Defs.Schema -> String
descriptionFromSchema schema =
    case schema of
        Defs.BooleanSchema _ ->
            "unknown"

        Defs.ObjectSchema subschema ->
            Maybe.withDefault "" subschema.description


typeFromSchema : Defs.Schema -> String
typeFromSchema schema =
    case schema of
        Defs.BooleanSchema _ ->
            "unknown"

        Defs.ObjectSchema subschema ->
            typeToString subschema.type_


typeToString : Defs.Type -> String
typeToString type_ =
    case type_ of
        Defs.AnyType ->
            "*"

        Defs.SingleType st ->
            singleTypeToString st

        Defs.NullableType st ->
            "nullable " ++ singleTypeToString st

        Defs.UnionType ut ->
            String.join ", " (List.map singleTypeToString ut)


singleTypeToString : Defs.SingleType -> String
singleTypeToString st =
    case st of
        Defs.IntegerType ->
            "int"

        Defs.NumberType ->
            "num"

        Defs.StringType ->
            "string"

        Defs.BooleanType ->
            "bool"

        Defs.ArrayType ->
            "array"

        Defs.ObjectType ->
            "object"

        Defs.NullType ->
            "null"


objectToYaml : Int -> Object -> String
objectToYaml indent object =
    if objectHasValue object == False then
        ""

    else
        let
            indentStr =
                String.repeat indent "  "
        in
        case object of
            Value _ value ->
                value

            Object fields ->
                let
                    fieldYamls =
                        List.map (fieldToYaml indent) fields
                in
                String.join "\n" (List.filter (\s -> s /= "") fieldYamls)

            Array elements ->
                let
                    elementYamls =
                        List.map (objectToYaml (indent + 1)) elements

                    dashedYamls =
                        List.map (\e -> indentStr ++ "- " ++ String.trimLeft e) elementYamls
                in
                String.join "\n" (List.filter (\s -> s /= "") dashedYamls)

            InfoOnly _ ->
                ""


fieldToYaml : Int -> Field -> String
fieldToYaml indent field =
    if fieldHasValue field == False then
        ""

    else
        let
            indentStr =
                String.repeat indent "  "
        in
        case field of
            Field _ name object ->
                case object of
                    Value _ _ ->
                        indentStr ++ name ++ ": " ++ objectToYaml (indent + 1) object

                    Object _ ->
                        indentStr ++ name ++ ":\n" ++ objectToYaml (indent + 1) object

                    Array _ ->
                        indentStr ++ name ++ ":\n" ++ objectToYaml (indent + 1) object

                    InfoOnly _ ->
                        ""

            AdditionalProperty _ name object ->
                case object of
                    Value _ _ ->
                        indentStr ++ name ++ ": " ++ objectToYaml (indent + 1) object

                    Object _ ->
                        indentStr ++ name ++ ":\n" ++ objectToYaml (indent + 1) object

                    Array _ ->
                        indentStr ++ name ++ ":\n" ++ objectToYaml (indent + 1) object

                    InfoOnly _ ->
                        ""


flip : Bool -> Bool
flip b =
    if b then
        False

    else
        True
