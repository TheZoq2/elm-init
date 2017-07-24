module Lib
    ( someFunc
    , interCodeWhitespace
    ) where

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.String as String

{-
  Takes a list of strings and creates a multi line string where
  each line is the element of a list
-}
stringListToMultiline :: [String] -> String
stringListToMultiline strings =
    String.unlines strings

{-
  Adds 4*amount spaces in front of the specified string
-}

indent :: Int -> String -> String
indent amount original =
    let
        indentStr = List.replicate 4 ' '
    in
        (List.concat $ List.replicate amount indentStr) ++ original



{-
  Representation of an elm message entry
-}

data Msg  = Msg {msgName :: String, msgParams :: [String]}

{-
  Converts a single msg into a string
-}
msgToString :: Msg -> String
msgToString Msg {msgName=name, msgParams=params} =
    name ++ " " ++ (List.intercalate " " params)


{-
  Stores an elm record. Each member stores (name, type, default value)
-}

data Record = Record {recordName :: String, recordMembers :: [(String, String, String)]}

{-
  Creates a string representation of an elm record
-}

recordToStrings :: Record -> [String]
recordToStrings Record {recordName=name, recordMembers=members} =
    let
        memberStrings =
            case members of
              [] ->
                ["{}"]
              _ ->
                  ( List.map
                    (\(separator, (name, typeName, _)) -> separator ++ name ++ ": " ++ typeName)
                    $ List.zip (["{ "] ++ (List.repeat ", ")) members
                  )
                  ++ [indent 1 "}"]

    in
        [ "type alias " ++ name ]
        ++ ( List.map (indent 1) memberStrings )


{-
  Creates an init function for a record
-}

recordInitFunction :: String -> Record -> [String]
recordInitFunction name (Record {recordName=recordName, recordMembers = members}) =
    let
        typeSignature = name ++ " : " ++ recordName
        declaration = "name ="
    in
        [ typeSignature
        , declaration
        , indent 1 $
            recordName
                ++ " "
                ++ ( List.concat
                      $ List.intersperse " "
                      $ List.map (\(_, _, defaultValue) -> defaultValue) members
                   )
        ]


{-
  Creates a Msg type declaration from the specified msgs
-}

msgStringsFromMsgs :: [Msg] -> [String]
msgStringsFromMsgs [] = msgStringsFromMsgs [Msg "Nothing" []]
msgStringsFromMsgs msgs =
    [ "type Msg"
    ]
    ++ ( List.map (indent 1)
            $ List.zipWith (++) (["= "] ++ (List.repeat "| "))
            $ List.map msgToString msgs
       )



{-
  Builds a single case option from a return value and a Msg
-}
buildCaseOption :: String -> Msg -> [String]
buildCaseOption returnValue (Msg {msgName=name, msgParams=params}) =
    [ name
        ++ " -> "
        ++ ( List.foldl (++) ""
                $ List.intersperse " "
                $ List.map (\(datatype, number) ->
                    (List.map Char.toLower datatype) ++ show number)
                $ List.zip params (List.iterate (\a -> a+1) 0)
           )
    ]
    ++ [(indent 1 returnValue)]

{-
  Maps buildCaseOption
-}

buildCaseOptions :: String -> [Msg] -> [String]
buildCaseOptions returnValue msgs =
    List.concatMap (buildCaseOption returnValue) msgs



{-
  Constructs an update function from a list of msgs
-}
buildUpdateFunction :: [Msg] -> [String]
buildUpdateFunction msgs =
    let
        typeAnotation = "update : Msg -> Model -> (Model, Cmd Msg)"
        functionDeclaration = "update msg model ->"
        caseHead = "case msg of"

        defaultReturn = "(model, Cmd.none)"
    in
        ( [ typeAnotation
          , functionDeclaration
          , indent 1 caseHead
          ]
          ++ (List.map (indent 2) $ buildCaseOptions defaultReturn msgs)
        )

buildViewFunction :: [String]
buildViewFunction =
    let
        typeAnotation = "view : Model -> Html Msg"
        functionDeclaration = "view model ="
        functionBody = "div [] []"
    in
        [ typeAnotation
        , functionDeclaration
        , indent 1 functionBody
        ]


data ModuleImport
  = Name String
  | Exposing String [String]
  | ExposingAll String


moduleImportToString :: ModuleImport -> String
moduleImportToString moduleImport =
    case moduleImport of
        Name name ->
            "import " ++ name
        Exposing name exposed ->
            "import " ++ name ++ (List.intercalate " " exposed)
        ExposingAll name ->
            "import " ++ name ++ " exposing (..)"

{-
  Creates import statements for the specified imports plus
  some default imports
-}

buildImportStatements :: [ModuleImport] -> [String]
buildImportStatements modules =
    let
        defaultModules =
            [ Name "html"
            , ExposingAll "html"
            , ExposingAll "Html.Attributes"
            , ExposingAll "Html.Events"
            ]
    in
        List.map moduleImportToString $ defaultModules ++ modules


buildSubscriptionFunction :: [String]
buildSubscriptionFunction =
    ["subscriptions :: Model -> Sub Msg"]
    ++ ["subscriptions _ ="]
    ++ [indent 1 "Sub.none"]

buildProgramFunction :: [String]
buildProgramFunction =
    [ "main : Program Never Model Msg"
    , "main ="
    , "    Html.program"
    , "        { init = init"
    , "        , update = update"
    , "        , view = view"
    , "        , subscriptions = subscriptions"
    , "        }"
    ]

{-
  Adds a header comment to the specified code

  -- <comment>

  code
    ...
-}

addHeaderComment :: String -> [String] -> [String]
addHeaderComment comment content =
    [ "-- " ++ comment
    , ""
    ]
    ++ content


{-
  Adds whitespace between chunks of code (lists of strings)
-}

interCodeWhitespace :: [[String]] -> [String]
interCodeWhitespace blocks =
    let
        whitespaceLines = 2
    in
        List.foldl (++) []
            $ List.map (\(a, b) -> a ++ b)
            $ List.zip blocks
            $ List.repeat
            $ List.take whitespaceLines
            $ List.repeat ""


isValidType :: String -> Bool
isValidType name =
    -- TODO Actually validate this
    True

inputTypeName :: String -> IO (Maybe String)
inputTypeName typeName = do
    putStrLn ("Type of " ++ typeName ++ "? (empty for none)")
    name <- getLine
    if name == "" then
        return Nothing
    else if isValidType name then
        return (Just name)
    else
        inputTypeName typeName


inputTypeList :: IO [String]
inputTypeList =
    let
        inner :: [String] -> IO [String]
        inner other = do
            name <- inputTypeName "type"
            case name of
              Nothing ->
                return other
              Just name ->
                inner $ other ++ [name]
    in do
        inner []


inputMsg :: IO (Maybe Msg)
inputMsg = do
    name <- inputTypeName "Msg"
    case name of
      Just name -> do
        types <- inputTypeList
        return $ Just $ Msg name types
      Nothing ->
        return Nothing


inputMsgs :: IO [Msg]
inputMsgs =
    let
        inner other = do
            msg <- inputMsg
            case msg of
              Just msg ->
                inner $ other ++ [msg]
              Nothing ->
                return other
    in
        inner []


inputRecordField :: IO (Maybe (String, String, String))
inputRecordField =
    do
        putStrLn "Name of field (empty for no field)"
        name <- getLine
        if name == "" then
            return Nothing
        else do
            typeName <- inputFieldType name
            putStrLn $ "Default value of " ++ name
            value <- getLine
            return $ Just (name, typeName, value)
    where
        inputFieldType :: String -> IO String
        inputFieldType fieldName =
            do
                fieldType <- inputTypeName fieldName
                case fieldType of
                  Just name ->
                    return name
                  Nothing ->
                      inputFieldType fieldName


inputModel :: IO (Record)
inputModel =
    do
        fields <- inner []
        return $ Record "Model" fields
    where
        inner :: [(String, String, String)] -> IO [(String, String, String)]
        inner acc =
            do
                field <- inputRecordField
                case field of
                    Just field ->
                        inner (acc ++ [field])
                    Nothing ->
                        return acc




someFunc :: IO ()
someFunc =
    let
        fullCodeBuilder msgs model =
            [ buildImportStatements []
            , addHeaderComment "Model" $ (recordToStrings model) ++ (recordInitFunction "init" model)
            , addHeaderComment "Messages" $ msgStringsFromMsgs msgs
            , addHeaderComment "Update" $ buildUpdateFunction msgs
            , addHeaderComment "View" $ buildViewFunction
            , addHeaderComment "Subscriptions" $ buildSubscriptionFunction
            , addHeaderComment "Program" $ buildProgramFunction
            ]
    in do
        msgs <- inputMsgs
        model <- inputModel
        putStrLn $ stringListToMultiline $ interCodeWhitespace $ fullCodeBuilder msgs model


