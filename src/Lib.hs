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
        indentStr = List.replicate 4 '.'
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
        whitespaceLines = 3
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

inputName :: IO (Maybe String)
inputName = do
    putStrLn "Name of msg? (empty for none)"
    name <- getLine
    if name == [] then
        return Nothing
    else if isValidType name then
        return (Just name)
    else
        inputName

inputTypeList :: IO [String]
inputTypeList =
    let
        inner :: [String] -> IO [String]
        inner other = do
            putStrLn "Name of type? (empty to finnish type list)"
            name <- getLine
            if name == [] then
                return other
            else if isValidType name then
                inner $ other ++ [name]
            else
                inner other
    in do
        inner []

inputMsg :: IO (Maybe Msg)
inputMsg = do
    name <- inputName
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




someFunc :: IO ()
someFunc =
    let
        fullCodeBuilder msgs =
            [ buildImportStatements []
            , addHeaderComment "Messages" $ msgStringsFromMsgs msgs
            , addHeaderComment "Update" $ buildUpdateFunction msgs
            , addHeaderComment "View" $ buildViewFunction
            ]
    in do
        msgs <- inputMsgs
        putStrLn $ stringListToMultiline $ interCodeWhitespace $ fullCodeBuilder msgs


