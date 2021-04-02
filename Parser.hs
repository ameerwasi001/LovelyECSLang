{-# LANGUAGE LambdaCase #-}

module Parser where

import Data.Void ( Void )
import Data.Char ( toLower )
import Data.List ( intercalate, partition )
import Text.Megaparsec as P hiding (State)
import Text.Megaparsec.Char ( string, char, space )
import Debug.Trace ( trace )
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as Set

data Node =
    StringNode String SourcePos
    | NumNode String SourcePos
    | IdentifierNode String SourcePos
    | BoolNode String SourcePos
    | BinOpNode Node String Node SourcePos
    | PatternNode Node SourcePos
    | IndexNode Node [Node] SourcePos
    | EntityNode String [Node] [Node] [Node] [Node] [Node]
    | TupleNode [Node] SourcePos
    | MultipleNode [Node]
    | ForNode Node Node Node [Node] SourcePos
    | ForInNode [Node] Node [Node] SourcePos
    | SystemNode String [Node] [Node] Node [Node] [Node] SourcePos
    | StateNode String [Node] [Node] [Node] [Node] SourcePos
    | TableNode [(String, Node)] SourcePos
    | IncludeNode String Node SourcePos
    | StaticVarNode Node Node SourcePos
    | CallNode Node [[Node]] SourcePos
    | LuaTableNode [Node] SourcePos
    | MethodDefNode (Maybe Node) [Node] [Node] SourcePos
    | IfNode Node [Node] [Node] SourcePos
    | ComponentsNode [Node]
    | AssignmentNode [Node] Node SourcePos
    | UnaryNode String Node SourcePos
    | ArrayNode [Node] SourcePos
    | ModuleNode String [Node] [Node] [Node] SourcePos
    | LuaStmntNode String SourcePos
    | ReturnNode Node SourcePos

indent :: String -> String
indent xs = intercalate "\n" $ map ("\t" ++) (lines xs)

getInfix :: [a] -> [a]
getInfix = init . tail

fromParens :: [Char] -> [Char]
fromParens xs = if head xs == '(' && last xs == ')' then getInfix xs else xs

removeInList :: Eq a => [a] -> [a] -> [a]
removeInList ls = filter (not . (`elem` ls))

compile :: Node -> String
compile (StringNode str _) = show str 
compile (NumNode n _) = n
compile (BoolNode b _) = b
compile (UnaryNode sign n _) = "(" ++ sign ++ "(" ++ compile n ++ "))"
compile (ArrayNode ns _) = "{" ++ intercalate ", " (map compile ns) ++ "}"
compile (PatternNode n _) = "tiny.filter(\"" ++ filter (\x -> x /= ' ' && x /= '"') (compile n) ++"\")"
compile (IdentifierNode id _) = id
compile (IndexNode exp inds _) = compile exp ++ intercalate "" (map (\x -> "[" ++ compile x ++ "]") inds)
compile (TupleNode ts _) = "(" ++ intercalate ", " (map compile ts) ++ ",)"
compile (EntityNode id args statics incs ns ms) = 
    intercalate ";\n" (map compile incs) ++ ";\n\nlocal " ++ 
    id ++ " = class(\"" ++ id ++ "\");\n\n" ++ 
    (
        case statics of
            [] -> ""
            _ -> intercalate ";\n" (map (((id ++ ".") ++) . compile) statics) ++ ";\n\n"
        ) ++
    "function " ++ id ++ ":init(" ++ intercalate ", " (map compile args) ++ ")\n" ++ indent (intercalate ";\n" (map compile ns)) ++
    ";\nend\n" ++ intercalate "\n\n" (map compile ms) ++ "\n\nreturn " ++ id
compile (ComponentsNode ns) = 
    intercalate ";\n" (map compile ns) ++ "\n\nreturn {\n" ++ indent (intercalate ",\n" (map getName ns)) ++ "\n}" where
        getName (MethodDefNode id _ _ _) = maybe "" compile id ++ " = " ++ maybe "" compile id
compile (BinOpNode a op b _) = case op of 
    "." -> "(" ++ compile a ++ op ++ compile b ++ ")"
    _ -> "(" ++ compile a ++ " " ++ op ++ " " ++ compile b ++ ")"
compile (LuaStmntNode str _) = str
compile (IncludeNode fn asId _) = "local " ++ compile asId ++ " = require \"" ++ fn ++ "\""
compile (StaticVarNode id expr _) = compile id ++ " = " ++ compile expr
compile (MethodDefNode id args body _) = 
    "function " ++ filter (\x -> x /= '(' && x /= ')') (maybe "" compile id) ++ "(" ++ intercalate ", " (map compile args) ++ ")\n" ++
    indent (declLocals body ++ "\n" ++ intercalate ";\n" (map compile body) ++ ";\n") ++
    "\nend"
    where
        declLocals ds = intercalate ";\n" $ Set.toList $ Set.fromList $ concatMap (decl []) ds

        decl :: [String] -> Node -> [String]
        decl ds (AssignmentNode xs _ _) = ds ++ concatMap (decl ds) xs
        decl ds (IdentifierNode x _) = ds ++ ["local " ++ x]
        decl ds (IfNode _ xs ys _) = ds ++ concatMap (decl ds) xs ++ concatMap (decl ds) ys
        decl ds (ForNode _ _ _ body _) = concatMap (decl ds) body
        decl ds (ForInNode _ _ body _) = concatMap (decl ds) body
        decl ds (MultipleNode ns) = concatMap (decl ds) ns
        decl ds (MethodDefNode id _ _ _) = ds ++ concatMap (decl ds) id
        decl ds _ = ds

compile (ReturnNode n pos) = "return " ++ compile n
compile (TableNode kvs pos) = 
    "{\n" ++ indent (intercalate ";\n" (map showPair kvs)) ++ "\n}" where showPair (k, v) = k ++ " = " ++ compile v
compile (LuaTableNode ns pos) = "{\n" ++ indent (intercalate ";\n" (map compile ns)) ++ "\n}"
compile (AssignmentNode lhs rhs pos) = intercalate ", " (map (fromParens . compile) lhs) ++ " = " ++ compile rhs
compile (CallNode id argsLists _) = fromParens (compile id) ++ concatMap (\xs -> "(" ++ intercalate ", " (map compile xs) ++ ")") argsLists
compile (ForNode id a b body _) = "for " ++ compile id ++ " = " ++ compile a ++ ", " ++ compile b ++ " do\n" ++ indent (intercalate ";\n" $ map compile body) ++ "\nend"
compile (ForInNode newIds expr body pos) = "for " ++ intercalate ", " (map compile newIds) ++ " in " ++ compile expr ++ " do\n" ++ indent (intercalate ";\n" $ map compile body) ++ "\nend"
compile (MultipleNode stmnts) = intercalate ";\n" (map compile stmnts)
compile (IfNode cond exprs elses _) = 
    "if " ++ compile cond ++ " then " ++ "\n" ++ indent (intercalate ";\n" (map compile exprs)) ++ (
        case elses of
            [] -> ""
            xs -> "\nelse\n" ++ indent (intercalate ";\n" (map compile xs))
    ) ++ ";\nend"
compile (SystemNode id args incs filt ns methods _) = 
    (
        case incs of 
            [] -> ""
            _ -> intercalate ";\n" (map compile incs) ++ "\n\n"
    ) ++
    "local " ++ id ++ " = tiny.processingSystem(class(\"" ++ id ++ "\"));\n\n" ++ 
    id ++ ".filter = " ++ compile filt ++ ";\n" ++
    "function " ++ id ++ ":init(" ++ intercalate ", " (map compile args) ++ ")\n" ++ indent (intercalate ";\n" (map compile ns)) ++
    "\nend\n" ++ intercalate "\n\n" (map compile methods) ++ "\n\nreturn " ++ id
compile (StateNode id args incs ns methods _) = 
    (
        case incs of 
            [] -> ""
            _ -> intercalate ";\n" (map compile incs) ++ "\n\n"
    ) ++
    "local " ++ id ++ " = class(\"" ++ id ++ "\");\n\n" ++ 
    "function " ++ id ++ ":init(" ++ intercalate ", " (map compile args) ++ ")\n" ++ indent (intercalate ";\n" (map compile ns)) ++
    "\nend;\n" ++ intercalate "\n\n" (map compile methods) ++ "\n\nreturn " ++ id
compile (ModuleNode id args incs ns _) =
    "class = require \"lib.30log\";\ntiny = require \"lib.tiny\";\ngamestate = require \"lib.gamestate\";\nbeholder = require \"lib.beholder\";\n" ++
    (
        case incs of 
            [] -> ""
            _ -> intercalate ";\n" (map compile incs) ++ "\n\n"
    ) ++ (if id == "love" then "self = love;\n" else id ++ " =  class(\"" ++ id ++ "\");\nself = " ++ id ++ ";\n") ++ "\n" ++
    intercalate ";\n" (map compile ns) ++
    "\n\nreturn " ++ (if null args then id else "{\n" ++ indent (intercalate ";\n" (map (\a -> compile a ++ " = " ++ compile a) args)) ++ "\n}")

extractString :: Node -> String
extractString (StringNode s _) = s
extractString (NumNode n _) = n
extractString (IdentifierNode id _) = id
extractString (BoolNode b _) = b

getList :: Node -> [Node]
getList (TupleNode as _) = as

lower :: Parser Char
lower = oneOf "abcdefghijklmnopqrstuvwxyz" :: Parser Char
upper = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: Parser Char
digit = oneOf "1234567890" :: Parser Char
newline = oneOf "\n;" :: Parser Char
newlines = P.many Parser.newline
skipLines = newlines *> spaces *> newlines *> spaces
space = oneOf " " :: Parser Char
spaces = P.many Parser.space
mspaces = Parser.space *> Parser.spaces
dollar = oneOf "$" :: Parser Char
keyword k = Text.Megaparsec.Char.string (showL k) :: Parser String

notKeyword = try $ notFollowedBy $ choice keywords *> Text.Megaparsec.Char.string " " where
    keywords = map ((\a -> Text.Megaparsec.Char.string a :: Parser String) . showL) [If ..]

showL k = toLower x : xs where (x:xs) = show k

data Keyword =
    If
    | Else
    | True
    | False
    | Not
    | And
    | Or
    | Entity
    | Component
    | State
    | System
    | Include
    | As
    | When
    | KnownAs
    | New
    | Return
    | Global
    | For
    | Static
    | Module
    | In
    deriving(Show, Eq, Enum)

type Parser = Parsec Void String

eofParser :: Parser String
eofParser = "" <$ eof

stringParser :: Char -> Parser Node
stringParser c =
    do
        pos <- getSourcePos
        str <- (char c *> manyTill L.charLiteral (char c)) :: Parser String
        return $ StringNode str pos

numberParser :: Parser Node
numberParser =
    do
        pos <- getSourcePos
        fs <- digit :: Parser Char
        str <- P.many digit :: Parser String
        return $ NumNode (fs : str) pos

fractionalParser :: Parser Node
fractionalParser =
    do
        pos <- getSourcePos
        dec <- numberParser
        Text.Megaparsec.Char.string "." :: Parser String
        frac <- numberParser
        return $ NumNode (extractString dec ++ "." ++ extractString frac) pos

identifierPrefixParser :: Parser Char -> Parser Char -> Parser Node
identifierPrefixParser fs sn =
    do
        pos <- getSourcePos
        notKeyword
        fc <- fs
        lst <- P.many sn
        let ident = modify (fc : lst) pos
        return ident
    where 
        modify ['$'] pos = IdentifierNode "self" pos
        modify ('$':xs) pos = BinOpNode (IdentifierNode "self" pos) "." (IdentifierNode xs pos) pos
        modify xs pos = IdentifierNode xs pos

identifierParser :: Parser Char -> Parser Char -> Parser Node
identifierParser fs sn = (\exp pos inds -> 
    case inds of
        [] -> exp
        _ -> IndexNode exp inds pos
        )
    <$> identifierPrefixParser fs sn
    <*> getSourcePos 
    <*> indexes

booleanParser :: Parser Node
booleanParser =
    do
        pos <- getSourcePos
        b <- keyword Parser.True <|> keyword Parser.False
        return $ BoolNode b pos

wholeStringParser :: Parser Node
wholeStringParser = stringParser '\"' <|> stringParser '\''

binOp f ops ret = do
  t1 <- f
  loop t1
  where termSuffix t1 = try (do
          pos <- getSourcePos
          spaces
          op <- ops
          spaces
          t2 <- f
          loop (ret t1 op t2 pos))
        loop t = termSuffix t <|> return t

exprParser :: Parser Node
exprParser = chnageBinOp <$> binOp compExprParser (Text.Megaparsec.Char.string "&" <|> Text.Megaparsec.Char.string "|") BinOpNode where
    chnageBinOp (BinOpNode a "&" b pos) = BinOpNode (chnageBinOp a) "and" (chnageBinOp b) pos
    chnageBinOp (BinOpNode a "|" b pos) = BinOpNode (chnageBinOp a) "or" (chnageBinOp b) pos
    chnageBinOp n = n

compExprParser :: Parser Node
compExprParser = binOp arithExprParser ops BinOpNode where
    ops =
        (
        Text.Megaparsec.Char.string "==" 
        <|> Text.Megaparsec.Char.string "~="
        <|> Text.Megaparsec.Char.string ">="
        <|> Text.Megaparsec.Char.string ">"
        <|> Text.Megaparsec.Char.string "<="
        <|> Text.Megaparsec.Char.string "<"
        ) :: Parser String

arithExprParser :: Parser Node
arithExprParser = binOp termParser (Text.Megaparsec.Char.string "+" <|> Text.Megaparsec.Char.string "-") BinOpNode

termParser :: Parser Node
termParser = binOp concatParser (Text.Megaparsec.Char.string "*" <|> Text.Megaparsec.Char.string "/") BinOpNode

concatParser :: Parser Node
concatParser = binOp atomParser (Text.Megaparsec.Char.string "..") BinOpNode

parenParser :: Parser Node
parenParser = Text.Megaparsec.Char.string "(" *> skipLines *> exprParser <* skipLines <* char ')'

accessParser :: Parser Node -> Parser Node
accessParser ident = binOp ident (Text.Megaparsec.Char.string ".") BinOpNode

blockParser :: Parser Node -> Parser [Node]
blockParser p = Text.Megaparsec.Char.string "{" *> P.many (skipLines *> p <* skipLines) <* Text.Megaparsec.Char.string "}"

blockOrExprParser :: Parser [Node]
blockOrExprParser = blockParser programStmntParser <|> ((\pos a -> [ReturnNode a pos]) <$> getSourcePos <*> exprParser)

dollarId :: Parser Node
dollarId = identifierParser (lower <|> dollar) (lower <|> upper <|> digit <|> oneOf "_")

smallId :: Parser Node
smallId = identifierParser lower (lower <|> upper <|> digit <|> oneOf "_")

bigId :: Parser Node
bigId = identifierParser upper (lower <|> upper <|> digit <|> oneOf "_")

eitherId :: Parser Node
eitherId = identifierParser (lower <|> upper) (lower <|> upper <|> digit <|> oneOf "_")

anyId :: Parser Node
anyId = identifierParser (lower <|> dollar <|> upper) (lower <|> upper <|> digit <|> oneOf "_")

methodParser :: Parser Node
methodParser = do
    pos <- getSourcePos
    id <- accessParser dollarId <* spaces
    args <- tuple <* spaces <* Text.Megaparsec.Char.string "=>" <* spaces
    body <- blockOrExprParser
    return $ MethodDefNode (Just id) (IdentifierNode "self" pos : getList args) body pos

funParser :: Parser Node
funParser = do
    pos <- getSourcePos
    id <- accessParser dollarId <* spaces
    args <- Parser.containerFunction "{" "}" "," TupleNode exprParser <* spaces <* Text.Megaparsec.Char.string "=>" <* spaces
    body <- blockOrExprParser
    return $ MethodDefNode (Just id) (getList args) body pos

returnParser :: Parser Node
returnParser = flip ReturnNode <$> getSourcePos <*> (keyword Return *> spaces *> exprParser <* spaces)

forLoopParser :: Parser Node
forLoopParser =
    (\pos var a b body -> ForNode var a b body pos)
    <$> getSourcePos
    <*> (keyword For *> spaces *> eitherId <* spaces <* Text.Megaparsec.Char.string "<-" <* spaces)
    <*> (exprParser <* spaces <* Text.Megaparsec.Char.string "~" <* spaces)
    <*> (exprParser <* spaces)
    <*> blockOrExprParser

forInLoopParser :: Parser Node
forInLoopParser = 
    (\pos newIds expr body -> ForInNode newIds (CallNode (IdentifierNode "pairs" pos) [[expr]] pos) body pos)
    <$> getSourcePos
    <*> (keyword For *> spaces *> eitherId `sepBy1` try (spaces *> Text.Megaparsec.Char.string "," <* spaces) <* spaces <* Text.Megaparsec.Char.string "<-" <* spaces)
    <*> (exprParser <* spaces)
    <*> blockOrExprParser

forAsLoopParser :: Parser Node
forAsLoopParser = 
    (\pos newIds expr body -> ForInNode newIds (CallNode (IdentifierNode "ipairs" pos) [[expr]] pos) body pos)
    <$> getSourcePos
    <*> (keyword For *> spaces *> eitherId `sepBy1` try (spaces *> Text.Megaparsec.Char.string "," <* spaces) <* spaces <* keyword In <* spaces)
    <*> (exprParser <* spaces)
    <*> blockOrExprParser

programStmntParser :: Parser Node
programStmntParser = choice $ map try [
    globalAssignmentParser,
    newParser,
    methodParser,
    funParser,
    funCallParser,
    callParser,
    forInLoopParser,
    forLoopParser,
    assignmentParser,  
    ifThenElseParser, 
    returnParser,
    forAsLoopParser
    ]

systemStmntParser :: Parser Node
systemStmntParser = choice $ map try [
    globalAssignmentParser,
    methodParser,
    funParser,
    funCallParser,
    assignmentParser,  
    ifThenElseParser, 
    returnParser,
    forAsLoopParser
    ]

componentBodySmntParser :: Parser Node
componentBodySmntParser = choice $ map try [
    globalAssignmentParser,
    assignmentParser,
    methodParser,
    ifThenElseParser,
    forAsLoopParser,
    forLoopParser
    ]

newParser :: Parser Node
newParser = (\pos name args -> CallNode name [IdentifierNode "self" pos : getList args] pos)
    <$> getSourcePos
    <*> (spaces *> keyword New *> spaces *> accessParser eitherId <* spaces)
    <*> (spaces *> tuple <* spaces)

anonymousMethodParser :: Parser Node
anonymousMethodParser = (\pos args body -> MethodDefNode Nothing args body pos) 
    <$> getSourcePos 
    <*> ((\a pos -> IdentifierNode "self" pos : getList a) <$> tuple <*> getSourcePos) 
    <*> (spaces *> Text.Megaparsec.Char.string "=>"  *> spaces *> blockOrExprParser)

anonymousFunParser :: Parser Node
anonymousFunParser = (\pos args body -> MethodDefNode Nothing args body pos) 
    <$> getSourcePos 
    <*> (getList <$> containerFunction "{" "}" "," TupleNode exprParser) 
    <*> (spaces *> Text.Megaparsec.Char.string "=>" *> spaces *> blockOrExprParser)

callParser :: Parser Node
callParser = do
    pos <- getSourcePos
    name <- spaces *> accessParser dollarId <* spaces
    argsLists <- (:) <$> tuple <*> P.many tuple
    let lists = case name of 
            BinOpNode a _ _ _ -> (a : head ls) : tail ls where ls = map getList argsLists
            IdentifierNode{} -> map getList argsLists
    return $ CallNode name lists pos

funCallParser :: Parser Node
funCallParser = (\pos name argsLists -> CallNode name (map getList argsLists) pos)
    <$> getSourcePos
    <*> (spaces *> accessParser anyId <* spaces)
    <*> ((:) <$> containerFunction "{" "}" "," TupleNode exprParser <*> P.many (containerFunction "{" "}" "," TupleNode exprParser))

unaryParser :: Parser String -> Parser Node -> Parser Node
unaryParser signP nodeP = (\pos sign node -> UnaryNode sign node pos)
    <$> getSourcePos
    <*> (spaces *> signP <* spaces)
    <*> (spaces *> nodeP <* spaces)

negParser :: Parser Node
negParser = unaryParser (Text.Megaparsec.Char.string "-") atomParser

notParser :: Parser Node
notParser = unaryParser (keyword Not) compExprParser

hashParser :: Parser Node
hashParser = unaryParser (Text.Megaparsec.Char.string "#") atomParser

arrayParser :: Parser Node
arrayParser = flip ArrayNode
    <$> getSourcePos 
    <*> (Text.Megaparsec.Char.string "[" *> spaces *> exprParser `sepBy` try (spaces *> Text.Megaparsec.Char.string "," <* spaces) <* spaces <* Text.Megaparsec.Char.string "]")

knownParser :: Parser [Node]
knownParser = (\pos id tup -> 
        map 
            (\x -> MultipleNode 
                [
                    AssignmentNode [BinOpNode (IdentifierNode "self" pos) "." (IdentifierNode (firstLower $ extractString x) pos) pos] (BoolNode "true" pos) pos, 
                    AssignmentNode [BinOpNode (IdentifierNode "self" pos) "." (IdentifierNode ("is" ++ extractString x) pos) pos] (BoolNode "true" pos) pos
                    ]
                )
            (getList tup)
        )
    <$> getSourcePos
    <*> (spaces *> keyword KnownAs <* spaces)
    <*> containerFunction "(" ")" "," TupleNode (accessParser bigId)
    where firstLower (x:xs) = toLower x : xs

indexes :: Parser [Node]
indexes = P.many (Text.Megaparsec.Char.string "[" *> spaces *> exprParser <* spaces <* Text.Megaparsec.Char.string "]")

tableParser :: Parser Node
tableParser = flip LuaTableNode
    <$> getSourcePos 
    <*> (Text.Megaparsec.Char.string "@" *> spaces *> blockParser assignmentParser)

setParser :: Parser Node
setParser = (\pos ls -> LuaTableNode (map (\x -> AssignmentNode [IdentifierNode (extractString x) pos] (BoolNode "true" pos) pos) ls) pos)
    <$> getSourcePos <* spaces <* Text.Megaparsec.Char.string "#" <* spaces
    <*> (Text.Megaparsec.Char.string "{" *> spaces *> exprParser `sepBy` try (spaces *> Text.Megaparsec.Char.string "," <* spaces) <* spaces <* Text.Megaparsec.Char.string "}")

prefixParser :: Parser Node
prefixParser = choice $ map try [
    callParser,
    anonymousFunParser,
    funCallParser,
    accessParser dollarId,
    anonymousMethodParser,
    identifierParser (lower <|> dollar) (lower <|> upper <|> digit <|> oneOf "_"),
    fractionalParser,
    numberParser,
    parenParser,
    wholeStringParser,
    booleanParser,
    negParser,
    notParser,
    hashParser,
    arrayParser,
    tableParser,
    setParser
    ]

atomParser :: Parser Node
atomParser = (\exp pos inds -> 
    case inds of
        [] -> exp
        _ -> IndexNode exp inds pos
        )
    <$> prefixParser
    <*> getSourcePos 
    <*> indexes

includes :: Parser [Node]
includes = P.many $ skipLines *> include <* skipLines where 
    include = (\pos string asId -> IncludeNode (extractString string) asId pos) 
        <$> getSourcePos 
        <*> (keyword Include *> spaces *> wholeStringParser <* spaces)
        <*> (keyword As *> spaces *> eitherId <* spaces)

containerFunction :: String -> String -> String -> ([Node] -> P.SourcePos -> Node) -> Parser Node -> Parser Node
containerFunction strt end sep f p =
    do
        pos <- getSourcePos
        Text.Megaparsec.Char.string strt
        spaces
        exprs <- p `sepBy` comma
        spaces
        Text.Megaparsec.Char.string end
        return $ f exprs pos
    where comma = spaces *> Text.Megaparsec.Char.string sep <* spaces

tuple :: Parser Node
tuple = Parser.containerFunction "(" ")" "," TupleNode exprParser

maybeTuple :: Parser Node
maybeTuple = tuple <|> (TupleNode [] <$ Text.Megaparsec.Char.string "" <*> getSourcePos)

staticVarParser :: Parser Node
staticVarParser = (\pos ident expr -> StaticVarNode ident expr pos) 
    <$> getSourcePos 
    <*> (keyword Static *> spaces *> identifierParser lower (lower <|> upper <|> digit <|> oneOf "_") <* spaces)
    <*> (Text.Megaparsec.Char.string "=" *> spaces *> exprParser)

assignmentParser :: Parser Node
assignmentParser = (\pos lhs rhs -> AssignmentNode lhs rhs pos)
    <$> getSourcePos 
    <*> (
        accessParser dollarId `sepBy` try (spaces *> Text.Megaparsec.Char.string "," <* spaces) <* spaces <* Text.Megaparsec.Char.string "<-" <* spaces
        )
    <*> exprParser

globalAssignmentParser :: Parser Node
globalAssignmentParser = (\(AssignmentNode lhss rhs pos) -> 
    AssignmentNode 
        (map (\lhs -> BinOpNode (IdentifierNode "_G" pos) "." lhs pos) lhss)
        rhs 
        pos
    ) <$> (keyword Global *> spaces *> assignmentParser <* spaces)

ifThenElseParser :: Parser Node
ifThenElseParser = 
    do
        pos <- getSourcePos 
        ifCond <- keyword If *> spaces *> exprParser <* spaces
        thenExpr <- blockParser programStmntParser <* spaces
        elseExpr <- 
            (keyword Else *> spaces *> (blockParser programStmntParser <|> ((:[]) <$> ifThenElseParser)) <* spaces)
            <|> ([] <$ Text.Megaparsec.Char.string "")
        return $ IfNode ifCond thenExpr elseExpr pos

entityParser :: Parser Node
entityParser = do
    skipLines
    incs <- try includes
    name <- keyword Entity *> spaces *> identifierParser upper (lower <|> upper <|> digit <|> oneOf "_") <* spaces
    args <- maybeTuple <* spaces
    try (
        do
            knownAs <- knownParser <* spaces <* skipLines
            statics <- P.many (skipLines *> staticVarParser <* skipLines)
            ns <- programStmntParser `endBy` (eofParser <|> skipLines)
            return $ let (ms, nns) = methodsAndDefs name ns in EntityNode (extractString name) (getList args) statics incs (knownAs ++ nns) ms
        ) <|> (
        do
            statics <- P.many (try $ skipLines *> staticVarParser) <* skipLines
            ns <- programStmntParser `endBy` (eofParser <|> skipLines)
            return $ let (ms, nns) = methodsAndDefs name ns in EntityNode (extractString name) (getList args) statics incs nns ms
        )

componentParser :: Parser Node
componentParser = (\pos name args -> 
        MethodDefNode 
            name 
                (IdentifierNode "component" pos : getList args) 
                [
                    AssignmentNode 
                        [
                            BinOpNode 
                                (IdentifierNode "component" pos) 
                                "." 
                                (IdentifierNode (firstLower $ extractString <$> name) pos) 
                            pos
                        ]
                        (
                            case getList args of
                                [arg] -> arg
                                _ -> TableNode (zip (map compile (getList args)) (map ((`IdentifierNode` pos) . compile) (getList args))) pos
                        )
                        pos
                    ] 
                pos
            )
    <$> getSourcePos
    <*> (Just <$> (spaces *> eitherId <* spaces))
    <*> tuple
    where firstLower (Just (x:xs)) = toLower x : xs

componentBodyParser :: Parser Node
componentBodyParser = do
    component <- componentParser
    pos <- getSourcePos 
    body <- ([AssignmentNode [IdentifierNode "self" pos] (LuaTableNode [] pos) pos] ++) <$> (spaces *> Text.Megaparsec.Char.string "=>" *> spaces *> blockParser componentBodySmntParser)
    return $ modify component body
    where 
        modify (MethodDefNode name args body pos) modification = 
            MethodDefNode 
                name 
                args 
                (
                    modification ++ [
                        AssignmentNode 
                            [BinOpNode (IdentifierNode "component" pos) "." (IdentifierNode (firstLower $ extractString <$> name) pos) pos] 
                            (IdentifierNode "self" pos)
                            pos
                        ]
                    ) 
                pos
        firstLower (Just (x:xs)) = toLower x : xs

componentsParser :: Parser Node
componentsParser = do
    skipLines
    incs <- includes
    ns <- (try componentBodyParser <|> componentParser) `endBy` (eofParser <|> skipLines)
    return $ ComponentsNode ns

patternParser :: Parser Node
patternParser = (\(UnaryNode _ n pos) -> PatternNode n pos) <$> unaryParser (keyword Component) patternExpr where
    patternExpr = 
        changeExpr <$> binOp 
            idOrUnary 
            (Text.Megaparsec.Char.string "|" <|> Text.Megaparsec.Char.string "&") 
            (\a op b pos -> PatternNode (BinOpNode a op b pos) pos)

    idOrUnary = anyId <|> unaryParser (Text.Megaparsec.Char.string "!") anyId 

    changeExpr (BinOpNode a op b pos) = BinOpNode (changeExpr a) op (changeExpr b) pos
    changeExpr (PatternNode n _) = changeExpr n
    changeExpr (IdentifierNode id pos) = StringNode id pos
    changeExpr n = n

systemParser :: Parser Node
systemParser = do
    skipLines
    pos <- getSourcePos 
    incs <- try includes
    name <- spaces *> keyword System *> spaces *> identifierParser upper (lower <|> upper <|> digit <|> oneOf "_") <* spaces
    args <- maybeTuple <* spaces
    filt <- keyword When *> spaces *> (patternParser <|> (exprParser <* spaces)) <* skipLines
    ns <- programStmntParser `endBy` (eofParser <|> skipLines)
    return $ let (ms, ds) = methodsAndDefs name ns in SystemNode (extractString name) (getList args) incs filt ds ms pos 

methodsAndDefs :: Node -> [Node] -> ([Node], [Node])
methodsAndDefs name ns = partition isMethod newNs where newNs = map (referenceSelf name) ns

referenceSelf :: Node -> Node -> Node
referenceSelf name (MethodDefNode (Just id) args body pos) = MethodDefNode (Just (BinOpNode name "." id pos)) args body pos
referenceSelf _ n = n

isMethod :: Node -> Bool
isMethod (MethodDefNode (Just id) args body pos) = Prelude.True
isMethod _ = Prelude.False

stateParser :: Parser Node
stateParser = do
    skipLines
    pos <- getSourcePos
    incs <- try includes
    name <- spaces *> keyword State *> spaces *> identifierParser upper (lower <|> upper <|> digit <|> oneOf "_") <* spaces
    args <- maybeTuple <* spaces <* skipLines
    ns <- programStmntParser `endBy` (eofParser <|> skipLines)
    return $ StateNode (extractString name) (getList args) incs (filter (not . isMethod) $ refNs name ns) (filter isMethod $ refNs name ns) pos
    where
        refNs name ns = map (referenceSelf name) ns

moduleParser :: Parser Node
moduleParser = do
    skipLines
    pos <- getSourcePos
    incs <- try includes
    ident <- spaces *> keyword Module *> spaces *> eitherId <* spaces
    args <- maybeTuple <* spaces <* skipLines
    ns <- programStmntParser `endBy` (eofParser <|> skipLines)
    return $ ModuleNode (extractString ident) (getList args) incs (map (referenceSelf ident) ns) pos

fileParser :: Parser Node
fileParser = choice $ map try [entityParser, systemParser, stateParser, moduleParser, componentsParser]

runParser :: String -> String -> Either (ParseErrorBundle String Void) Node
runParser = P.runParser (fileParser <* eofParser)