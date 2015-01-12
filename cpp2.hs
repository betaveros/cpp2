{-# LANGUAGE TupleSections, FlexibleContexts, ImplicitParams #-}
-- imports {{{
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.Foldable (foldrM)
-- import Data.Function
import Data.Functor.Identity
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Monoid
import Data.Maybe
import Data.Set (Set)
-- import Debug.Trace
import System.Directory
import System.Environment
import System.IO
import System.Process
import System.Exit
import Text.Parsec
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set
-- }}}
-- character classes {{{
isIdentifier :: Char -> Bool
isIdentifier c = isDigit c || isAlpha c || c == '_'
isIdentifierStart :: Char -> Bool
isIdentifierStart c = isAlpha c || c == '_'
isOperatorSymbol :: Char -> Bool
isOperatorSymbol = (`elem` "!%&*+-/:<=>?^|~")
isHighPrecedence :: Char -> Bool
isHighPrecedence c = isIdentifier c || c `elem` "[](). "
-- }}}
-- non-parser string operations {{{
-- we'd like to not strip newlines because that might mess up the line count
lstrip :: String -> String
lstrip = dropWhile (`elem` " \t")
rstrip :: String -> String
rstrip = reverse . lstrip . reverse
dstrip :: String -> String
dstrip = rstrip . lstrip

lineify :: String -> String
lineify = dstrip . map f
    where f '\n' = ' '
          f '\r' = ' '
          f c = c

maybeWrap :: String -> String
maybeWrap s'
    | all isHighPrecedence s = s
    | otherwise = concat ["(", s, ")"]
    where s = dstrip s'
ampWrap :: String -> String
ampWrap s
    | all isAlpha s = '&' : s
    | otherwise = "&(" ++ s ++ ")"
-- }}}
-- generic parser utilities {{{
retcat :: (Monoid m) => [m] -> Parsec s u m
retcat = return . mconcat

catMany :: (Monoid m) => Parsec s u m -> Parsec s u m
catMany = fmap mconcat . many
catMany1 :: (Stream s Identity t, Monoid m) => Parsec s u m -> Parsec s u m
catMany1 = fmap mconcat . many1

anyStr1 :: (Stream s Identity Char) => Parsec s u String
anyStr1 = (:[]) <$> anyChar
-- }}}
-- parser definition, state, state manipulation {{{
type VarMap = Map String AType
data CPP2State = CPP2State {
    hygienicRegister :: Int,
    outputLineNumber :: Int,
    outputToSourceMap :: Map Int Int,
    varMap :: VarMap,
    includes :: Set String,
    hasMod :: Bool,
    mod1Value :: Maybe Integer
    } deriving Show
type Parser = Parsec String CPP2State

initState :: CPP2State
initState = CPP2State {
    hygienicRegister = 0,
    outputLineNumber = 1,
    outputToSourceMap = Map.singleton 1 1,
    varMap = Map.empty,
    includes = Set.fromList ["cstdio"],
    hasMod = False,
    mod1Value = Nothing
    }

increaseLineNumber :: Parser ()
increaseLineNumber = do
    s <- getState
    lno <- sourceLine <$> getPosition
    let o = outputLineNumber s + 1
    let m = outputToSourceMap s
    putState $ s {
        outputLineNumber = o,
        outputToSourceMap = Map.insert o lno m
        }

include :: String -> Parser ()
include inc = modifyState (\st -> st { includes = Set.insert inc (includes st) })

includeMod :: Parser ()
includeMod = modifyState (\st -> st { hasMod = True })

includeMod1 :: Parser ()
includeMod1 = do
    includeMod
    st <- getState
    case mod1Value st of
        Nothing -> setState st { mod1Value = Just 1000000007 }
        _ -> return ()

getIncludes :: Parser [String]
getIncludes = Set.toList . includes <$> getState


getHygienicVariable :: Parser String
getHygienicVariable = do
    s <- getState
    let i = hygienicRegister s
    setState $ s { hygienicRegister = i + 1 }
    return $ "_t" ++ show i
-- }}}
-- basic character-class and space parsers {{{
spaces1 :: Parser String
spaces1 = many1 ((try endOfLine <* increaseLineNumber) <|> space)

voidSpaces1 :: Parser String
voidSpaces1 = many1 space

voids :: String -> Parser ()
voids = void . string

voidc :: Char -> Parser ()
voidc = void . char

comment :: Parser String
comment = blockComment <|> lineComment
    where blockComment = do
            try $ voids "/*"
            void $ manyTill anyChar (try (string "*/"))
            return " "
          lineComment = do
            try $ voids "//"
            void $ many (satisfy (/= '\n'))
            return ""

waste :: Parser String
waste = catMany (comment <|> spaces1)

waste1 :: Parser String
waste1 = catMany1 (comment <|> spaces1)

voidw :: Parser ()
voidw = void $ many (comment <|> voidSpaces1)

keyword :: String -> Parser ()
keyword s = try $ string s >> notFollowedBy (satisfy isIdentifier)

(|=>) :: String -> b -> Parser b
a |=> b = keyword a >> return b

semic :: Parser ()
semic = void $ char ';'

idLike :: Parser String
idLike = many (satisfy isIdentifier)

identifier :: Parser String
identifier = do
    s <- satisfy isIdentifierStart
    x <- idLike
    return $ s : x

macroKeyword :: Parser String
macroKeyword = try $ do
    s <- identifier
    voidc '!'
    return s
-- }}}
-- fake type system {{{
data AType = AInt | ALong | AChar
    | AArray String AType
    | AVector AType | ADeque AType | AList AType
    | AUnknown String deriving (Eq, Show)

showType :: AType -> String
showType AInt = "int"
showType ALong = "long long"
showType AChar = "char"
showType (AVector t) = let s = showType t in
    concat ["vector<", s, if ">" `isSuffixOf` s then " >" else ">"]
showType (ADeque t) = let s = showType t in
    concat ["deque<", s, if ">" `isSuffixOf` s then " >" else ">"]
showType (AList t) = let s = showType t in
    concat ["list<", s, if ">" `isSuffixOf` s then " >" else ">"]
showType (AArray _ _) = error "showType called on AArray"
showType (AUnknown s) = s

getFmt :: AType -> Maybe String
getFmt AInt = Just "%d"
getFmt ALong = Just "%lld"
getFmt AChar = Just "%c"
getFmt _ = Nothing

putType :: String -> AType -> Parser ()
putType s t = modifyState (\st -> st { varMap = Map.insert s t (varMap st) })
getTypeMaybe :: String -> Parser (Maybe AType)
getTypeMaybe s = do
    m <- varMap <$> getState
    return $ Map.lookup s m

guessTypeMaybe :: String -> Parser (Maybe AType)
guessTypeMaybe s = do
    let (front, rest') = span isIdentifier s
    let rest = dropWhile isSpace rest'
    ft <- getTypeMaybe front
    case rest of
        "" -> return ft
        ('+':_) -> return ft
        ('-':_) -> return ft
        ('*':_) -> return ft
        ('/':_) -> return ft
        ('%':_) -> return ft
        ('[':_) -> case ft of
            Just (AVector t) -> return (Just t)
            Just (ADeque t) -> return (Just t)
            Just (AArray _ t) -> return (Just t)
            _ -> return Nothing
        _ -> return Nothing
angledType :: Parser AType
angledType = do
    voidc '<'
    voidw
    t <- aType
    voidw
    voidc '>'
    return t

knownOneParamType :: String -> (AType -> AType) -> Parser AType
knownOneParamType s tt = do
    keyword s
    voidw
    t <- angledType <|> aType
    include s
    return $ tt t

knownVectorType :: Parser AType
knownVectorType = knownOneParamType "vector" AVector
knownDequeType :: Parser AType
knownDequeType = knownOneParamType "deque" ADeque
knownListType :: Parser AType
knownListType = knownOneParamType "list" AList

knownType :: Parser AType
knownType =
    "int" |=> AInt
    <|> "ll" |=> ALong
    <|> "long" |=> ALong
    <|> "char" |=> AChar
    <|> knownVectorType
    <|> knownDequeType
    <|> knownListType

aType :: Parser AType
aType = knownType <|> fmap AUnknown identifier
-- }}}
-- numbers, identifiers {{{
billion :: Integer
billion = 10^(9::Int)

numberLit :: Parser String
numberLit = do
    c <- many1 digit
    let postBillion = do
        voidc 'b' <|> voidc 'B'
        ps <- many digit
        let pv = if null ps then 0 else read ps
        r <- idLike
        return $ show (read c * billion + pv) ++ r
    postBillion <|> do
        r <- idLike
        return $ c ++ r

checkedIdentifier :: Parser String
checkedIdentifier = do
    x <- identifier
    let ig f = include f >> return x
    case x of
        "and" -> return "&&"
        "or" -> return "||"
        "NULL" -> ig "cstdlib"
        "RAND_MAX" -> ig "cstdlib"
        "abort" -> ig "cstdlib"
        "abs" -> ig "cstdlib"
        "acos" -> ig "cmath"
        "asin" -> ig "cmath"
        "assert" -> ig "cassert"
        "atan" -> ig "cmath"
        "atan2" -> ig "cmath"
        "atoi" -> ig "cstdlib"
        "cos" -> ig "cmath"
        "exp" -> ig "cmath"
        "fill" -> ig "algorithm"
        "greater" -> ig "functional"
        "isalnum" -> ig "cctype"
        "isalpha" -> ig "cctype"
        "isdigit" -> ig "cctype"
        "islower" -> ig "cctype"
        "isprint" -> ig "cctype"
        "ispunct" -> ig "cctype"
        "isspace" -> ig "cctype"
        "isupper" -> ig "cctype"
        "list" -> ig "list"
        "make_pair" -> ig "utility"
        "map" -> ig "map"
        "max" -> ig "algorithm"
        "memset" -> ig "cstring"
        "min" -> ig "algorithm"
        "nth_element" -> ig "algorithm"
        "pair" -> ig "utility"
        "pow" -> ig "cmath"
        "priority_queue" -> ig "queue"
        "qsort" -> ig "cstdlib"
        "queue" -> ig "queue"
        "rand" -> ig "cstdlib"
        "set" -> ig "set"
        "sin" -> ig "cmath"
        "sort" -> ig "algorithm"
        "sqrt" -> ig "cmath"
        "srand" -> ig "cstdlib"
        "strcmp" -> ig "cstring"
        "strlen" -> ig "cstring"
        "swap" -> ig "algorithm"
        "tan" -> ig "cmath"
        "tolower" -> ig "cctype"
        "toupper" -> ig "cctype"
        "unique" -> ig "algorithm"
        "vector" -> ig "vector"
        _ -> return x
-- }}}
-- chunk units and chunks, whatever that means {{{
-- parse as little as possible of an expression or whatever while making sense
-- useful if you want to paranoidly parse manyTill something
chunkUnit :: Parser String
chunkUnit = parenBlock <|> bracketBlock <|> someLit <|> waste1 <|> numberLit <|> checkedIdentifier <|> string "." <|> many1 (satisfy isOperatorSymbol)

commaChunkUnit :: Parser String
commaChunkUnit = chunkUnit <|> string ","
semicChunkUnit :: Parser String
semicChunkUnit = commaChunkUnit <|> string ";"

-- sugar chunker {{{
data LPSymbol = Min | Max | Minify | Maxify
    | Mod | ModPlus | ModMinus | ModTimes
    | ModPlusEq | ModMinusEq | ModTimesEq
    | PushBack | PushFront | PopBack | PopFront
    | Orig String

readSugar :: String -> Either String LPSymbol
readSugar "<?"  = Right Min
readSugar ">?"  = Right Max
readSugar "<?=" = Right Minify
readSugar ">?=" = Right Maxify
readSugar "mod" = Right Mod
readSugar "%%"  = Right Mod
readSugar "+%"  = Right ModPlus
readSugar "-%"  = Right ModMinus
readSugar "*%"  = Right ModTimes
readSugar "+%=" = Right ModPlusEq
readSugar "-%=" = Right ModMinusEq
readSugar "*%=" = Right ModTimesEq
readSugar "+>"  = Right PushBack
readSugar "+<"  = Right PushFront
readSugar "%>"  = Right PopBack
readSugar "%<"  = Right PopFront
readSugar "<="  = Left "<="
readSugar ">="  = Left ">="
readSugar "!="  = Left "!="
readSugar "=="  = Left "=="
readSugar x
    | last x == '=' = Right (Orig x)
    | otherwise = Left x

readSugarList :: [String] -> ([(String, LPSymbol)], String)
readSugarList [] = ([], "")
readSugarList (x:xs) = case (readSugar x, readSugarList xs) of
    (Left s,  ([], sf)) -> ([], s ++ sf)
    (Left s,  ((s', y') : r, sf)) -> ((s ++ s', y') : r, sf)
    (Right y, (r, sf)) -> (("", y) : r, sf)

combineAroundSymbol :: (String, LPSymbol) -> String -> Parser String
combineAroundSymbol (s1,sym) s2 = case sym of
        -- to get linebreaks right, exactly one appearance of s1 and s2 must not be lineified
        Min        -> iac ["min(", ds s1, ", ", ds s2, ")"]
        Max        -> iac ["max(", ds s1, ", ", ds s2, ")"]
        Minify     -> iac [s1, "= min(", li s1, ", ", ds s2, ")"]
        Maxify     -> iac [s1, "= max(", li s1, ", ", ds s2, ")"]
        PushBack   -> iac [rs s1, ".push_back(" , ds s2, ")"]
        PushFront  -> iac [rs s1, ".push_front(", ds s2, ")"]
        PopBack    -> iac [s1, "= ", w s2, ".back(); " , wli s2, ".pop_back()"]
        PopFront   -> iac [s1, "= ", w s2, ".front(); ", wli s2, ".pop_front()"]
        Mod        -> includeMod  >> rc ["_mod(", ds s1, ", ", ds s2, ")"]
        ModPlus    -> includeMod1 >> rc ["_mod1(", w s1, " + ", w s2, ")"]
        ModMinus   -> includeMod1 >> rc ["_mod1(", w s1, " - ", w s2, ")"]
        ModTimes   -> includeMod1 >> rc ["_mtimes(", ds s1, ", ", ds s2, ")"]
        ModPlusEq  -> includeMod1 >> rc [s1, "= _mod1(", wli s1, " + ", w s2, ")"]
        ModMinusEq -> includeMod1 >> rc [s1, "= _mod1(", wli s1, " - ", w s2, ")"]
        ModTimesEq -> includeMod1 >> rc [s1, "= _mtimes(", li s1, ", ", ds s2, ")"]
        Orig s     -> rc [s1, s, s2]
    where rc = retcat
          iac = (include "algorithm" >>) . rc
          rs = rstrip
          ds = dstrip
          li = lineify
          w = maybeWrap
          wli = maybeWrap . lineify

sugarChunk :: [String] -> Parser String
sugarChunk c = let (sl, sf) = readSugarList c
    in foldrM combineAroundSymbol sf sl
-- }}}

chunk :: Parser String
chunk = many chunkUnit >>= sugarChunk

chunk1 :: Parser String
chunk1 = many1 chunkUnit >>= sugarChunk

commaChunk :: Parser String
commaChunk = catMany commaChunkUnit

semicChunk :: Parser String
semicChunk = catMany semicChunkUnit

chunkList :: Parser [String]
chunkList = chunk `sepBy` string ","
-- }}}
-- scanf, printf {{{
scanfFor :: Bool -> [(AType, String)] -> String
scanfFor skipFlag xs = concat [
        "scanf(\"",
        concat [fromJust (getFmt t) | (t, _) <- xs],
        if skipFlag then " \"" else "\"",
        concat [", " ++ ampWrap v | (_, v) <- xs],
        ");"]

postScanCommand :: Bool -> Parser String
postScanCommand skipFlag = do
    mt <- optionMaybe knownType
    case mt of
        Just t -> do -- this is an initializer list
            voidw
            vs <- sepBy1 (do
                voidw
                v <- checkedIdentifier
                voidw
                return v) (string ",")
            semic
            forM_ vs $ flip putType t
            retcat [showType t, " ", intercalate ", " vs, "; ", scanfFor skipFlag (map (t,) vs)]
        Nothing -> do
            vs <- map (dropWhile isSpace) <$> chunkList
            voidw
            semic
            tvs <- forM vs $ \v -> do
                tmt <- guessTypeMaybe v
                case tmt of
                    Just t -> return (t, v)
                    Nothing -> parserFail $ "cannot guess type of " ++ show v
            return . scanfFor skipFlag $ tvs

data Mfmt = Mfmt [String] String
instance Monoid Mfmt where
    mempty = Mfmt [] ""
    (Mfmt v s) `mappend` (Mfmt v' s') = Mfmt (v ++ v') (s ++ s')

guessFormat :: String -> Parser String
guessFormat c = do
    m <- guessTypeMaybe c
    case m of
        Nothing -> parserFail $ "macro format: cannot infer type of " ++ c
        Just t -> case getFmt t of
            Just f -> return f
            Nothing -> parserFail $ "macro format: no format specifier for inferred type " ++ show t

macroFormatString :: Parser Mfmt
macroFormatString = do
    voidc '"'
    let s |>> t = try $ string s >> return (Mfmt [] t)
    mres <- (mconcat <$>) . many $ "\\\\" |>> "\\\\"
        <|> "\\\"" |>> "\\\""
        <|> "{{"   |>> "{"
        <|> "}}"   |>> "}"
        <|> "%"    |>> "%%"
        <|> do
            void $ char '{'
            givenFmt <- optionMaybe . try $ do
                x <- identifier
                voidc ':'
                return x
            c <- chunk
            void $ char '}'
            fmt <- case givenFmt of
                Just f -> return $ "%" ++ f
                Nothing -> guessFormat c
            return $ Mfmt [c] fmt
        <|> (do x <- satisfy (`notElem` "\n\r\""); return $ Mfmt [] [x])
    voidc '"'
    return mres

macroFormatChunk :: Parser Mfmt
macroFormatChunk = do
    c <- chunk
    fmt <- guessFormat c
    return $ Mfmt [c] fmt
macroFormatDebugChunk :: Parser Mfmt
macroFormatDebugChunk = do
    c <- chunk
    fmt <- guessFormat c
    return $ Mfmt [c] (concat ["(", c, " = ", fmt, ")"])

compileMfmt :: Mfmt -> String
compileMfmt (Mfmt v s) =
    concat ["\"", s, "\"", concatMap (", " ++) v]
compileMfmtStdout :: Mfmt -> String
compileMfmtStdout m =
    concat ["printf(", compileMfmt m, ");"]
compileMfmtStderr :: Mfmt -> String
compileMfmtStderr m =
    concat ["fprintf(stderr, ", compileMfmt m, ");"]

postPrintCommandAdding :: (Mfmt -> Mfmt) -> Parser String
postPrintCommandAdding f = do
    m <- mconcat <$> ((macroFormatString <|> macroFormatChunk) `sepBy` (string "," >> waste))
    voidw
    semic
    return . compileMfmtStdout $ f m
postPrintCommand :: Parser String
postPrintCommand = postPrintCommandAdding id
postPrintlnCommand :: Parser String
postPrintlnCommand = postPrintCommandAdding (<> Mfmt [] "\\n")

postDebugPrintCommand :: Parser String
postDebugPrintCommand = do
    lno <- sourceLine <$> getPosition
    let lnodis = printf "L%3d:" lno
    voidw
    m <- mconcat <$> ((macroFormatString <|> macroFormatDebugChunk) `sepBy` (string "," >> waste))
    voidw
    semic
    return $ compileMfmtStderr (Mfmt [] lnodis <> m <> Mfmt [] "\\n")
postGetsCommand :: Parser String
postGetsCommand = do
    voidw
    c <- chunk
    t <- guessTypeMaybe c
    semic
    case t of
        Just (AArray dim AChar) -> retcat ["fgets(", c, ", ", dim, ", stdin);"]
        Just _ -> parserFail $ "gets!: guessed type of " ++ show c ++ " not char[*]"
        Nothing -> parserFail $ "gets!: cannot infer type of " ++ show c
-- }}}
-- macroCommand {{{
macroCommand :: Parser String
macroCommand = do
    kw <- macroKeyword
    voidw
    case kw of
        "scan" -> postScanCommand False
        "s" -> postScanCommand False
        "ss" -> postScanCommand True
        "sort" -> do
            voidw
            s <- chunk
            semic
            include "algorithm"
            retcat ["sort(", s, ".begin(), ", s, ".end());"]
        "p" -> postPrintCommand
        "print" -> postPrintCommand
        "pl" -> postPrintlnCommand
        "pn" -> postPrintlnCommand
        "pln" -> postPrintlnCommand
        "println" -> postPrintlnCommand
        "d" -> postDebugPrintCommand
        "r" -> postRepeatCommand
        "rep" -> postRepeatCommand
        "gets" -> postGetsCommand
        "repeat" -> postRepeatCommand
        "gcases" -> postRepeatCommand
        _ -> parserFail $ "unrecognized macro: " ++ kw
-- }}}
-- typeCommand {{{
arrayBlock :: Parser (String, String)
arrayBlock = do
    w <- waste
    voidc '['
    s <- identifier <|> numberLit
    voidc ']'
    return (concat [w, "[", s, "]"], s)

typeCommand :: Parser String
typeCommand = try $ do
    t <- knownType
    ls <- (do
        w1 <- waste
        v <- checkedIdentifier
        abks <- many $ try arrayBlock
        c <- chunk
        return ((v, map snd abks),
            concat [w1, v, concatMap fst abks, c])) `sepBy` string ","
    forM_ ls $ \((v,abks),_) -> putType v (foldr AArray t abks)
    return $ showType t ++ intercalate "," (map snd ls)
-- }}}
-- brace paren bracket block {{{
braceBlock :: Parser String
braceBlock = do
    voidc '{'
    s <- many topLevelUnit
    voidc '}'
    retcat $ ["{"] ++ s ++ ["}"]

parenBlock :: Parser String
parenBlock = do
    voidc '('
    s <- many topLevelUnit
    voidc ')'
    retcat $ ["("] ++ s ++ [")"]

bracketBlock :: Parser String
bracketBlock = do
    voidc '['
    s <- commaChunk
    voidc ']'
    retcat ["[", s, "]"]
-- }}}
-- literal {{{
backslashEscape :: Parser String
backslashEscape = do
    voidc '\\'
    c <- anyChar
    return ['\\', c]

stringLit :: Parser String
stringLit = do
    voidc '"'
    s <- manyTill (backslashEscape <|> anyStr1) (voidc '"')
    retcat $ ["\""] ++ s ++ ["\""]

charLit :: Parser String
charLit = do
    voidc '\''
    s <- backslashEscape <|> anyStr1
    voidc '\''
    retcat ["'", s, "'"]

someLit :: Parser String
someLit = stringLit <|> charLit
-- }}}
-- for {{{
data ForStruct = ForStruct {
    varType :: AType,
    varInit :: String,
    varEnd :: Maybe (Bool, String),
        -- Nothing = no condition;
        -- otherwise (inclusive, end)
    varDown :: Bool,
    varStep :: Maybe String -- Nothing = ++ or --; Just s = += or -= s
}

defaultForStruct :: ForStruct
defaultForStruct = ForStruct {
    varType = AInt,
    varInit = "0",
    varEnd = Nothing,
    varDown = False,
    varStep = Nothing
}

forStructCondition :: String -> ForStruct -> String
forStructCondition v fs = case varEnd fs of
    Nothing -> ""
    Just (b, end) -> concat [
        " ", v, " ",
        if varDown fs then ">" else "<",
        if b then "=" else "",
        " ", end]

compileForStruct :: String -> ForStruct -> String
compileForStruct name fs = concat $ [
        showType (varType fs), " ", name, " = ", varInit fs, ";",
        forStructCondition name fs,
        "; "] ++
        let schar = if varDown fs then '-' else '+' in
            case varStep fs of
                Nothing -> [schar : schar : name]
                Just s -> [name, " ", [schar], "= ", show s]

data ForKeyword = FFrom | FTo | FTil | FBy | FDown deriving (Eq, Show)

forKeyword :: Parser ForKeyword
forKeyword = "from" |=> FFrom
    <|> "to" |=> FTo
    <|> "til" |=> FTil
    <|> "by" |=> FBy
    <|> "down" |=> FDown

lookForBlocker :: Parser ()
lookForBlocker = lookAhead (void forKeyword <|> void (oneOf "{})]"))

forArg :: Parser String
forArg = voidw >> (rstrip . concat <$> manyTill commaChunkUnit lookForBlocker)

forAfterFrom :: Parser ForStruct
forAfterFrom = do
    arg <- forArg
    fs <- forToTilLike
    return $ fs { varInit = arg }

downed :: Parser ForStruct -> Parser ForStruct
downed = fmap (\fs -> fs { varDown = True })

forToTilLike :: Parser ForStruct
forToTilLike =
    (keyword "to" >> forAfterTo)
    <|> (keyword "til" >> forAfterTil)
    <|> (keyword "by" >> forAfterBy)
    <|> (keyword "down" >> voidw >> downed (
        (keyword "to" >> downed forAfterTo)
        <|> (keyword "til" >> downed forAfterTil)
        <|> (keyword "by" >> downed forAfterBy)))
    <|> return defaultForStruct

forAfterTo :: Parser ForStruct
forAfterTo = do
    a <- forArg
    fs <- forByLike
    return $ fs { varInit = "1", varEnd = Just (True, a) }

forAfterTil :: Parser ForStruct
forAfterTil = do
    a <- forArg
    fs <- forByLike
    return $ fs { varInit = "0", varEnd = Just (False, a) }

forByLike :: Parser ForStruct
forByLike = (keyword "by" >> forAfterBy)
    <|> (keyword "down" >> voidw >> keyword "by" >> downed forAfterBy)
    <|> return defaultForStruct


forAfterBy :: Parser ForStruct
forAfterBy = do
    x' <- forArg
    return $ defaultForStruct { varStep = Just x' }

forStructContent :: Parser String
forStructContent = do
    (v, kw1) <- try $ do
        v <- identifier
        voidw
        k <- forKeyword
        return (v, k)
    fs <- case kw1 of
        FFrom -> forAfterFrom
        FTo -> forAfterTo
        FTil -> forAfterTil
        FBy -> forAfterBy
        FDown -> do
            kw2 <- forKeyword
            downed $ case kw2 of
                FFrom -> forAfterFrom
                FTo -> forAfterTo
                FTil -> forAfterTil
                FBy -> forAfterBy
                _ -> parserFail $ "unexpected keyword after down: " ++ show kw2
    putType v (varType fs)
    return $ compileForStruct v fs

forContent :: Parser String
forContent = forStructContent <|> topLevel

forContentTilBrace :: Parser String
forContentTilBrace = forStructContent <|> semicChunk

forParenContent :: Parser String
forParenContent = do
    voidc '('
    voidw
    fc <- forContent
    voidw
    voidc ')'
    return fc

forStructure :: Parser String
forStructure = do
    keyword "for"
    voidw
    fc <- forParenContent <|> forContentTilBrace
    retcat ["for (", fc, ")"]
-- }}}
-- if, else, while, switch, repeat {{{
parenContent :: Parser String
parenContent = do
    voidc '('
    voidw
    c <- topLevel
    voidw
    voidc ')'
    return c

ifStructure :: Parser String
ifStructure = do
    keyword "if"
    voidw
    c <- rstrip <$> (parenContent <|> commaChunk)
    retcat ["if (", c, ")"]

elseStructure :: Parser String
elseStructure = do
    keyword "else"
    s <- waste
    t <- option "" ifStructure
    retcat ["else", s, t]

whileStructure :: Parser String
whileStructure = do
    keyword "while"
    voidw
    c <- rstrip <$> (parenContent <|> commaChunk)
    retcat ["while (", c, ")"]

switchStructure :: Parser String
switchStructure = do
    keyword "switch"
    voidw
    c <- rstrip <$> (parenContent <|> commaChunk)
    retcat ["switch (", c, ")"]

postRepeatCommand :: Parser String
postRepeatCommand = do
    c <- rstrip <$> (parenContent <|> commaChunk)
    v <- getHygienicVariable
    retcat ["for (int ", v, " = ", c, "; ", v, " > 0; --", v, ")"]
-- }}}
-- More or less something outer-level command-like {{{
topLevelUnit :: Parser String
topLevelUnit =
    parenBlock <|> bracketBlock <|> braceBlock
    <|> someLit
    <|> waste1
    <|> macroCommand
    <|> typeCommand
    <|> ifStructure
    <|> elseStructure
    <|> forStructure
    <|> whileStructure
    <|> switchStructure
    <|> chunk1
    <|> semicChunkUnit

topLevel :: Parser String
topLevel = catMany1 topLevelUnit

topParser :: Parser (String, CPP2State, Int -> Int)
topParser = do
    s <- topLevel
    eof
    st <- getState
    incs <- getIncludes
    let otsm = outputToSourceMap st
    let headerLines = "// @betaveros :: generated with cpp2.hs" : ["#include <" ++ x ++ ">" | x <- incs] ++ ["using namespace std;"]
                        ++ ["long long _mod(long long x, long long m) { long long r = x % m; return r < 0 ? r + m : r; }" | hasMod st]
                        ++ (case mod1Value st of
                            Nothing -> []
                            Just x -> [
                                "long long _mod1(long long a) { return _mod(a, " ++ show x ++ "); }",
                                "long long _mtimes(long long a, long long b) { return _mod1(a * b); }"])
    return (unlines headerLines ++ s,
        st,
        snd . fromJust . flip Map.lookupLE otsm . max 1 . subtract (length headerLines))
-- }}}
-- main {{{
cppCompileParser :: String -> String -> (Int -> Int) -> Parsec String () String
cppCompileParser orig new f = catMany (try p <|> (:[]) <$> anyChar)
    where p = do
                void $ string orig
                void $ string ":"
                ns <- many1 digit
                retcat [new, ":", show $ f (read ns),
                    "(", orig, ":", ns, ")"]

openInputFile :: String -> String -> IO Handle
openInputFile fstem islug =
    openFile (concat [fstem, "-", islug, ".in"]) ReadMode


printColoredLine :: (?colorful :: Bool) => String -> Char -> String -> IO ()
printColoredLine cesc pchar s = do
    let plinestart = (pchar : pchar : ' ' : s) ++ " "
    let pline = plinestart ++ replicate (60 - length plinestart) pchar
    hPutStrLn stderr $ if ?colorful
        then concat [cesc, pline, [chr 27], "[0m"]
        else pline

printPurpleLine :: (?colorful :: Bool) => Char -> String -> IO ()
printPurpleLine = printColoredLine $ chr 27 : "[35m"

printCyanLine :: (?colorful :: Bool) => Char -> String -> IO ()
printCyanLine = printColoredLine $ chr 27 : "[36m"

parseItAll :: (?colorful :: Bool) => String -> IO (String, CPP2State, Int -> Int)
parseItAll fname = do
    cont <- readFile fname
    case runParser topParser initState fname cont of
        Left err -> do
            let ep = errorPos err
            hPutStr stderr $ sourceName ep
            hPutChar stderr ':'
            hPutStr stderr . show $ sourceLine ep
            hPutChar stderr ':'
            hPutStr stderr . show $ sourceColumn ep
            hPutStr stderr " | "
            hPutStrLn stderr . intercalate " / " . tail . lines . show $ err
            exitWith (ExitFailure 1)
        Right x -> return x

msgOf :: String -> String -> String
msgOf fname msg = concat ["cpp2 @ ", fname, " : ", msg]

processCPP2 :: (?colorful :: Bool, ?isDry :: Bool) => String -> String -> IO ()
processCPP2 fname fstem = do
    let fcname = fstem ++ ".cpp"
    (s, _, f) <- parseItAll fname
    writeFile fcname s
    printCyanLine ':' $ msgOf fname "preprocess OK"
    (_, _, Just herr, ph) <- createProcess (proc "g++" (
        [fcname, "-O2",
            "-Wall", "-Wextra",
            "-Wconversion", "-Wpointer-arith",
            "-Wshadow"]
            ++ ["-fcolor-diagnostics" | ?colorful]
            ++ if ?isDry then ["-fsyntax-only"] else ["-o", fstem])
            ){ std_err = CreatePipe }
    code <- waitForProcess ph
    errs <- hGetContents herr
    case runParser (cppCompileParser fcname fname f) () "g++" errs of
        Left emsg -> hPrint stderr emsg
        Right es -> hPutStr stderr es
    case code of
        ExitSuccess -> return ()
        _ -> exitWith code

fileExistsNewer :: String -> String -> IO Bool
fileExistsNewer f1 f2 = do
    e1 <- doesFileExist f1
    if e1
        then liftM2 (>=) (getModificationTime f1) (getModificationTime f2)
        else return False

processIfNecessary :: (?colorful :: Bool, ?isDry :: Bool) => String -> String -> IO ()
processIfNecessary fname fstem = do
    xnewer <- fileExistsNewer fstem fname
    if xnewer
        then printCyanLine ':' $ msgOf fname "already processed"
        else processCPP2 fname fstem

mainArg :: String -> [String] -> [String] -> IO ()
mainArg arg fargs flags = do
    let (fstem, frest) = span (/= '.') arg
    let ?colorful = "--no-color" `notElem` flags
    let fname = arg ++ if null frest then ".cpp2" else ""
    if "--dump" `elem` flags then do
            (s, cst, _) <- parseItAll fname
            putStr s
            when ("--state" `elem` flags) $ hPrint stderr cst
        else do
            let ?isDry = "--dry" `elem` flags
            if "--force" `elem` flags
                then processCPP2 fname fstem
                else processIfNecessary fname fstem
            unless ?isDry $ do
                let p = proc ("./" ++ fstem) []
                p' <- case fargs of
                    [] -> do
                        printPurpleLine '=' fstem
                        return p
                    (x:_) -> do
                        printPurpleLine '/' $ concat [fstem, " <-", x, ".in"]
                        h <- openInputFile fstem x
                        return $ p { std_in = UseHandle h }
                (_, _, _, rph) <- createProcess p'
                rcode <- waitForProcess rph
                exitWith rcode

isFlag :: String -> Bool
isFlag ('-':_) = True
isFlag _ = False
-- }}}
main :: IO ()
main = do
    args <- getArgs
    case partition isFlag args of
        (["--version"],_) -> putStrLn "C++ Competitive Programming Preprocessor ver. 0.0alpha"
        (_, []) -> hPutStrLn stderr "cpp2: no input files"
        (flags, arg:fargs) -> mainArg arg fargs flags
-- vim:set fdm=marker:
