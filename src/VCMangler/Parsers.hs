module VCMangler.Parsers
( 
    mangledParser,
    typeParser,
    qualifiedNameParser,
    MangledState(..)
) where 

import Text.Parsec
import Data.Char (digitToInt)
import Numeric (readHex)
import Debug.Trace (trace)

import VCMangler.Types
import VCMangler.Maps

{----------------------------------------------------------------------------------------------------
    Parser Combinators
----------------------------------------------------------------------------------------------------}

mangledParser :: Parsec String MangledState UnmangledName
mangledParser = do
    char '?'
    n <- qualifiedNameParser
    char '@'
    m <- functionModifiersParser
    option [] storageReturnParser
    r <- case getFunctionName n of
        (OpName OpConstructor) -> char '@' >> return (getFirstQualifier n)
        (OpName OpDestructor) -> char '@' >> return Void_
        _ -> subTypeParser
    ts <- typeArgumentParser
    return (Function n m r ts)

{----------------------------------------------------------------------------------------------------
    Name Parsers
----------------------------------------------------------------------------------------------------}

qualifiedNameParser :: Parsec String MangledState QualifiedName
qualifiedNameParser = do
    n <- lookupNameParser <|> do 
        p <- identifierNameParser <|> (char '?' >> (operatorNameParser <|> templateNameParser))
        case p of 
            OpName _ -> modifyState id
            _        -> modifyState (pushName p)
        return p
    f <- qualifierParser
    return (f n)

qualifierParser :: Parsec String MangledState (QualifiedName -> QualifiedName)
qualifierParser = option id $ do
    q <- lookupNameParser <|> try functionNameParser <|> do
        p <- identifierNameParser <|> (char '?' >> (anonymousNamespaceNameParser <|> templateNameParser))
        modifyState (pushName p)
        return p
    r <- qualifierParser
    return (r . liftNameToQualifier q)

identifierNameParser :: Parsec String MangledState QualifiedName
identifierNameParser = do
    c <- oneOf ("_$" ++ ['A'..'Z'] ++ ['a'..'z'])
    cs <- (many . oneOf) ("_$" ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'])
    char '@'
    return (Name (c:cs))

operatorNameParser :: Parsec String MangledState QualifiedName
operatorNameParser = do
    op <- fmap (:[]) (oneOf operatorCodeRange) <|> fmap (\c -> '_':[c]) (char '_' >> oneOf operatorUCodeRange)
    return (OpName (codeToOperator op))

templateNameParser :: Parsec String MangledState QualifiedName
templateNameParser = do
    char '$'
    (MangledState names types) <- getState
    putState (MangledState [] types)
    (Name i) <- identifierNameParser
    modifyState (pushName (Name i))
    ts <- many1 templateArgumentParser
    char '@'
    putState (MangledState names types)
    let r = TName i ts
    modifyState (pushName r)
    return r

anonymousNamespaceNameParser :: Parsec String MangledState QualifiedName
anonymousNamespaceNameParser = do
    string "A0x"
    h <- count 8 (oneOf (['0'..'9'] ++ ['a'..'f']))
    char '@'
    return (Name ("0x" ++ h))

functionNameParser :: Parsec String MangledState QualifiedName
functionNameParser = do
    char '?'
    q1 <- fmap (\c -> show (digitToInt c + 1)) (oneOf ['0'..'9']) -- Not exactly sure what the numbers mean yet, undname increases by one?
    char '?'
    (FName q2) <- lookupNameParser <|> do
        f <- fmap FName mangledParser
        modifyState (pushName f)
        return f
    return (FQualifier q2 (Name q1)) 

lookupNameParser :: Parsec String MangledState QualifiedName
lookupNameParser = do
    (MangledState ns _) <- getState
    c <- oneOf (take (length ns) ['0'..'9'])
    return (ns !! digitToInt c)

{----------------------------------------------------------------------------------------------------
    Type Parsers
----------------------------------------------------------------------------------------------------}

typeArgumentParser :: Parsec String MangledState [Type]
typeArgumentParser = (string "XZ" >> return [Void_]) <|> do
    tss <- manyTill typeParser (lookAhead (oneOf "Z@"))
    a <- (char 'Z' >> return [VarArgs_]) <|> (char '@' >> return [])
    char 'Z'
    return (tss ++ a)

typeParser :: Parsec String MangledState Type
typeParser = lookupTypeParser <|> do 
    st <- subTypeParser
    modifyState (pushType st)
    return st

subTypeParser :: Parsec String MangledState Type
subTypeParser = primitiveTypeParser <|> compositeTypeParser <|> pointerTypeParser <|> referenceTypeParser

lookupTypeParser :: Parsec String MangledState Type
lookupTypeParser = do
    (MangledState _ ts) <- getState
    c <- oneOf (take (length ts) ['0'..'9'])
    return (ts !! digitToInt c)

primitiveTypeParser :: Parsec String MangledState Type
primitiveTypeParser = do
    code <- fmap (:[]) (oneOf typeCodeRange) <|> fmap (\c -> '_':[c]) (char '_' >> oneOf typeUCodeRange)
    return (codeToType code)

compositeTypeParser :: Parsec String MangledState Type
compositeTypeParser = do
    t <- fmap (:[]) (oneOf compositeCodeRange) <|> fmap (\c -> 'W':[c]) (char 'W' >> char '4')
    q <- qualifiedNameParser
    char '@'
    return (Composite (codeToComposite t) q)

-- TODO: check for 'E' or 'M' before/after 'I' for 64bit base
pointerTypeParser :: Parsec String MangledState Type
pointerTypeParser = try functionPointerTypeParser <|> try memberFunctionPointerTypeParser <|> do
    pm1 <- fmap codeToPointerModifier (oneOf pointerModifierCodeRange)
    pm2 <- fmap (pm1 ++) (option [] (char 'I' >> return [Restrict]))
    tm <- fmap codeToModifiers (oneOf modifiersCodeRange)
    t <- subTypeParser
    return (Pointer tm pm2 t)

functionPointerTypeParser :: Parsec String MangledState Type
functionPointerTypeParser = do
    string "P6"
    cc <- callingConventionParser
    r <- subTypeParser
    ts <- typeArgumentParser
    return (FunctionPointer cc r ts)

memberFunctionPointerTypeParser :: Parsec String MangledState Type
memberFunctionPointerTypeParser = do
    string "P8"
    q <- qualifiedNameParser
    char '@'
    oneOf modifiersCodeRange
    c <- fmap codeToCallingConvention (oneOf callingConventionCodeRange)
    r <- subTypeParser
    as <- typeArgumentParser
    return (FunctionPointer c r as)

referenceTypeParser :: Parsec String MangledState Type
referenceTypeParser = do
    c <- (char 'A' >> return Reference) <|> (string "$$Q" >> return RValueReference)
    rm <- option [] (char 'I' >> return [Restrict])
    tm <- fmap codeToModifiers (oneOf modifiersCodeRange)
    t <- subTypeParser
    return (c tm rm t)

{----------------------------------------------------------------------------------------------------
    Template Argument Parsers
----------------------------------------------------------------------------------------------------}

templateArgumentParser :: Parsec String MangledState TemplateArgument
templateArgumentParser = (char '$' >> (templateQualifiedTypeParser <|> templateValueParser)) <|> fmap TType typeParser

templateQualifiedTypeParser :: Parsec String MangledState TemplateArgument
templateQualifiedTypeParser = do
    string "$C"
    m <- fmap codeToModifiers (oneOf ['B'..'D'])
    t <- typeParser
    return (TType (Qualified m t))

templateValueParser :: Parsec String MangledState TemplateArgument
templateValueParser = do
    char '0'
    x <- smallTemplateValueParser <|> largeTemplateValueParser <|> negativeTemplateValue
    return (TValue x)

smallTemplateValueParser :: Parsec String MangledState Int
smallTemplateValueParser = do
    c <- oneOf ['0'..'9']
    return (digitToInt c + 1)

largeTemplateValueParser :: Parsec String MangledState Int
largeTemplateValueParser = do
    cs <- many1 (oneOf ['A'..'P'])
    char '@'
    return $ (fst . head . readHex . map encodingToHex) cs

negativeTemplateValue :: Parsec String MangledState Int
negativeTemplateValue = do
    char '?'
    x <- smallTemplateValueParser <|> largeTemplateValueParser
    return (-x)

{----------------------------------------------------------------------------------------------------
    Miscellaneous Parsers
----------------------------------------------------------------------------------------------------}

functionModifiersParser :: Parsec String MangledState FunctionModifiers
functionModifiersParser = do
    m <- globalFunctionModifiersParser <|> memberFunctionModifiersParser
    c <- callingConventionParser
    return (m c)

globalFunctionModifiersParser :: Parsec String MangledState (Convention -> FunctionModifiers)
globalFunctionModifiersParser = do
    c <- oneOf globalFunctionModifiersCodeRange
    return (Global (codeToGlobalFunctionModifiers c))

memberFunctionModifiersParser :: Parsec String MangledState (Convention -> FunctionModifiers)
memberFunctionModifiersParser = do
    c <- oneOf memberFunctionModifiersCodeRange
    let (m, d, a) = codeToMemberFunctionModifiers c
    case m of
        Static -> return ' '
        _ -> oneOf modifiersCodeRange
    return (\x -> Member d x m a)

callingConventionParser :: Parsec String MangledState Convention
callingConventionParser = do
    c <- oneOf callingConventionCodeRange
    return (codeToCallingConvention c)

storageReturnParser :: Parsec String MangledState [Modifier]
storageReturnParser = do
    char '?'
    op <- oneOf modifiersCodeRange
    return (codeToModifiers op)

{----------------------------------------------------------------------------------------------------
    Various Helper Functions
----------------------------------------------------------------------------------------------------}

-- Only types larger than 1 character can be abbreviated
shouldPush :: Type -> Bool
shouldPush Composite{}       = True
shouldPush Pointer{}         = True
shouldPush Reference{}       = True
shouldPush FunctionPointer{} = True
shouldPush Bool_             = True
shouldPush LongLong_         = True
shouldPush ULongLong_        = True
shouldPush WCharT_           = True
shouldPush _                 = False

getFunctionName :: QualifiedName -> QualifiedName
getFunctionName (Qualifier _ q) = getFunctionName q
getFunctionName (TQualifier _ _ q) = getFunctionName q
getFunctionName x = x

-- This function assumes that the first qualifier is either a class or a template class
getFirstQualifier :: QualifiedName -> Type
getFirstQualifier (Qualifier _ (Qualifier s q)) = getFirstQualifier (Qualifier s q)
getFirstQualifier (Qualifier _ (TQualifier s ts q)) = getFirstQualifier (TQualifier s ts q)
getFirstQualifier (TQualifier _ _ (Qualifier s q)) = getFirstQualifier (Qualifier s q)
getFirstQualifier (TQualifier _ _ (TQualifier s ts q)) = getFirstQualifier (TQualifier s ts q)
getFirstQualifier (Qualifier s _) = Composite Class (Name s)
getFirstQualifier (TQualifier s ts _) = Composite Class (TName s ts)

liftNameToQualifier :: QualifiedName -> (QualifiedName -> QualifiedName)
liftNameToQualifier (Name s) = Qualifier s
liftNameToQualifier (TName s ts) = TQualifier s ts
liftNameToQualifier (FName n) = FQualifier n
liftNameToQualifier (Qualifier s q) = Qualifier s . liftNameToQualifier q
liftNameToQualifier (TQualifier s ts q) = TQualifier s ts . liftNameToQualifier q
liftNameToQualifier (FQualifier f q) = FQualifier f . liftNameToQualifier q 

-- Use state to keep track of lookback identifiers and types
data MangledState = MangledState [QualifiedName] [Type]

pushName n (MangledState ns ts) = if length ns < 10
                                  then MangledState (ns ++ [n]) ts
                                  else MangledState ns ts

pushType t (MangledState ns ts) = if length ts < 10 && shouldPush t
                                  then MangledState ns (ts ++ [t])
                                  else MangledState ns ts