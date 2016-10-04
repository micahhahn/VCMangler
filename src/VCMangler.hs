module VCMangler 
( 
    parseMangledName,
    UnmangledName(..),
    QualifiedName(..),
    Convention(..),
    Distance(..),
    Access(..),
    MethodType(..),
    FunctionModifiers(..),
    Type(..),
    Modifier(..),
    CompositeType(..),
    TemplateArgument(..),
    Operator(..)
) where

import Text.Parsec

import VCMangler.Types
import VCMangler.Parsers

parseMangledName :: String -> Either String UnmangledName
parseMangledName s = case runParser mangledParser (MangledState [] []) "(source)" s of
                        (Left error) -> Left (show error)
                        (Right u) -> Right u