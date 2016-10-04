module VCMangler.Maps
(
    operatorCodeRange,
    operatorUCodeRange,
    codeToOperator,
    encodingToHex,
    typeCodeRange,
    typeUCodeRange,
    codeToType,
    compositeCodeRange,
    codeToComposite,
    codeToPointerModifier,
    pointerModifierCodeRange,
    modifiersCodeRange,
    codeToModifiers,
    globalFunctionModifiersCodeRange,
    codeToGlobalFunctionModifiers,
    memberFunctionModifiersCodeRange,
    codeToMemberFunctionModifiers,
    callingConventionCodeRange,
    codeToCallingConvention
) where

import VCMangler.Types
import Data.Maybe (fromMaybe)

lookupOrFail :: (Eq a) => [(a, b)] -> a -> b
lookupOrFail abs a = fromMaybe (error "Out or range") (lookup a abs)

operatorMap = 
    [ ("0", OpConstructor)
    , ("1", OpDestructor)
    , ("2", OpNew)
    , ("3", OpDelete)
    , ("4", OpAssign)
    , ("5", OpRightShift)
    , ("6", OpLeftShift)
    , ("7", OpNot)
    , ("8", OpEquals)
    , ("9", OpNotEquals)
    , ("A", OpIndex)
    , ("B", OpType)
    , ("C", OpArrow)
    , ("D", OpStar)
    , ("E", OpIncrement)
    , ("F", OpDecrement)
    , ("G", OpMinus)
    , ("H", OpPlus)
    , ("I", OpAmpersand)
    , ("J", OpArrowStar)
    , ("K", OpDivide)
    , ("L", OpModulus)
    , ("M", OpLess)
    , ("N", OpGreater)
    , ("O", OpLessEqual)
    , ("P", OpGreaterEqual)
    , ("Q", OpComma)
    , ("R", OpCall)
    , ("S", OpComplement)
    , ("T", OpBitXor)
    , ("U", OpBitOr)
    , ("V", OpAnd)
    , ("W", OpOr)
    , ("X", OpMultiplyAssign)
    , ("Y", OpPlusAssign)
    , ("Z", OpMinusAssign)
    , ("_0", OpDivideAssign)
    , ("_1", OpModulusAssign)
    , ("_2", OpRightShiftAssign)
    , ("_3", OpLeftShiftAssign)
    , ("_4", OpBitAndAssign)
    , ("_5", OpBitOrAssign)
    , ("_6", OpBitXorAssign)
    , ("_7", OpVirtualTable)
    ]

operatorCodeRange = ['0'..'9'] ++ ['A'..'Z'] 
operatorUCodeRange = ['0'..'7']

codeToOperator :: String -> Operator
codeToOperator = lookupOrFail operatorMap

hexEncodingMap = 
    [ ('A', '0')
    , ('B', '1')
    , ('C', '2')
    , ('D', '3')
    , ('E', '4')
    , ('F', '5')
    , ('G', '6')
    , ('H', '7')
    , ('I', '8')
    , ('J', '9')
    , ('K', 'A')
    , ('L', 'B')
    , ('M', 'C')
    , ('N', 'D')
    , ('O', 'E')
    , ('P', 'F')
    ]

encodingToHex :: Char -> Char
encodingToHex = lookupOrFail hexEncodingMap

typeMap = 
    [ ("C", SChar_)
    , ("D", Char_)
    , ("E", UChar_)
    , ("F", Short_)
    , ("G", UShort_)
    , ("H", Int_)
    , ("I", UInt_)
    , ("J", Long_)
    , ("K", ULong_)
    , ("M", Float_)
    , ("N", Double_)
    , ("X", Void_)
    , ("Z", VarArgs_)
    , ("_N", Bool_)
    , ("_J", LongLong_)
    , ("_K", ULongLong_)
    , ("_W", WCharT_)
    ]

typeCodeRange = "MNXZ" ++ ['C'..'K']
typeUCodeRange = "NJKW"

codeToType :: String -> Type
codeToType = lookupOrFail typeMap

compositeMap = 
    [ ("T", Union)
    , ("U", Struct)
    , ("V", Class)
    , ("W4", Enum)
    ]

compositeCodeRange = ['T'..'V']

codeToComposite :: String -> CompositeType
codeToComposite = lookupOrFail compositeMap

pointerModifierMap = 
    [ ('P', [])
    , ('Q', [Const])
    , ('R', [Volatile])
    , ('S', [Const, Volatile])
    ]

pointerModifierCodeRange = ['P'..'S']

codeToPointerModifier :: Char -> [Modifier]
codeToPointerModifier = lookupOrFail pointerModifierMap

modifiersMap = 
    [ ('A', [])
    , ('B', [Const])
    , ('C', [Volatile])
    , ('D', [Const, Volatile])
    ]

modifiersCodeRange = ['A'..'D']

codeToModifiers :: Char -> [Modifier]
codeToModifiers = lookupOrFail modifiersMap

globalFunctionModifiersMap =
    [ ('Y', Near)
    , ('Z', Far)
    ]

globalFunctionModifiersCodeRange = ['Y', 'Z']

codeToGlobalFunctionModifiers :: Char -> Distance
codeToGlobalFunctionModifiers = lookupOrFail globalFunctionModifiersMap

memberFunctionModifiersMap = 
    [ ('A', (Method, Near, Private))
    , ('B', (Method, Far, Private))
    , ('C', (Static, Near, Private))
    , ('D', (Static, Far, Private))
    , ('E', (Virtual, Near, Private))
    , ('F', (Virtual, Far, Private))
    , ('I', (Method, Near, Protected))
    , ('J', (Method, Far, Protected))
    , ('K', (Static, Near, Protected))
    , ('L', (Static, Far, Protected))
    , ('M', (Virtual, Near, Protected))
    , ('N', (Virtual, Far, Protected))
    , ('Q', (Method, Near, Public))
    , ('R', (Method, Far, Public))
    , ('S', (Static, Near, Public))
    , ('T', (Static, Far, Public))
    , ('U', (Virtual, Near, Public))
    , ('V', (Virtual, Far, Public))
    ]

memberFunctionModifiersCodeRange = ['A'..'F'] ++ ['I'..'N'] ++ ['Q'..'V']

codeToMemberFunctionModifiers :: Char -> (MethodType, Distance, Access)
codeToMemberFunctionModifiers = lookupOrFail memberFunctionModifiersMap

callingConventionMap = 
    [ ('A', CDecl)
    , ('C', Pascal)
    , ('E', ThisCall)
    , ('G', StdCall)
    , ('I', FastCall)
    ]

callingConventionCodeRange = ['A', 'C', 'E', 'G', 'I']

codeToCallingConvention :: Char -> Convention
codeToCallingConvention = lookupOrFail callingConventionMap