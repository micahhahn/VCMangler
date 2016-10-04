{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module VCMangler.Types (
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

{-
    TODO: Add demangling for thunks such as:
        ?Release@?$CMFCComObject@VCAccessibleProxy@ATL@@@@WBA@AGKXZ
        ?QueryInterface@?$CMFCComObject@VCAccessibleProxy@ATL@@@@W3AGJABU_GUID@@PAPAX@Z
        ?AddRef@?$CMFCComObject@VCAccessibleProxy@ATL@@@@WBA@AGKXZ
        ?QueryInterface@?$CMFCComObject@VCAccessibleProxy@ATL@@@@WBA@AGJABU_GUID@@PAPAX@Z
        ?Release@?$CMFCComObject@VCAccessibleProxy@ATL@@@@W3AGKXZ
        ?AddRef@?$CMFCComObject@VCAccessibleProxy@ATL@@@@W3AGKXZ
-}

data UnmangledName where 
    Function :: QualifiedName -> FunctionModifiers -> Type -> [Type] -> UnmangledName
    deriving (Eq, Show)

data QualifiedName = Qualifier String QualifiedName
                   | TQualifier String [TemplateArgument] QualifiedName
                   | FQualifier UnmangledName QualifiedName -- Enforce this is Function? 
                   | Name String
                   | OpName Operator
                   | TName String [TemplateArgument]
                   | FName UnmangledName
                   deriving (Eq, Show)

data Convention = CDecl
                | Pascal
                | ThisCall
                | StdCall
                | FastCall 
                deriving (Eq, Show)

data Distance = Near
              | Far
              deriving (Show, Eq)

data Access = Private
            | Protected
            | Public
            deriving (Show, Eq)

data MethodType = Method
                | Static
                | Virtual
                deriving (Show, Eq)

data FunctionModifiers = Global Distance Convention
                       | Member Distance Convention MethodType Access
                       deriving (Show, Eq)

-- Eventually enforce reference is outermost type
-- Enforce Restrict can only modify Pointer
-- Enfore Reference can only have restrict modifying the pointer
data Type = Composite CompositeType QualifiedName
          | Pointer [Modifier] [Modifier] Type
          | Reference [Modifier] [Modifier] Type
          | RValueReference [Modifier] [Modifier] Type
          | FunctionPointer Convention Type [Type]
          | Qualified [Modifier] Type
          | SChar_
          | Char_
          | UChar_
          | Short_
          | UShort_
          | Int_
          | UInt_
          | Long_
          | ULong_
          | Float_
          | Double_
          | Void_
          | Bool_
          | LongLong_
          | ULongLong_
          | WCharT_
          | VarArgs_            -- Must always be last type and calling convention restricted to CDecl?
          deriving (Eq, Show)

data TConstVolatile = TConstVolatile
                    deriving (Eq, Show)

data TRestrict = TRestrict
               deriving (Eq, Show)

data Modifier = Const
              | Volatile
              | Restrict
              deriving (Eq, Show)

data CompositeType = Class
                   | Struct
                   | Union
                   | Enum
                   deriving (Eq, Show)

-- UnmangledName should be a function
data TemplateArgument = TType Type
                      | TValue Int
                      | TFunc UnmangledName
                      deriving (Eq, Show)

data Operator = OpConstructor
              | OpDestructor
              | OpNew               -- new
              | OpDelete            -- delete
              | OpAssign            -- =
              | OpRightShift        -- >>
              | OpLeftShift         -- <<
              | OpNot               -- !
              | OpEquals            -- ==
              | OpNotEquals         -- !=
              | OpIndex             -- []
              | OpType              -- TYPE()
              | OpArrow             -- ->
              | OpStar              -- * (dereference or multiply)
              | OpIncrement         -- ++
              | OpDecrement         -- --
              | OpMinus             -- - (negation or subtraction)
              | OpPlus              -- +
              | OpAmpersand         -- & (address or and)
              | OpArrowStar         -- ->*
              | OpDivide            -- /
              | OpModulus           -- %
              | OpLess              -- <
              | OpGreater           -- >
              | OpLessEqual         -- <=
              | OpGreaterEqual      -- >=
              | OpComma             -- ,
              | OpCall              -- ()
              | OpComplement        -- ~
              | OpBitXor            -- ^
              | OpBitOr             -- |
              | OpAnd               -- &&
              | OpOr                -- ||
              | OpMultiplyAssign    -- *=
              | OpPlusAssign        -- +=
              | OpMinusAssign       -- -=
              | OpDivideAssign      -- /=
              | OpModulusAssign     -- %=
              | OpRightShiftAssign  -- >>=
              | OpLeftShiftAssign   -- <<=
              | OpBitAndAssign      -- &=
              | OpBitOrAssign       -- |=
              | OpBitXorAssign      -- ^=
              | OpVirtualTable      -- virtual table?
              deriving (Eq, Ord, Show)