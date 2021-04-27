{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Builder.C.Syntax
  ( File (..)
  , Element (..)
  , Declaration (..)
  , Definition (..)
  , VariableQualifier (..)
  , TypeQualifier (..)
  , Type (..)
  , QualifiedType
  , Field (..)
  , Identifier (..)
  , Initializer (..)
  , Expression (..)
  , Literal (..)
  , IntBase (..)
  , FractionBase (..)
  , ArithmeticExpression (..)
  , Statement (..)
  , Branch (..)
  ) where

import Data.Hashable (Hashable)
import Data.String   (IsString (fromString))
import Data.Text     (Text)
import GHC.Generics  (Generic)

data File =
  File Text [Element]
  deriving (Show, Read, Eq, Ord, Generic)

data Element
  = Declaration Declaration
  | Definition Definition
  | TypeDefinition QualifiedType Identifier
  deriving (Show, Read, Eq, Ord, Generic)

data Declaration
  = ValueDeclaration [VariableQualifier] QualifiedType (Maybe Identifier)
  | FunctionDeclaration [VariableQualifier] QualifiedType Identifier [([VariableQualifier], QualifiedType, Identifier)]
  deriving (Show, Read, Eq, Ord, Generic)

data Definition
  = ValueDefinition [VariableQualifier] QualifiedType Identifier Initializer
  | FunctionDefinition [VariableQualifier] QualifiedType Identifier [([VariableQualifier], QualifiedType, Identifier)] Statement
  deriving (Show, Read, Eq, Ord, Generic)

data VariableQualifier
  = Constant
  deriving (Show, Read, Eq, Ord, Generic)

data TypeQualifier
  = Unsigned
  deriving (Show, Read, Eq, Ord, Generic)

data Type
  = Char
  | Int
  | Double
  | Float
  | TypeVariable Identifier
  | Structure Identifier
  | Enumerable Identifier
  | StructureLiteral (Maybe Identifier) [Field]
  | Union [Field]
  | EnumerableLiteral (Maybe Identifier) [Identifier]
  | Function Type [Type]
  deriving (Show, Read, Eq, Ord, Generic)

type QualifiedType = ([TypeQualifier], Type)

data Field =
  Field QualifiedType Identifier
  deriving (Show, Read, Eq, Ord, Generic)

newtype Identifier = Identifier Text
  deriving (Show, Read, Eq, Ord, Generic)

instance Hashable Identifier

instance IsString Identifier where
  fromString = Identifier . fromString

data Initializer
  = Expression Expression
  | List [Initializer]
  deriving (Show, Read, Eq, Ord, Generic)

data Expression
  = Variable Identifier
  | Literal Literal
  | CompoundLiteral QualifiedType [Initializer]
  | ArithmeticExpression ArithmeticExpression
  | Call Expression [Expression]
  deriving (Show, Read, Eq, Ord, Generic)

data Literal
  = Integer Integer IntBase
  | Fraction Integer Word Int FractionBase
  | String Text
  deriving (Show, Read, Eq, Ord, Generic)

data IntBase = IntBinary | IntOctal | IntDecimal | IntHexadecimal deriving (Show, Read, Eq, Ord, Enum, Generic)

data FractionBase = FractionDecimal | FractionHexadecimal deriving (Show, Read, Eq, Ord, Enum, Generic)

data ArithmeticExpression
  = Add Expression Expression
  | Subtract Expression Expression
  | Multiple Expression Expression
  | Divide Expression Expression
  | Minus Expression
  deriving (Show, Read, Eq, Ord, Generic)

data Statement
  = ExpressionStatement Expression
  | Return Expression
  | If Expression Statement (Maybe Statement)
  | Case Expression [Branch]
  deriving (Show, Read, Eq, Ord, Generic)

data Branch =
  Branch Expression Statement
  deriving (Show, Read, Eq, Ord, Generic)
