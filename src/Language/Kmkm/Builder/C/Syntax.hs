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
  , ConstantExpression (..)
  ) where

import Data.Hashable (Hashable)
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
  deriving (Show, Read, Eq, Ord, Generic)

type QualifiedType = ([TypeQualifier], Type)

data Field =
  Field QualifiedType Identifier
  deriving (Show, Read, Eq, Ord, Generic)

newtype Identifier = Identifier Text
  deriving (Show, Read, Eq, Ord, Generic)

instance Hashable Identifier

data Initializer
  = Expression Expression
  | List [Initializer]
  deriving (Show, Read, Eq, Ord, Generic)

data Expression
  = Variable Identifier
  | Literal Literal
  | Compound QualifiedType [Initializer]
  | ArithmeticExpression (ArithmeticExpression Expression)
  deriving (Show, Read, Eq, Ord, Generic)

data Literal
  = Integer Integer IntBase
  | Fraction Integer Word Int FractionBase
  | String Text
  deriving (Show, Read, Eq, Ord, Generic)

data IntBase = IntBinary | IntOctal | IntDecimal | IntHexadecimal deriving (Show, Read, Eq, Ord, Enum, Generic)

data FractionBase = FractionDecimal | FractionHexadecimal deriving (Show, Read, Eq, Ord, Enum, Generic)

data ArithmeticExpression e
  = Add e e
  | Subtract e e
  | Multiple e e
  | Divide e e
  | Minus e
  deriving (Show, Read, Eq, Ord, Generic)

data Statement
  = ExpressionStatement Expression
  | Return Expression
  | If Expression Statement (Maybe Statement)
  | Case Expression [Branch]
  deriving (Show, Read, Eq, Ord, Generic)

data Branch =
  Branch ConstantExpression Statement
  deriving (Show, Read, Eq, Ord, Generic)

data ConstantExpression
  = ConstantLiteral Literal
  | ConstantArithmeticExpression ConstantExpression
  deriving (Show, Read, Eq, Ord, Generic)
