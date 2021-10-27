{-# LANGUAGE DeriveGeneric #-}

module Language.Kmkm.Internal.Build.C.Syntax
  ( File (..)
  , Element (..)
  , Definition (..)
  , VariableQualifier (..)
  , TypeQualifier (..)
  , Type (..)
  , QualifiedType
  , Deriver (..)
  , Field (..)
  , Identifier (..)
  , Initializer (..)
  , Expression (..)
  , Literal (..)
  , IntBase (..)
  , FractionBase (..)
  , ArithmeticExpression (..)
  , BlockElement (..)
  , Statement (..)
  , Branch (..)
  ) where

import Data.String  (IsString (fromString))
import Data.Text    (Text)
import GHC.Generics (Generic)

data File =
  File Text [Either Text Element]
  deriving (Show, Eq, Ord, Generic)

data Element
  = Declaration QualifiedType [VariableQualifier] (Maybe Identifier) [Deriver]
  | Definition Definition
  | TypeDefinition (Either Text (QualifiedType, [Deriver])) Identifier
  deriving (Show, Eq, Ord, Generic)

data Definition
  = ExpressionDefinition QualifiedType [VariableQualifier] Identifier [Deriver] Initializer
  | StatementDefinition QualifiedType [VariableQualifier] Identifier [Deriver] (Either Text [Either Text BlockElement])
  deriving (Show, Eq, Ord, Generic)

data VariableQualifier
  = Constant
  | External
  deriving (Show, Read, Eq, Ord, Generic)

data TypeQualifier
  = Unsigned
  deriving (Show, Read, Eq, Ord, Generic)

data Type
  = Void
  | Char
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

data Deriver
  = Pointer [VariableQualifier]
  | Function [(QualifiedType, [VariableQualifier], Maybe (Either Text Identifier), [Deriver])]
  deriving (Show, Read, Eq, Ord, Generic)

data Field =
  Field QualifiedType Identifier [Deriver]
  deriving (Show, Read, Eq, Ord, Generic)

newtype Identifier = Identifier Text
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString Identifier where
  fromString = Identifier . fromString

data Initializer
  = ExpressionInitializer (Either Text Expression)
  | ListInitializer [Initializer]
  deriving (Show, Eq, Ord, Generic)

data Expression
  = Variable Identifier
  | Literal Literal
  | CompoundLiteral QualifiedType [Initializer]
  | ArithmeticExpression ArithmeticExpression
  | Call Expression [Expression]
  | StatementExpression [Either Text BlockElement] -- ^ GCC extension.
  | Assign Identifier Expression
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic)

data BlockElement
  = BlockStatement Statement
  | BlockDeclaration QualifiedType [VariableQualifier] (Maybe Identifier) [Deriver]
  | BlockDefinition Definition
  | BlockTypeDefinition (Either Text (QualifiedType, [Deriver])) Identifier -- ^ GCC extension.
  deriving (Show, Eq, Ord, Generic)

data Statement
  = ExpressionStatement Expression
  | Return Expression
  | If Expression Statement (Maybe Statement)
  | Case Expression [Branch]
  | Block [Either Text BlockElement]
  deriving (Show, Eq, Ord, Generic)

data Branch =
  Branch Expression Statement
  deriving (Show, Eq, Ord, Generic)
