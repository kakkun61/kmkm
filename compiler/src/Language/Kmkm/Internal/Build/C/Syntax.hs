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
  , C (..)
  ) where

import           Data.Function         (on)
import           Data.String           (IsString (fromString))
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import qualified Language.C.Pretty     as C
import           Language.C.Syntax.AST (CExtDecl)
import qualified Text.PrettyPrint      as P

data File =
  File Text [Element]
  deriving (Show, Eq, Ord, Generic)

data Element
  = Declaration QualifiedType [VariableQualifier] (Maybe Identifier) [Deriver]
  | Definition Definition
  | TypeDefinition QualifiedType Identifier
  | Embedded C
  deriving (Show, Eq, Ord, Generic)

data Definition
  = ExpressionDefinition QualifiedType [VariableQualifier] Identifier [Deriver] Initializer
  | StatementDefinition QualifiedType [VariableQualifier] Identifier [Deriver] [BlockElement]
  deriving (Show, Eq, Ord, Generic)

data VariableQualifier
  = Constant
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
  | Function [(QualifiedType, [VariableQualifier], Maybe Identifier, [Deriver])]
  deriving (Show, Read, Eq, Ord, Generic)

data Field =
  Field QualifiedType Identifier
  deriving (Show, Read, Eq, Ord, Generic)

newtype Identifier = Identifier Text
  deriving (Show, Read, Eq, Ord, Generic)

instance IsString Identifier where
  fromString = Identifier . fromString

data Initializer
  = ExpressionInitializer Expression
  | ListInitializer [Initializer]
  deriving (Show, Eq, Ord, Generic)

data Expression
  = Variable Identifier
  | Literal Literal
  | CompoundLiteral QualifiedType [Initializer]
  | ArithmeticExpression ArithmeticExpression
  | Call Expression [Expression]
  | StatementExpression Statement -- ^ GCC extension.
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
  | BlockTypeDefinition QualifiedType Identifier -- ^ GCC extension.
  | BlockEmbed C
  deriving (Show, Eq, Ord, Generic)

data Statement
  = ExpressionStatement Expression
  | Return Expression
  | If Expression Statement (Maybe Statement)
  | Case Expression [Branch]
  | Block [BlockElement]
  deriving (Show, Eq, Ord, Generic)

data Branch =
  Branch Expression Statement
  deriving (Show, Eq, Ord, Generic)

newtype C = C CExtDecl deriving (Show, Generic)

instance Eq C where
  (==) = (==) `on` (\(C c) -> P.render $ C.pretty c)

instance Ord C where
  compare = compare `on` (\(C c) -> P.render $ C.pretty c)