-- | “Redundant statement-expression removal” pass.
module Language.Kmkm.Internal.Build.C.Simplify
  ( simplify
  ) where

import Language.Kmkm.Internal.Build.C.Syntax (BlockElement (BlockDefinition, BlockStatement),
                                              Definition (ExpressionDefinition, StatementDefinition),
                                              Element (Declaration, Definition, Embedded, TypeDefinition),
                                              Expression (StatementExpression), File (File),
                                              Initializer (ExpressionInitializer, ListInitializer),
                                              Statement (Block, ExpressionStatement, Return))

simplify :: File -> File
simplify f =
  let f' = simplify' f
  in if f == f' then f else simplify f'

simplify' :: File -> File
simplify' (File n es) = File n $ element <$> es

element :: Element -> Element
element d@Declaration {}    = d
element (Definition d)      = Definition $ definition d
element d@TypeDefinition {} = d
element e@Embedded {}       = e

definition :: Definition -> Definition
definition (ExpressionDefinition t qs i ds n) = ExpressionDefinition t qs i ds $ initializer n
definition (StatementDefinition t qs i ds es@[BlockStatement (Return (StatementExpression (Block es')))]) =
  case es' of
    [] -> StatementDefinition t qs i ds $ blockElement <$> es
    _ ->
      case last es' of -- last never fail
        BlockStatement (ExpressionStatement e) ->
          let es'' = (blockElement <$> init es') ++ [BlockStatement $ Return $ expression e]
          in StatementDefinition t qs i ds es''
        _ -> StatementDefinition t qs i ds $ blockElement <$> es
definition d = d

initializer :: Initializer -> Initializer
initializer (ExpressionInitializer e) = ExpressionInitializer $ expression e
initializer (ListInitializer is)      = ListInitializer $ initializer <$> is

expression :: Expression -> Expression
expression (StatementExpression (Block [BlockStatement (ExpressionStatement e)])) = e
expression e                                                                      = e

blockElement :: BlockElement -> BlockElement
blockElement (BlockDefinition d) = BlockDefinition $ definition d
blockElement e                   = e
