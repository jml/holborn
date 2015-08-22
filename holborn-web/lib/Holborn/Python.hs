module Holborn.Python (annotateSourceCode, getAST, Token) where

import BasicPrelude hiding (lex)

import qualified Data.Map as M
import Language.Python.Common ( Argument(..)
                              , Expr(..)
                              , Ident(..)
                              , Parameter(..)
                              , ParamTuple(..)
                              , ParseError
                              , Module(..)
                              , SrcSpan
                              , Statement(..)
                              , Token(token_span)
                              )
import Language.Python.Common.Token (tokenString)
import Language.Python.Version2.Lexer (lex)
import Language.Python.Version2.Parser (parseModule)

import Holborn.Scope ( Annotation
                     , Scoped
                     , ID
                     , addReference
                     , bind
                     , execScoped
                     , flattenScope
                     , newScope
                     , pushScope
                     , popScope
                     )


getAST :: Text -> FilePath -> Either ParseError (Module SrcSpan, [Token])
getAST sourceCode = parseModule (textToString sourceCode)


getTokens :: Text -> FilePath -> Either ParseError [Token]
getTokens sourceCode = lex (textToString sourceCode)


annotateSourceCode :: Text -> Either ParseError [(String, Maybe (Annotation ID))]
annotateSourceCode sourceCode = do
  (ast, _) <- getAST sourceCode filename
  tokens <- getTokens sourceCode filename
  return $ calculateAnnotations ast tokens
  where filename = "<stdin>"


bindIdent :: Ident a -> Scoped a ()
bindIdent (Ident name srcSpan) = void (bind name srcSpan)


addReferenceIdent :: Ident a -> Scoped a ()
addReferenceIdent (Ident name srcSpan) = addReference name srcSpan


calculateAnnotations :: Module SrcSpan -> [Token] -> [(String, Maybe (Annotation ID))]
calculateAnnotations moduleSpan tokens =
  [(ts, M.lookup (token_span t) flattened) | t <- tokens, let ts = tokenString t, not (null ts)]
  where
    flattened = flattenScope $ execScoped (interpret moduleSpan) newScope


interpret :: Module a -> Scoped a ()
interpret (Module statements) = mapM_ interpretStatement statements

interpretStatement :: Statement a -> Scoped a ()
-- XXX: The LHS of an assignment operator can be *so* much more than just
-- single variable.
interpretStatement (Assign [Var identifier _] _ _) = bindIdent identifier
interpretStatement (Assign {}) = terror "Unhandled assignment"
interpretStatement (Fun name args _ body _) = do
  bindIdent name
  pushScope
  mapM_ bindIdent $ concatMap paramName args
  mapM_ interpretStatement body
  void popScope
  where
    paramName (Param n _ _ _) = [n]
    paramName (VarArgsPos n _ _) = [n]
    paramName (VarArgsKeyword n _ _) = [n]
    paramName (UnPackTuple tuple _ _) = tupleIdentifiers tuple
    paramName _ = []

    tupleIdentifiers (ParamTupleName n _) = [n]
    tupleIdentifiers (ParamTuple ts _) = concatMap tupleIdentifiers ts
interpretStatement (Return (Just expr) _) = interpretExpression expr
interpretStatement (Return Nothing _) = return ()
interpretStatement (StmtExpr expr _) = interpretExpression expr
interpretStatement _ = return ()


interpretExpression :: Expr a -> Scoped a ()
interpretExpression (Var identifier _) = addReferenceIdent identifier
interpretExpression (Call function args _) = do
  interpretExpression function
  mapM_ (interpretExpression . arg_expr) args
interpretExpression (Subscript x y _) = do
  interpretExpression x
  interpretExpression y
-- XXX: Should handle the slice args
interpretExpression (SlicedExpr x _ _) = interpretExpression x
interpretExpression (CondExpr trueBranch condition falseBranch _) = do
  interpretExpression trueBranch
  interpretExpression condition
  interpretExpression falseBranch
interpretExpression (BinaryOp _ left right _) = do
  interpretExpression left
  interpretExpression right
interpretExpression (UnaryOp _ expr _) = interpretExpression expr
-- XXX: Should handle attributes!
interpretExpression (Dot expr _ _) = interpretExpression expr
interpretExpression (Lambda _ body _) = do
  pushScope
  -- XXX: Handle parameter binding
  interpretExpression body
  void popScope
interpretExpression (Tuple exprs _) = mapM_ interpretExpression exprs
interpretExpression (Yield _ _) = terror "I can't be bothered implementing yield"
-- XXX: should add a debug statement here
interpretExpression _ = return ()
