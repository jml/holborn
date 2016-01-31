{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Python language analysis.

Logic for building a model of bindings and references from Python source code,
using "Holborn.Scope".

Currently missing support for:

 * class methods
 * instance attributes
 * dotted names (e.g. @import os.path@ or @from os.path import basedir@)
 * Deleting variables (e.g. @del@)
 * @global@ or @nonlocal@
 * The @exec@ statement
 * Backticks
 * Wildcard imports, e.g. @from foo import *@
-}
module Holborn.Syntax.Python (annotateSourceCode, getAST, ParseError, Token) where

import BasicPrelude hiding (lex)

import qualified Data.Map as M
import Data.Text (pack)
import Debug.Trace
import Language.Python.Common ( Argument(..)
                              , Comprehension(..)
                              , ComprehensionExpr(..)
                              , CompFor(..)
                              , CompIf(..)
                              , CompIter(..)
                              , Decorator(..)
                              , DictMappingPair(..)
                              , ExceptClause(..)
                              , Expr(..)
                              , FromItem(..)
                              , FromItems(..)
                              , Handler(..)
                              , Ident(..)
                              , ImportItem(..)
                              , Module(..)
                              , Parameter(..)
                              , ParamTuple(..)
                              , ParseError
                              , RaiseExpr(..)
                              , Slice(..)
                              , SrcSpan(..)
                              , Module(..)
                              , Slice(..)
                              , SrcSpan
                              , Statement(..)
                              , Token(token_span)
                              , YieldArg(..)
                              )
import Language.Python.Common.Token (tokenString)
import Language.Python.Version2.Lexer (lex)
import Language.Python.Version2.Parser (parseModule)

import Holborn.Syntax.Scope ( Scoped
                            , ID
                            , addReference
                            , bind
                            , calculateAnnotations
                            , enterScope
                            , exitScope
                            , unbind
                            , Interpreter(..)
                            )
import Holborn.Syntax.Types (Annotation)


getAST :: Text -> FilePath -> Either ParseError (Module SrcSpan, [Token])
getAST sourceCode = parseModule (textToString sourceCode)


getTokens :: Text -> FilePath -> Either ParseError [Token]
getTokens sourceCode = lex (textToString sourceCode)


annotateSourceCode :: Text -> Either ParseError [(String, Maybe (Annotation ID))]
annotateSourceCode sourceCode = do
  (ast, _) <- getAST sourceCode filename
  tokens <- getTokens sourceCode filename
  return $ annotateTokens ast tokens
  where filename = "<stdin>"


getSymbol :: Ident a -> (Text, a)
getSymbol (Ident name srcSpan) = (pack name, srcSpan)


bindIdent :: Ident a -> Scoped a ()
bindIdent = void . uncurry bind . getSymbol


unbindIdent :: Ident a -> Scoped a ()
unbindIdent = unbind . fst . getSymbol


addReferenceIdent :: Ident a -> Scoped a ()
addReferenceIdent = uncurry addReference . getSymbol


annotateTokens :: Module SrcSpan -> [Token] -> [(String, Maybe (Annotation ID))]
annotateTokens moduleSpan tokens =
  [(ts, M.lookup (token_span t) flattened) | t <- tokens, let ts = tokenString t, not (null ts)]
  where
    flattened = calculateAnnotations moduleSpan


-- | Given an expression that is being bound to, return all of the identifiers
-- in that expression.
--
-- Doesn't raise any errors if the expression is not a valid thing to bind to
-- (e.g. `2`), on the (possibly erroneous) assumption that by the time this is
-- called we're guaranteed to have valid Python.
--
-- Implements https://docs.python.org/2/reference/simple_stmts.html#grammar-token-target
getTargets :: Expr a -> [Ident a]
getTargets (Var ident _) = [ident]
getTargets (Tuple exprs _) = concatMap getTargets exprs
getTargets (List exprs _) = concatMap getTargets exprs
-- Valid assignees that we're not bothering to track
getTargets (Dot {}) = []  -- We don't handle attributes yet
getTargets (Subscript {}) = []   -- xs[0] = 'foo' shouldn't bind to anything
getTargets (SlicedExpr {}) = []  -- xs[0:1] = 'foo' likewise
getTargets _ = []


instance Interpreter Module a where
  interpret (Module statements) = interpretSequence statements


instance Interpreter Statement a where
  interpret (Import imports _) = interpretSequence imports
  interpret (FromImport _ fromItems _) = interpret fromItems
  interpret (While cond body elseSuite _) = do
    interpret cond
    interpretSequence body
    interpretSequence elseSuite
  interpret (For targets generator body elseSuite _) = do
    mapM_ bindIdent $ concatMap getTargets targets
    interpret generator
    interpretSequence body
    interpretSequence elseSuite
  interpret (Fun name params _ body _) = do
    bindIdent name
    enterScope
    bindParameters params
    interpretSequence body
    void exitScope
  interpret (Class name args body _) = do
    bindIdent name
    interpretSequence args
    enterScope  -- XXX: No idea whether we actually want this.
    interpretSequence body
    void exitScope
  interpret (Conditional guards elseSuite _) = do
    forM_ guards $ \(condition, suite) -> do
      interpret condition
      interpretSequence suite
    interpretSequence elseSuite
  interpret (Assign toExprs fromExpr _) = do
    mapM_ bindIdent $ concatMap getTargets toExprs
    interpret fromExpr
  interpret (AugmentedAssign toExpr _ fromExpr _) = do
    -- Treat augmented assignment as a modification rather than a binding.
    interpret toExpr
    interpret fromExpr
  interpret (Decorated decorators defn _) = do
    interpretSequence decorators
    interpret defn
  interpret (Return expr _) = interpretMaybe expr
  interpret (Try body excepts elseSuite finallySuite _) = do
    interpretSequence body
    interpretSequence excepts
    interpretSequence elseSuite
    interpretSequence finallySuite
  interpret (Raise expr _) = interpret expr
  interpret (With contexts suite _) = do
    forM_ contexts $ \(context, binding) -> do
      interpret context
      maybe (return ()) (mapM_ bindIdent . getTargets) binding
    interpretSequence suite
  interpret (Pass {}) = return ()
  interpret (Break {}) = return ()
  interpret (Continue {}) = return ()
  interpret (Delete exprs _) = do
    interpretSequence exprs
    mapM_ unbindIdent $ concatMap getTargets exprs
  interpret (StmtExpr expr _) = interpret expr
  interpret (Global {}) = _unhandled "Global"
  interpret (NonLocal {}) = _unhandled "NonLocal"
  interpret (Assert exprs _) = interpretSequence exprs
  interpret (Print _ exprs _ _) = interpretSequence exprs
  interpret (Exec {}) = _unhandled "Exec"


instance Interpreter ImportItem a where
  interpret (ImportItem _ (Just alias) _) = bindIdent alias
  interpret (ImportItem [singleName] _ _) = bindIdent singleName
  interpret (ImportItem {}) = _unhandled "dotted name in import"


instance Interpreter FromItems a where
  interpret (ImportEverything {}) = _unhandled "ImportEverything"
  interpret (FromItems items _) = interpretSequence items


instance Interpreter FromItem a where
  interpret (FromItem _ (Just alias) _) = bindIdent alias
  interpret (FromItem name Nothing _) = bindIdent name


instance Interpreter Handler a where
  interpret (Handler clause suite _) = do
    interpret clause
    interpretSequence suite


instance Interpreter ExceptClause a where
  interpret (ExceptClause e _) =
    case e of
      Just (exception, binding) -> do
        interpret exception
        maybe (return ()) (mapM_ bindIdent . getTargets) binding
      Nothing -> return ()


instance Interpreter Decorator a where
  interpret (Decorator [singleName] args _) = do
    addReferenceIdent singleName
    interpretSequence args
  interpret (Decorator {}) = _unhandled "dotted name in decorator"


instance Interpreter RaiseExpr a where
  -- | Interpret a `raise` expression.
  --
  -- In Python 2, can be `raise exception`, `raise ExceptionClass, value`, or
  -- `raise ExceptionClass, value, traceback`.
  --
  -- In Python 3, can be `raise exception` or `raise exception from
  -- other_exception`.
  --
  -- Both variants support bare `raise`.
  interpret expr =
    case expr of
      RaiseV2 Nothing -> return ()
      RaiseV2 (Just (expr', rest)) -> do
        interpret expr'
        case rest of
          Nothing -> return ()
          Just (expr'', rest') -> do
            interpret expr''
            interpretMaybe rest'
      RaiseV3 Nothing -> return ()
      RaiseV3 (Just (expr', fromExpr)) -> do
        interpret expr'
        interpretMaybe fromExpr


-- | Warn about how we've encountered an aspect of Python that we don't know
-- how to deal with.
--
-- Silently does I/O using "Debug.Trace" so we can continue processing without
-- aborting.
--
-- In the UI, this will simply look like code that isn't linkified, and any
-- references to identifiers in this code will be unresolved.
_unhandled :: Monad m => Text -> m ()
_unhandled thing = traceShow ("UNHANDLED " ++ (thing :: Text)) $ return ()


bindParameters :: [Parameter a] -> Scoped a ()
bindParameters = mapM_ bindIdent . concatMap parameterNames

parameterNames :: Parameter a -> [Ident a]
parameterNames param =
  case param of
    Param n _ _ _ -> [n]
    VarArgsPos n _ _ -> [n]
    VarArgsKeyword n _ _ -> [n]
    UnPackTuple tuple _ _ -> tupleIdentifiers tuple
    _ -> mzero
  where
    tupleIdentifiers (ParamTupleName n _) = [n]
    tupleIdentifiers (ParamTuple ts _) = concatMap tupleIdentifiers ts


instance Interpreter Argument a where

  interpret = interpret . arg_expr


instance Interpreter Expr a where
  interpret (Var identifier _) = addReferenceIdent identifier
  interpret (Int {}) = return ()
  interpret (LongInt {}) = return ()
  interpret (Float {}) = return ()
  interpret (Imaginary {}) = return ()
  interpret (Bool {}) = return ()
  interpret (None {}) = return ()
  interpret (Ellipsis {}) = return ()
  interpret (ByteStrings {}) = return ()
  interpret (Strings {}) = return ()
  interpret (UnicodeStrings {}) = return ()
  interpret (Call function args _) = do
    interpret function
    interpretSequence args
  interpret (Subscript x y _) = do
    interpret x
    interpret y
  interpret (SlicedExpr x slices' _) = void $ do
    interpret x
    interpretSequence slices'
  interpret (CondExpr trueBranch condition falseBranch _) = do
    interpret trueBranch
    interpret condition
    interpret falseBranch
  interpret (BinaryOp _ left right _) = do
    interpret left
    interpret right
  interpret (UnaryOp _ expr _) = interpret expr
  -- XXX: Should handle attributes!
  interpret (Dot expr _ _) = interpret expr
  interpret (Lambda parameters body _) = do
    enterScope
    bindParameters parameters
    interpret body
    void exitScope
  interpret (Tuple exprs _) = interpretSequence exprs
  interpret (Yield arg _) =
    case arg of
      Just (YieldFrom expr _) -> interpret expr
      Just (YieldExpr expr) -> interpret expr
      _ -> return ()
  interpret (Generator comprehension _) = interpret comprehension
  interpret (ListComp comprehension _) = interpret comprehension
  interpret (List exprs _) = interpretSequence exprs
  interpret (Dictionary pairs _) = interpretSequence pairs
  interpret (DictComp comprehension _) = interpret comprehension
  interpret (Set exprs _) = interpretSequence exprs
  interpret (SetComp comprehension _) = interpret comprehension
  interpret (Starred expr _) = interpret expr
  interpret (Paren expr _) = interpret expr
  interpret (StringConversion {}) = return ()  -- XXX: Backticks


instance Interpreter Comprehension a where
  interpret (Comprehension expr for _) = do
    interpret expr
    interpret for


instance Interpreter ComprehensionExpr a where
  interpret (ComprehensionExpr expr) = interpret expr
  interpret (ComprehensionDict dictPairs) = interpret dictPairs


instance Interpreter DictMappingPair a where
  interpret (DictMappingPair left right) = do
    interpret left
    interpret right


instance Interpreter CompFor a where
  interpret (CompFor forExprs inExpr iter _) = do
    interpretSequence forExprs
    interpret inExpr
    interpretMaybe iter


instance Interpreter CompIter a where
  interpret (IterFor iterFor _) = interpret iterFor
  interpret (IterIf iterIf _) = interpret iterIf


instance Interpreter CompIf a where
  interpret (CompIf compIf compIfIter _) = do
    interpret compIf
    interpretMaybe compIfIter


instance Interpreter Slice a where
  interpret (SliceEllipsis {}) = return ()
  interpret (SliceExpr expr _) = interpret expr
  interpret (SliceProper lower upper stride _) = do
    interpretMaybe lower
    interpretMaybe upper
    case stride of
      Just (Just expr) -> interpret expr
      _ -> return ()
