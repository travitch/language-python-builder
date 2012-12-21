{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|

This module is modeled after the helper functions in template-haskell
that provide easy composition and name generation.  Additionally, the
helpers in this module serve to provide default annotations.  This is
a convenience as most AST nodes do not need annotations, so the
unannotated variants can provide a default empty annotation.

The helpers are each suffixed by a character describing their
category; S for statements, E for expressions, O for operators, etc.
The annotated variants include an A in the suffix.  Modules (and a few
other minor portions of the AST) cannot carry annotations.

Annotations can be any type, but the helpers that provide a default
annotation require that the annotation type be an instance of Monoid
(for mempty).  If your type cannot meet this requirement, you could
lift it with [].

The other important feature of this module is name handling.  The
'newName' helper creates a *guaranteed unique* name.  It cannot
collide with any other names in the module (except those created via
reflection within Python).  In brief, this support is provided by
tracking captured names and rewriting the AST to give names created
with 'newName' unique names after all of the captured names are
recorded.  You can subvert this by manually creating identifiers, but
that is not recommended.  'captureName' is meant to capture names
provided by imported modules, but it can also be used to fix names in
cases where you know they are unique or you want collisions.

-}
module Language.Python.Common.Builder (
  -- * Types
  Q, runQ,

  -- * Convenience Types
  ComprehensionQ, ParameterQ, ParamTupleQ, ExprQ, OpQ,
  ArgumentQ, SliceQ, StatementQ, ImportItemQ, ImportRelativeQ,
  FromItemsQ, ModuleQ, DecoratorQ, HandlerQ, RaiseExprQ,
  AssignOpQ, FromItemQ, ExceptClauseQ, CompForQ, CompIterQ, CompIfQ,

  -- * Names
  captureName, newName, makeDottedName,

  -- * Modules
  moduleM,

  -- * Statements
  importS, fromImportS, whileS, forS, funS, classS, conditionalS,
  assignS, augmentedAssignS, decoratedS, returnS, tryS, raiseS,
  withS, passS, breakS, continueS, deleteS, stmtExprS, globalS,
  nonLocalS, assertS, printS, execS,

  -- * Expressions
  varE, intE, longIntE, floatE, imaginaryE, boolE, noneE, ellipsisE,
  byteStringE, stringE, callE, subscriptE, slicedExprE, condExprE,
  binaryOpE, unaryOpE, lambdaE, tupleE, yieldE, generatorE, listCompE,
  listE, dictionaryE, dictCompE, setE, setCompE, starredE, parenE,
  stringConversionE,

  -- * Operators
  andO, orO, notO, exponentO, lessThanO, greaterThanO, equalityO,
  greaterThanEqualsO, lessThanEqualsO, notEqualsO, notEqualsV2O,
  inO, isO, isNotO, notInO, binaryOrO, xorO, binaryAndO, shiftLeftO,
  shiftRightO, multiplyO, plusO, minusO, divideO, floorDivideO,
  invertO, moduloO, dotO,

  -- * Assign operators
  plusAssignO, minusAssignO, multAssignO, divAssignO, modAssignO,
  powAssignO, binAndAssignO, binOrAssignO, binXorAssignO,
  leftShiftAssignO, rightShiftAssignO, floorDivAssignO,

  -- * Parameters
  paramP, varArgsPosP, varArgsKeywordP, endPositionalP, unPackTupleP,

  -- * Parameter tuples
  paramTupleNamePT, paramTuplePT,

  -- * Arguments
  argExprA, argVarArgsPosA, argVarArgsKeywordA, argKeywordA,

  -- * Slices
  sliceProperS, sliceExprS, sliceEllipsisS,

  -- * Decorators
  decoratorD,

  -- * Imports
  importItemI, fromItemI, importEverythingI, fromItemsI,
  importRelativeI,

  -- * Exceptions
  handlerH, exceptClauseC, raiseExprV2C, raiseExprV3C,

  -- * Comprehensions
  comprehensionC, compForC, compIfC, iterForC, iterIfC,

  -- * Annotated Names
  captureNameA, newNameA,

  -- * Annotated Statements
  importAS, fromImportAS, whileAS, forAS, funAS, classAS, conditionalAS,
  assignAS, augmentedAssignAS, decoratedAS, returnAS, tryAS, raiseAS,
  withAS, passAS, breakAS, continueAS, deleteAS, stmtExprAS, globalAS,
  nonLocalAS, assertAS, printAS, execAS,

  -- * Annotated Expressions
  varAE, intAE, longIntAE, floatAE, imaginaryAE, boolAE, noneAE,
  ellipsisAE, byteStringAE, stringAE, callAE, subscriptAE,
  slicedExprAE, condExprAE, binaryOpAE, unaryOpAE, lambdaAE,
  tupleAE, yieldAE, generatorAE, listCompAE, listAE, dictionaryAE,
  dictCompAE, setAE, setCompAE, starredAE, parenAE, stringConversionAE,

  -- * Annotated Operators
  andAO, orAO, notAO, exponentAO, lessThanAO, greaterThanAO, equalityAO,
  greaterThanEqualsAO, lessThanEqualsAO, notEqualsAO, notEqualsV2AO,
  inAO, isAO, isNotAO, notInAO, binaryOrAO, xorAO, binaryAndAO, shiftLeftAO,
  shiftRightAO, multiplyAO, plusAO, minusAO, divideAO, floorDivideAO,
  invertAO, moduloAO, dotAO,

  -- * Assign operators
  plusAssignAO, minusAssignAO, multAssignAO, divAssignAO, modAssignAO,
  powAssignAO, binAndAssignAO, binOrAssignAO, binXorAssignAO,
  leftShiftAssignAO, rightShiftAssignAO, floorDivAssignAO,

  -- * Annotated Parameters
  paramAP, varArgsPosAP, varArgsKeywordAP, endPositionalAP, unPackTupleAP,

  -- * Annotated Parameter tuples
  paramTupleNameAPT, paramTupleAPT,

  -- * Annotated Arguments
  argExprAA, argVarArgsPosAA, argVarArgsKeywordAA, argKeywordAA,

  -- * Annotated Slices
  sliceProperAS, sliceExprAS, sliceEllipsisAS,

  -- * Annotated Decorators
  decoratorAD,

  -- * Annotated Imports
  importItemAI, fromItemAI, importEverythingAI, fromItemsAI,
  importRelativeAI,

  -- * Annotated Exceptions
  handlerAH, exceptClauseAC,

  -- * Annotated Comprehensions
  comprehensionAC, compForAC, compIfAC, iterForAC, iterIfAC,

  module AST
  ) where

import Control.Exception hiding ( Handler )
import Control.Monad.State.Strict
import Data.Data ( Data, Typeable )
import Data.List ( foldl' )
import Data.Generics.Uniplate.Data
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import qualified Data.Traversable as T
import Text.Printf
import Text.Regex.TDFA

import Language.Python.Common.AST as AST hiding ( annot )

data QState = QState { nextId :: !Int
                     , capturedNames :: !(Set String)
                     }

-- | The Q monad is much like the Q(uasi) monad from template-haskell.
-- It provides hygienic name generation services.
newtype Q a = Q { unQ :: State QState a }
            deriving (Monad, MonadState QState)

-- | The state used to rewrite the AST to reduce 'UniqueIdent's into
-- normal 'Ident's.  The @idSource@ is used to implement a gensym.
-- The @existingNames@ are all of the identifiers currently in the
-- AST.  The @nameMap@ maps the Int that uniquely identifies a
-- 'UniqueIdent' to the 'Ident' that it was rewritten to.  This lets
-- the traversal share 'Ident' values maximally.
data RewriteState annot =
  RewriteState { idSource :: Int
               , existingNames :: Set String
               , nameMap :: Map Int (Ident annot)
               }

data PythonBuilderException = InvalidNameCapture String
                            | InvalidGeneratedName String
                            | EmptyDottedName
                            deriving (Show, Typeable)

instance Exception PythonBuilderException

type RewriteMonad annot = State (RewriteState annot)

-- | Take an AST fragment wrapped in the Q monad and resolve all of
-- the unique identifiers.  This can consume any AST fragment, but is
-- probably most useful with Modules.
runQ :: forall annot base . (Data annot, Biplate (base annot) (Ident annot))
        => Q (base annot) -> base annot
runQ q = evalState tM rwState
  where
    tM = transformBiM rewriteIds tempModule
    rwState = RewriteState 0 cnames M.empty
    (tempModule, QState _ cnames) = runState (unQ q) (QState 0 S.empty)

    -- | If the UniqueIdentifier has already been assigned a normal
    -- Identifier, just retrieve that.  Otherwise, assign a unique
    -- name.
    rewriteIds :: Ident annot -> RewriteMonad annot (Ident annot)
    rewriteIds i@(Ident name annot)
      | not (generatedIdentifier name) = return i
      | otherwise =
        case reads name of
          [(uid, '-' : baseName)] -> do
            s <- get
            case M.lookup uid (nameMap s) of
              Just assignedName -> return assignedName
              Nothing -> assignUniqueName baseName uid annot
          _ -> throw (InvalidGeneratedName name)
    -- | If the base name does not exist in the program, just use
    -- that.  Otherwise, gensym a suffix until there is no collision
    -- with an existing identifer.
    assignUniqueName :: String -> Int -> annot -> RewriteMonad annot (Ident annot)
    assignUniqueName base uid annot = do
      s <- get
      case base `S.member` existingNames s of
        False -> do
          let newIdent = Ident base annot
          put s { existingNames = S.insert base (existingNames s)
                , nameMap = M.insert uid newIdent (nameMap s)
                }
          return newIdent
        -- Collision, make a new ident via gensym
        True -> gensym base uid annot

    gensym :: String -> Int -> annot -> RewriteMonad annot (Ident annot)
    gensym base uid annot = do
      s <- get
      let tag = idSource s
          possibleName = printf "%s_%d" base tag
      case possibleName `S.member` existingNames s of
        False -> do
          let newIdent = Ident possibleName annot
          put s { existingNames = S.insert possibleName (existingNames s)
                , nameMap = M.insert uid newIdent (nameMap s)
                , idSource = tag + 1
                }
          return newIdent
        True -> do
          -- Still a collision, try with the next tag value.
          put s { idSource = tag + 1 }
          gensym base uid annot

-- Type aliases

type ComprehensionQ e annot = Q (Comprehension e annot)
type ParameterQ annot = Q (Parameter annot)
type ParamTupleQ annot = Q (ParamTuple annot)
type ExprQ annot = Q (Expr annot)
type OpQ annot = Q (Op annot)
type ArgumentQ annot = Q (Argument annot)
type SliceQ annot = Q (Slice annot)
type StatementQ annot = Q (Statement annot)
type ImportItemQ annot = Q (ImportItem annot)
type ImportRelativeQ annot = Q (ImportRelative annot)
type FromItemsQ annot = Q (FromItems annot)
type ModuleQ annot = Q (Module annot)
type DecoratorQ annot = Q (Decorator annot)
type HandlerQ annot = Q (Handler annot)
type RaiseExprQ annot = Q (RaiseExpr annot)
type AssignOpQ annot = Q (AssignOp annot)
type FromItemQ annot = Q (FromItem annot)
type ExceptClauseQ annot = Q (ExceptClause annot)
type CompForQ annot = Q (CompFor annot)
type CompIterQ annot = Q (CompIter annot)
type CompIfQ annot = Q (CompIf annot)

-- | Returns true if the string was an automatically-generated
-- identifier (format ###-name).  We use this ugly and type-unsafe
-- representation to avoid modifications to the underlying library.
generatedIdentifier :: String -> Bool
generatedIdentifier = (=~ "[[:digit:]]+-.*")

-- Names

-- | Make an identifier that captures a name from the environment.
-- The builder does not enforce that the name is actually in scope (it
-- cannot even if it wanted to).
--
-- This function will raise an Exception if the name is in the format
-- used for gensym names (###-string).  This is an invalid identifier
-- anyway.
captureName :: (Monoid annot) => String -> Q (Ident annot)
captureName = captureNameA mempty

captureNameA :: annot -> String -> Q (Ident annot)
captureNameA annot name
  | generatedIdentifier name = throw (InvalidNameCapture name)
  | otherwise = do
    s <- get
    put s { capturedNames = S.insert name (capturedNames s) }
    return $ Ident name annot

-- | Generate a new unique name.  It will not collide with any other
-- names in the program, no matter what identifiers the caller
-- captures or generates.
--
-- Note: reflection (e.g., eval) can circumvent this contract.
newName :: (Monoid annot) => String -> Q (Ident annot)
newName = newNameA mempty

newNameA :: annot -> String -> Q (Ident annot)
newNameA annot nameBase = do
  s <- get
  let uid = nextId s
  put s { nextId = uid + 1 }
  return $ Ident (show uid ++ "-" ++ nameBase) annot

-- | Make a dotted name expression from a list of names.
makeDottedName :: (Monoid annot) => [Ident annot] -> ExprQ annot
makeDottedName [] = throw EmptyDottedName
makeDottedName (base:names) =
  foldl' (binaryOpE dotO) (varE base) (map varE names)

-- Module

-- | Modules do not have annotations themselves
moduleM :: [StatementQ annot] -> ModuleQ annot
moduleM stmts = do
  stmts' <- sequence stmts
  return $ Module stmts'

-- Statements

-- | The import statement
--
-- > import sys
importS :: (Monoid annot)
           => [ImportItemQ annot] -- ^ The module to import
           -> StatementQ annot
importS = importAS mempty

importAS :: annot -> [ImportItemQ annot] -> StatementQ annot
importAS annot itms = do
  itms' <- sequence itms
  return $ Import itms' annot

-- | Selective @from ... import ...@ statement
--
-- > from sys import argv
--
-- The @FromItems@ parameter allows * imports.
fromImportS :: (Monoid annot)
               => ImportRelativeQ annot -- ^ The module to import
               -> FromItemsQ annot -- ^ The names to import from the module
               -> StatementQ annot
fromImportS = fromImportAS mempty

fromImportAS :: annot -> ImportRelativeQ annot -> FromItemsQ annot -> StatementQ annot
fromImportAS annot rel itms = do
  rel' <- rel
  itms' <- itms
  return $ FromImport rel' itms' annot

-- | The @while@ statement
--
-- > while condition_expr:
-- >   body
-- > else:
-- >   elsebody
--
-- The else body is optional (represented by an empty list)
whileS :: (Monoid annot)
          => ExprQ annot -- ^ While condition expression
          -> [StatementQ annot] -- ^ While body
          -> [StatementQ annot] -- ^ Else body
          -> StatementQ annot
whileS = whileAS mempty

whileAS :: annot -> ExprQ annot -> [StatementQ annot] -> [StatementQ annot] -> StatementQ annot
whileAS annot cond body els = do
  cond' <- cond
  body' <- sequence body
  els' <- sequence els
  return $ While cond' body' els' annot

-- | The @for@ statement
--
-- > for key, val in foo.iteritems():
-- >   body
-- > else:
-- >   elsebody
--
-- Again, the else body is optional.  The
forS :: (Monoid annot)
        => [ExprQ annot] -- ^ The variables to bind each iteration
        -> ExprQ annot -- ^ The expression that generates items to iterate over
        -> [StatementQ annot] -- ^ The body of the for statement
        -> [StatementQ annot] -- ^ The body of the else branch
        -> StatementQ annot
forS = forAS mempty

forAS :: annot -> [ExprQ annot] -> ExprQ annot -> [StatementQ annot] -> [StatementQ annot] -> StatementQ annot
forAS annot targets gen body els = do
  targets' <- sequence targets
  gen' <- gen
  body' <- sequence body
  els' <- sequence els
  return $ For targets' gen' body' els' annot

-- | A function declaration statement
--
-- def foo(p1, p2):
--   return p1 + p2 * 2
--
-- The optional 'ExprQ' is a python3 return value annotation for the
-- function.  It doesn't really have a meaning but the expression can
-- be introspected at runtime using __attributes__.
funS :: (Monoid annot)
        => Ident annot -- ^ The name of the function
        -> [ParameterQ annot] -- ^ The parameter list
        -> Maybe (ExprQ annot) -- ^ Optional return value annotation
        -> [StatementQ annot] -- ^ Function body
        -> StatementQ annot
funS = funAS mempty

funAS :: annot -> Ident annot -> [ParameterQ annot] -> Maybe (ExprQ annot) -> [StatementQ annot] -> StatementQ annot
funAS annot name args resAnnot body = do
  args' <- sequence args
  resAnnot' <- T.sequence resAnnot
  body' <- sequence body
  return $ Fun name args' resAnnot' body' annot

-- | A class declaration statement
--
-- > class Foo(superclass):
-- >   body
classS :: (Monoid annot)
          => Ident annot -- ^ The name of the class
          -> [ArgumentQ annot] -- ^ Superclasses (new-style classes)
          -> [StatementQ annot] -- ^ The class body
          -> StatementQ annot
classS = classAS mempty

classAS :: annot -> Ident annot -> [ArgumentQ annot] -> [StatementQ annot] -> StatementQ annot
classAS annot name args body = do
  args' <- sequence args
  body' <- sequence body
  return $ Class name args' body' annot

-- | Chains of if/elif/else statements
--
-- > if cond1:
-- >   body1
-- > elif cond2:
-- >   body2
-- > else:
-- >   elsebody
conditionalS :: (Monoid annot)
                => [(ExprQ annot, [StatementQ annot])] -- ^ Condition/body pairs
                -> [StatementQ annot] -- ^ Else body
                -> StatementQ annot
conditionalS = conditionalAS mempty

conditionalAS :: annot -> [(ExprQ annot, [StatementQ annot])] -> [StatementQ annot] -> StatementQ annot
conditionalAS annot guards els = do
  guards' <- mapM seqGuard guards
  els' <- sequence els
  return $ Conditional guards' els' annot
  where
    seqGuard (ex, stmts) = do
      ex' <- ex
      stmts' <- sequence stmts
      return (ex', stmts')

-- | Assignment statement (note, assignment in Python is a statement
-- and not an expression).
--
-- > a = b = 5
assignS :: (Monoid annot)
           => [ExprQ annot] -- ^ The values being assigned *to*
           -> ExprQ annot -- ^ The value being assigned to all of the targets
           -> StatementQ annot
assignS = assignAS mempty

assignAS :: annot -> [ExprQ annot] -> ExprQ annot -> StatementQ annot
assignAS annot to expr = do
  to' <- sequence to
  expr' <- expr
  return $ Assign to' expr' annot

-- | Augmented (compound) assignment statements.
--
-- a += 6
augmentedAssignS :: (Monoid annot)
                    => ExprQ annot -- ^ The value being modified
                    -> AssignOpQ annot -- ^ The operator to use
                    -> ExprQ annot -- ^ The input value
                    -> StatementQ annot
augmentedAssignS = augmentedAssignAS mempty

augmentedAssignAS :: annot -> ExprQ annot -> AssignOpQ annot -> ExprQ annot -> StatementQ annot
augmentedAssignAS annot to op expr = do
  to' <- to
  op' <- op
  expr' <- expr
  return $ AugmentedAssign to' op' expr' annot

-- | A decorator wrapping another statement
decoratedS :: (Monoid annot)
              => [DecoratorQ annot] -- ^ Decorator specification
              -> StatementQ annot -- ^ Decoratee
              -> StatementQ annot
decoratedS = decoratedAS mempty

decoratedAS :: annot -> [DecoratorQ annot] -> StatementQ annot -> StatementQ annot
decoratedAS annot decs stmt = do
  decs' <- sequence decs
  stmt' <- stmt
  return $ Decorated decs' stmt' annot

-- | Return statement (with optional return value).
returnS :: (Monoid annot) => Maybe (ExprQ annot) -> StatementQ annot
returnS = returnAS mempty

returnAS :: annot -> Maybe (ExprQ annot) -> StatementQ annot
returnAS annot mexpr = do
  expr' <- T.sequence mexpr
  return $ Return expr' annot

tryS :: (Monoid annot)
        => [StatementQ annot] -- ^ Try block
        -> [HandlerQ annot]   -- ^ Exception handling clauses
        -> [StatementQ annot] -- ^ Else block
        -> [StatementQ annot] -- ^ Finally block
        -> StatementQ annot
tryS = tryAS mempty

tryAS :: annot -> [StatementQ annot] -> [HandlerQ annot] -> [StatementQ annot] -> [StatementQ annot] -> StatementQ annot
tryAS annot body excepts els finallys = do
  body' <- sequence body
  excepts' <- sequence excepts
  els' <- sequence els
  finally' <- sequence finallys
  return $ Try body' excepts' els' finally' annot

raiseS :: (Monoid annot) => RaiseExprQ annot -> StatementQ annot
raiseS = raiseAS mempty

raiseAS :: annot -> RaiseExprQ annot -> StatementQ annot
raiseAS annot ex = do
  ex' <- ex
  return $ Raise ex' annot

withS :: (Monoid annot) => [(ExprQ annot, Maybe (ExprQ annot))] -> [StatementQ annot] -> StatementQ annot
withS = withAS mempty

withAS :: annot -> [(ExprQ annot, Maybe (ExprQ annot))] -> [StatementQ annot] -> StatementQ annot
withAS annot context body = do
  context' <- mapM seqContext context
  body' <- sequence body
  return $ With context' body' annot
  where
    seqContext (x1, x2) = do
      x1' <- x1
      x2' <- T.sequence x2
      return (x1', x2')

passS :: (Monoid annot) => StatementQ annot
passS = passAS mempty

passAS :: annot -> StatementQ annot
passAS = return . Pass

breakS :: (Monoid annot) => StatementQ annot
breakS = breakAS mempty

breakAS :: annot -> StatementQ annot
breakAS = return . Break

continueS :: (Monoid annot) => StatementQ annot
continueS = continueAS mempty

continueAS :: annot -> StatementQ annot
continueAS = return . Continue

deleteS :: (Monoid annot) => [ExprQ annot] -> StatementQ annot
deleteS = deleteAS mempty

deleteAS :: annot -> [ExprQ annot] -> StatementQ annot
deleteAS annot exprs = do
  exprs' <- sequence exprs
  return $ Delete exprs' annot

stmtExprS :: (Monoid annot) => ExprQ annot -> StatementQ annot
stmtExprS = stmtExprAS mempty

stmtExprAS :: annot -> ExprQ annot -> StatementQ annot
stmtExprAS annot expr = do
  expr' <- expr
  return $ StmtExpr expr' annot

globalS :: (Monoid annot) => [Ident annot] -> StatementQ annot
globalS = globalAS mempty

globalAS :: annot -> [Ident annot] -> StatementQ annot
globalAS annot ids = return $ Global ids annot

nonLocalS :: (Monoid annot) => [Ident annot] -> StatementQ annot
nonLocalS = nonLocalAS mempty

nonLocalAS :: annot -> [Ident annot] -> StatementQ annot
nonLocalAS annot ids = return $ NonLocal ids annot

assertS :: (Monoid annot) => [ExprQ annot] -> StatementQ annot
assertS = assertAS mempty

assertAS :: annot -> [ExprQ annot] -> StatementQ annot
assertAS annot exprs = do
  exprs' <- sequence exprs
  return $ Assert exprs' annot

printS :: (Monoid annot) => Bool -> [ExprQ annot] -> Bool -> StatementQ annot
printS = printAS mempty

printAS :: annot -> Bool -> [ExprQ annot] -> Bool -> StatementQ annot
printAS annot hasChev exprs hasComma = do
  exprs' <- sequence exprs
  return $ Print hasChev exprs' hasComma annot

execS :: (Monoid annot) => ExprQ annot -> Maybe (ExprQ annot, Maybe (ExprQ annot)) -> StatementQ annot
execS = execAS mempty

execAS :: annot -> ExprQ annot -> Maybe (ExprQ annot, Maybe (ExprQ annot)) -> StatementQ annot
execAS annot exec env = do
  exec' <- exec
  env' <- T.mapM seqEnvs env
  return $ Exec exec' env' annot
  where
    seqEnvs (genv, mlocenv) = do
      genv' <- genv
      locenv' <- T.sequence mlocenv
      return (genv', locenv')

-- Expressions

varE :: (Monoid annot) => Ident annot -> ExprQ annot
varE = varAE mempty

varAE :: annot -> Ident annot -> ExprQ annot
varAE annot ident = return $ Var ident annot

intE :: (Show a, Integral a, Monoid annot) => a -> ExprQ annot
intE = intAE mempty

intAE :: (Show a, Integral a) => annot -> a -> ExprQ annot
intAE annot n = return $ AST.Int (fromIntegral n) (show n) annot

longIntE :: (Show a, Integral a, Monoid annot) => a -> ExprQ annot
longIntE = longIntAE mempty

longIntAE :: (Show a, Integral a) => annot -> a -> ExprQ annot
longIntAE annot n = return $ LongInt (fromIntegral n) (show n) annot

floatE :: (Monoid annot) => Double -> ExprQ annot
floatE  = floatAE mempty

floatAE :: annot -> Double -> ExprQ annot
floatAE annot d = return $ AST.Float d (show d) annot

imaginaryE :: (Monoid annot) => Double -> ExprQ annot
imaginaryE = imaginaryAE mempty

imaginaryAE :: annot -> Double -> ExprQ annot
imaginaryAE annot d = return $ Imaginary d (show d) annot

boolE :: (Monoid annot) => Bool -> ExprQ annot
boolE = boolAE mempty

boolAE :: annot -> Bool -> ExprQ annot
boolAE annot b = return $ AST.Bool b annot

noneE :: (Monoid annot) => ExprQ annot
noneE = noneAE mempty

noneAE :: annot -> ExprQ annot
noneAE = return . None

ellipsisE :: (Monoid annot) => ExprQ annot
ellipsisE = ellipsisAE mempty

ellipsisAE :: annot -> ExprQ annot
ellipsisAE = return . Ellipsis

byteStringE :: (Monoid annot) => [String] -> ExprQ annot
byteStringE = byteStringAE mempty

byteStringAE :: annot -> [String] -> ExprQ annot
byteStringAE annot ss = return $ ByteStrings ss annot

stringE :: (Monoid annot) => [String] -> ExprQ annot
stringE = stringAE mempty

stringAE :: annot -> [String] -> ExprQ annot
stringAE annot ss = return $ Strings ss annot

callE :: (Monoid annot) => ExprQ annot -> [ArgumentQ annot] -> ExprQ annot
callE = callAE mempty

callAE :: annot -> ExprQ annot -> [ArgumentQ annot] -> ExprQ annot
callAE annot callee args = do
  callee' <- callee
  args' <- sequence args
  return $ Call callee' args' annot

subscriptE :: (Monoid annot) => ExprQ annot -> ExprQ annot -> ExprQ annot
subscriptE = subscriptAE mempty

subscriptAE :: annot -> ExprQ annot -> ExprQ annot -> ExprQ annot
subscriptAE annot s expr = do
  subscriptee' <- s
  expr' <- expr
  return $ Subscript subscriptee' expr' annot

slicedExprE :: (Monoid annot) => ExprQ annot -> [SliceQ annot] -> ExprQ annot
slicedExprE = slicedExprAE mempty

slicedExprAE :: annot -> ExprQ annot -> [SliceQ annot] -> ExprQ annot
slicedExprAE annot s slicebits = do
  slicee' <- s
  slices' <- sequence slicebits
  return $ SlicedExpr slicee' slices' annot

-- | Conditional expression
--
-- > true_value if condition else false_value
--
-- > condExprE trueVal condition falseVal
condExprE :: (Monoid annot) => ExprQ annot -> ExprQ annot -> ExprQ annot -> ExprQ annot
condExprE = condExprAE mempty

condExprAE :: annot -> ExprQ annot -> ExprQ annot -> ExprQ annot -> ExprQ annot
condExprAE annot tb cond fb = do
  tb' <- tb
  cond' <- cond
  fb' <- fb
  return $ CondExpr tb' cond' fb' annot

binaryOpE :: (Monoid annot) => OpQ annot -> ExprQ annot -> ExprQ annot -> ExprQ annot
binaryOpE = binaryOpAE mempty

binaryOpAE :: annot -> OpQ annot -> ExprQ annot -> ExprQ annot -> ExprQ annot
binaryOpAE annot op lhs rhs = do
  op' <- op
  lhs' <- lhs
  rhs' <- rhs
  return $ BinaryOp op' lhs' rhs' annot

unaryOpE :: (Monoid annot) => OpQ annot -> ExprQ annot -> ExprQ annot
unaryOpE = unaryOpAE mempty

unaryOpAE :: annot -> OpQ annot -> ExprQ annot -> ExprQ annot
unaryOpAE annot op operand = do
  op' <- op
  operand' <- operand
  return $ UnaryOp op' operand' annot

lambdaE :: (Monoid annot) => [ParameterQ annot] -> ExprQ annot -> ExprQ annot
lambdaE = lambdaAE mempty

lambdaAE :: annot -> [ParameterQ annot] -> ExprQ annot -> ExprQ annot
lambdaAE annot args body = do
  args' <- sequence args
  body' <- body
  return $ Lambda args' body' annot

tupleE :: (Monoid annot) => [ExprQ annot] -> ExprQ annot
tupleE = tupleAE mempty

tupleAE :: annot -> [ExprQ annot] -> ExprQ annot
tupleAE annot exprs = do
  exprs' <- sequence exprs
  case not (null exprs) of
    False -> return $ Tuple [] annot
    True -> parenAE annot (return $ Tuple exprs' annot)

yieldE :: (Monoid annot) => Maybe (ExprQ annot) -> ExprQ annot
yieldE = yieldAE mempty

yieldAE :: annot -> Maybe (ExprQ annot) -> ExprQ annot
yieldAE annot mex = do
  ex' <- T.sequence mex
  return $ Yield ex' annot

generatorE :: (Monoid annot) => ComprehensionQ (Expr annot) annot -> ExprQ annot
generatorE = generatorAE mempty

generatorAE :: annot -> ComprehensionQ (Expr annot) annot -> ExprQ annot
generatorAE annot gen = do
  gen' <- gen
  return $ Generator gen' annot

listCompE :: (Monoid annot) => ComprehensionQ (Expr annot) annot -> ExprQ annot
listCompE = listCompAE mempty

listCompAE :: annot -> ComprehensionQ (Expr annot) annot -> ExprQ annot
listCompAE annot comp = do
  comp' <- comp
  return $ ListComp comp' annot

listE :: (Monoid annot) => [ExprQ annot] -> ExprQ annot
listE = listAE mempty

listAE :: annot -> [ExprQ annot] -> ExprQ annot
listAE annot itms = do
  itms' <- sequence itms
  return $ List itms' annot

dictionaryE :: (Monoid annot) => [(ExprQ annot, ExprQ annot)] -> ExprQ annot
dictionaryE = dictionaryAE mempty

dictionaryAE :: annot -> [(ExprQ annot, ExprQ annot)] -> ExprQ annot
dictionaryAE annot mappings = do
  mappings' <- mapM seqPair mappings
  return $ Dictionary mappings' annot
  where
    seqPair (f, s) = do
      f' <- f
      s' <- s
      return (f', s')

dictCompE :: (Monoid annot) => ComprehensionQ (Expr annot, Expr annot) annot -> ExprQ annot
dictCompE = dictCompAE mempty

dictCompAE :: annot -> ComprehensionQ (Expr annot, Expr annot) annot -> ExprQ annot
dictCompAE annot comp = do
  comp' <- comp
  return $ DictComp comp' annot

setE :: (Monoid annot) => [ExprQ annot] -> ExprQ annot
setE = setAE mempty

setAE :: annot -> [ExprQ annot] -> ExprQ annot
setAE annot itms = do
  itms' <- sequence itms
  return $ Set itms' annot

setCompE :: (Monoid annot) => ComprehensionQ (Expr annot) annot -> ExprQ annot
setCompE = setCompAE mempty

setCompAE :: annot -> ComprehensionQ (Expr annot) annot -> ExprQ annot
setCompAE annot comp = do
  comp' <- comp
  return $ SetComp comp' annot

starredE :: (Monoid annot) => ExprQ annot -> ExprQ annot
starredE = starredAE mempty

starredAE :: annot -> ExprQ annot -> ExprQ annot
starredAE annot e = do
  e' <- e
  return $ Starred e' annot

parenE :: (Monoid annot) => ExprQ annot -> ExprQ annot
parenE = parenAE mempty

parenAE :: annot -> ExprQ annot -> ExprQ annot
parenAE annot e = do
  e' <- e
  return $ Paren e' annot

stringConversionE :: (Monoid annot) => ExprQ annot -> ExprQ annot
stringConversionE = stringConversionAE mempty

stringConversionAE :: annot -> ExprQ annot -> ExprQ annot
stringConversionAE annot e = do
  e' <- e
  return $ StringConversion e' annot

-- Operators

andO :: (Monoid annot) => OpQ annot
andO = andAO mempty

andAO :: annot -> OpQ annot
andAO = return . And

orO :: (Monoid annot) => OpQ annot
orO = orAO mempty

orAO :: annot -> OpQ annot
orAO = return . Or

notO :: (Monoid annot) => OpQ annot
notO = notAO mempty

notAO :: annot -> OpQ annot
notAO = return . Not

exponentO :: (Monoid annot) => OpQ annot
exponentO = exponentAO mempty

exponentAO :: annot -> OpQ annot
exponentAO = return . Exponent

lessThanO :: (Monoid annot) => OpQ annot
lessThanO = lessThanAO mempty

lessThanAO :: annot -> OpQ annot
lessThanAO = return . LessThan

greaterThanO :: (Monoid annot) => OpQ annot
greaterThanO = greaterThanAO mempty

greaterThanAO :: annot -> OpQ annot
greaterThanAO = return . GreaterThan

equalityO :: (Monoid annot) => OpQ annot
equalityO = equalityAO mempty

equalityAO :: annot -> OpQ annot
equalityAO = return . Equality

greaterThanEqualsO :: (Monoid annot) => OpQ annot
greaterThanEqualsO = greaterThanEqualsAO mempty

greaterThanEqualsAO :: annot -> OpQ annot
greaterThanEqualsAO = return . GreaterThanEquals

lessThanEqualsO :: (Monoid annot) => OpQ annot
lessThanEqualsO = lessThanEqualsAO mempty

lessThanEqualsAO :: annot -> OpQ annot
lessThanEqualsAO = return . LessThanEquals

notEqualsO :: (Monoid annot) => OpQ annot
notEqualsO = notEqualsAO mempty

notEqualsAO :: annot -> OpQ annot
notEqualsAO = return . NotEquals

notEqualsV2O :: (Monoid annot) => OpQ annot
notEqualsV2O = notEqualsV2AO mempty

notEqualsV2AO :: annot -> OpQ annot
notEqualsV2AO = return . NotEqualsV2

inO :: (Monoid annot) => OpQ annot
inO = inAO mempty

inAO :: annot -> OpQ annot
inAO = return . In

isO :: (Monoid annot) => OpQ annot
isO = isAO mempty

isAO :: annot -> OpQ annot
isAO = return . Is

isNotO :: (Monoid annot) => OpQ annot
isNotO = isNotAO mempty

isNotAO :: annot -> OpQ annot
isNotAO = return . IsNot

notInO :: (Monoid annot) => OpQ annot
notInO = notInAO mempty

notInAO :: annot -> OpQ annot
notInAO = return . NotIn

binaryOrO :: (Monoid annot) => OpQ annot
binaryOrO = binaryOrAO mempty

binaryOrAO :: annot -> OpQ annot
binaryOrAO = return . BinaryOr

xorO :: (Monoid annot) => OpQ annot
xorO = xorAO mempty

xorAO :: annot -> OpQ annot
xorAO = return . Xor

binaryAndO :: (Monoid annot) => OpQ annot
binaryAndO = binaryAndAO mempty

binaryAndAO :: annot -> OpQ annot
binaryAndAO = return . BinaryAnd

shiftLeftO :: (Monoid annot) => OpQ annot
shiftLeftO = shiftLeftAO mempty

shiftLeftAO :: annot -> OpQ annot
shiftLeftAO = return . ShiftLeft

shiftRightO :: (Monoid annot) => OpQ annot
shiftRightO = shiftRightAO mempty

shiftRightAO :: annot -> OpQ annot
shiftRightAO = return . ShiftRight

multiplyO :: (Monoid annot) => OpQ annot
multiplyO = multiplyAO mempty

multiplyAO :: annot -> OpQ annot
multiplyAO = return . Multiply

plusO :: (Monoid annot) => OpQ annot
plusO = plusAO mempty

plusAO :: annot -> OpQ annot
plusAO = return . Plus

minusO :: (Monoid annot) => OpQ annot
minusO = minusAO mempty

minusAO :: annot -> OpQ annot
minusAO = return . Minus

divideO :: (Monoid annot) => OpQ annot
divideO = divideAO mempty

divideAO :: annot -> OpQ annot
divideAO = return . Divide

floorDivideO :: (Monoid annot) => OpQ annot
floorDivideO = floorDivideAO mempty

floorDivideAO :: annot -> OpQ annot
floorDivideAO = return . FloorDivide

invertO :: (Monoid annot) => OpQ annot
invertO = invertAO mempty

invertAO :: annot -> OpQ annot
invertAO = return . Invert

moduloO :: (Monoid annot) => OpQ annot
moduloO = moduloAO mempty

moduloAO :: annot -> OpQ annot
moduloAO = return . Modulo

dotO :: (Monoid annot) => OpQ annot
dotO = dotAO mempty

dotAO :: annot -> OpQ annot
dotAO = return . Dot

-- Parameters

paramP :: (Monoid annot) => Ident annot -> Maybe (ExprQ annot) -> Maybe (ExprQ annot) -> ParameterQ annot
paramP = paramAP mempty

paramAP :: annot -> Ident annot -> Maybe (ExprQ annot) -> Maybe (ExprQ annot) -> ParameterQ annot
paramAP annot ident pyAnnot defVal = do
  pyAnnot' <- T.sequence pyAnnot
  defVal' <- T.sequence defVal
  return $ Param ident pyAnnot' defVal' annot

varArgsPosP :: (Monoid annot) => Ident annot -> Maybe (ExprQ annot) -> ParameterQ annot
varArgsPosP = varArgsPosAP mempty

varArgsPosAP :: annot -> Ident annot -> Maybe (ExprQ annot) -> ParameterQ annot
varArgsPosAP annot ident pyAnnot = do
  pyAnnot' <- T.sequence pyAnnot
  return $ VarArgsPos ident pyAnnot' annot

varArgsKeywordP :: (Monoid annot) => Ident annot -> Maybe (ExprQ annot) -> ParameterQ annot
varArgsKeywordP = varArgsKeywordAP mempty

varArgsKeywordAP :: annot -> Ident annot -> Maybe (ExprQ annot) -> ParameterQ annot
varArgsKeywordAP annot ident pyAnnot = do
  pyAnnot' <- T.sequence pyAnnot
  return $ VarArgsKeyword ident pyAnnot' annot

endPositionalP :: (Monoid annot) => ParameterQ annot
endPositionalP = endPositionalAP mempty

endPositionalAP :: annot -> ParameterQ annot
endPositionalAP = return . EndPositional

unPackTupleP :: (Monoid annot) => ParamTupleQ annot -> Maybe (ExprQ annot) -> ParameterQ annot
unPackTupleP = unPackTupleAP mempty

unPackTupleAP :: annot -> ParamTupleQ annot -> Maybe (ExprQ annot) -> ParameterQ annot
unPackTupleAP annot unpack def = do
  unpack' <- unpack
  def' <- T.sequence def
  return $ UnPackTuple unpack' def' annot

-- Parameter Tuples

paramTupleNamePT :: (Monoid annot) => Ident annot -> ParamTupleQ annot
paramTupleNamePT = paramTupleNameAPT mempty

paramTupleNameAPT :: annot -> Ident annot -> ParamTupleQ annot
paramTupleNameAPT annot ident = return $ ParamTupleName ident annot

paramTuplePT :: (Monoid annot) => [ParamTupleQ annot] -> ParamTupleQ annot
paramTuplePT = paramTupleAPT mempty

paramTupleAPT :: annot -> [ParamTupleQ annot] -> ParamTupleQ annot
paramTupleAPT annot tuples = do
  tuples' <- sequence tuples
  return $ ParamTuple tuples' annot

-- Arguments

argExprA :: (Monoid annot) => ExprQ annot -> ArgumentQ annot
argExprA = argExprAA mempty

argExprAA :: annot -> ExprQ annot -> ArgumentQ annot
argExprAA annot expr = do
  expr' <- expr
  return $ ArgExpr expr' annot

argVarArgsPosA :: (Monoid annot) => ExprQ annot -> ArgumentQ annot
argVarArgsPosA = argVarArgsPosAA mempty

argVarArgsPosAA :: annot -> ExprQ annot -> ArgumentQ annot
argVarArgsPosAA annot expr = do
  expr' <- expr
  return $ ArgVarArgsPos expr' annot

argVarArgsKeywordA :: (Monoid annot) => ExprQ annot -> ArgumentQ annot
argVarArgsKeywordA = argVarArgsKeywordAA mempty

argVarArgsKeywordAA :: annot -> ExprQ annot -> ArgumentQ annot
argVarArgsKeywordAA annot expr = do
  expr' <- expr
  return $ ArgVarArgsKeyword expr' annot

argKeywordA :: (Monoid annot) => Ident annot -> ExprQ annot -> ArgumentQ annot
argKeywordA = argKeywordAA mempty

argKeywordAA :: annot -> Ident annot -> ExprQ annot -> ArgumentQ annot
argKeywordAA annot ident expr = do
  expr' <- expr
  return $ ArgKeyword ident expr' annot

-- Slices

sliceProperS :: (Monoid annot) => Maybe (ExprQ annot) -> Maybe (ExprQ annot) -> Maybe (Maybe (ExprQ annot)) -> SliceQ annot
sliceProperS = sliceProperAS mempty

sliceProperAS :: annot -> Maybe (ExprQ annot) -> Maybe (ExprQ annot) -> Maybe (Maybe (ExprQ annot)) -> SliceQ annot
sliceProperAS annot lower upper stride = do
  lower' <- T.sequence lower
  upper' <- T.sequence upper
  stride' <- case stride of
    Nothing -> return Nothing
    Just s -> do
      s' <- T.sequence s
      return (Just s')
  return $ SliceProper lower' upper' stride' annot

sliceExprS :: (Monoid annot) => ExprQ annot -> SliceQ annot
sliceExprS = sliceExprAS mempty

sliceExprAS :: annot -> ExprQ annot -> SliceQ annot
sliceExprAS annot expr = do
  expr' <- expr
  return $ SliceExpr expr' annot

sliceEllipsisS :: (Monoid annot) => SliceQ annot
sliceEllipsisS = sliceEllipsisAS mempty

sliceEllipsisAS :: annot -> SliceQ annot
sliceEllipsisAS = return . SliceEllipsis

-- Decorators

decoratorD :: (Monoid annot) => DottedName annot -> [ArgumentQ annot] -> DecoratorQ annot
decoratorD = decoratorAD mempty

decoratorAD :: annot -> DottedName annot -> [ArgumentQ annot] -> DecoratorQ annot
decoratorAD annot name args = do
  args' <- sequence args
  return $ Decorator name args' annot

-- Assign ops

plusAssignO :: (Monoid annot) => AssignOpQ annot
plusAssignO = plusAssignAO mempty

plusAssignAO :: annot -> AssignOpQ annot
plusAssignAO = return . PlusAssign

minusAssignO :: (Monoid annot) => AssignOpQ annot
minusAssignO = minusAssignAO mempty

minusAssignAO :: annot -> AssignOpQ annot
minusAssignAO = return . MinusAssign

multAssignO :: (Monoid annot) => AssignOpQ annot
multAssignO = multAssignAO mempty

multAssignAO :: annot -> AssignOpQ annot
multAssignAO = return . MultAssign

divAssignO :: (Monoid annot) => AssignOpQ annot
divAssignO = divAssignAO mempty

divAssignAO :: annot -> AssignOpQ annot
divAssignAO = return . DivAssign

modAssignO :: (Monoid annot) => AssignOpQ annot
modAssignO = modAssignAO mempty

modAssignAO :: annot -> AssignOpQ annot
modAssignAO = return . ModAssign

powAssignO :: (Monoid annot) => AssignOpQ annot
powAssignO = powAssignAO mempty

powAssignAO :: annot -> AssignOpQ annot
powAssignAO = return . PowAssign

binAndAssignO :: (Monoid annot) => AssignOpQ annot
binAndAssignO = binAndAssignAO mempty

binAndAssignAO :: annot -> AssignOpQ annot
binAndAssignAO = return . BinAndAssign

binOrAssignO :: (Monoid annot) => AssignOpQ annot
binOrAssignO = binOrAssignAO mempty

binOrAssignAO :: annot -> AssignOpQ annot
binOrAssignAO = return . BinOrAssign

binXorAssignO :: (Monoid annot) => AssignOpQ annot
binXorAssignO = binXorAssignAO mempty

binXorAssignAO :: annot -> AssignOpQ annot
binXorAssignAO = return . BinXorAssign

leftShiftAssignO :: (Monoid annot) => AssignOpQ annot
leftShiftAssignO = leftShiftAssignAO mempty

leftShiftAssignAO :: annot -> AssignOpQ annot
leftShiftAssignAO = return . LeftShiftAssign

rightShiftAssignO :: (Monoid annot) => AssignOpQ annot
rightShiftAssignO = rightShiftAssignAO mempty

rightShiftAssignAO :: annot -> AssignOpQ annot
rightShiftAssignAO = return . RightShiftAssign

floorDivAssignO :: (Monoid annot) => AssignOpQ annot
floorDivAssignO = floorDivAssignAO mempty

floorDivAssignAO :: annot -> AssignOpQ annot
floorDivAssignAO = return . FloorDivAssign

-- Imports

importItemI :: (Monoid annot) => DottedName annot -> Maybe (Ident annot) -> ImportItemQ annot
importItemI = importItemAI mempty

importItemAI :: annot -> DottedName annot -> Maybe (Ident annot) -> ImportItemQ annot
importItemAI annot modName impName = return $ ImportItem modName impName annot

fromItemI :: (Monoid annot) => Ident annot -> Maybe (Ident annot) -> FromItemQ annot
fromItemI = fromItemAI mempty

fromItemAI :: annot -> Ident annot -> Maybe (Ident annot) -> FromItemQ annot
fromItemAI annot modName entity = return $ FromItem modName entity annot

importEverythingI :: (Monoid annot) => FromItemsQ annot
importEverythingI = importEverythingAI mempty

importEverythingAI :: annot -> FromItemsQ annot
importEverythingAI = return . ImportEverything

fromItemsI :: (Monoid annot) => [FromItemQ annot] -> FromItemsQ annot
fromItemsI = fromItemsAI mempty

fromItemsAI :: annot -> [FromItemQ annot] -> FromItemsQ annot
fromItemsAI annot itms = do
  itms' <- sequence itms
  return $ FromItems itms' annot

importRelativeI :: (Monoid annot) => Int -> Maybe (DottedName annot) -> ImportRelativeQ annot
importRelativeI = importRelativeAI mempty

importRelativeAI :: annot -> Int -> Maybe (DottedName annot) -> ImportRelativeQ annot
importRelativeAI annot relDots relMod =
  return $ ImportRelative relDots relMod annot

-- Exceptions

handlerH :: (Monoid annot) => ExceptClauseQ annot -> [StatementQ annot] -> HandlerQ annot
handlerH = handlerAH mempty

handlerAH :: annot -> ExceptClauseQ annot -> [StatementQ annot] -> HandlerQ annot
handlerAH annot clause body = do
  clause' <- clause
  body' <- sequence body
  return $ Handler clause' body' annot

exceptClauseC :: (Monoid annot) => Maybe (ExprQ annot, Maybe (ExprQ annot)) -> ExceptClauseQ annot
exceptClauseC = exceptClauseAC mempty

exceptClauseAC :: annot -> Maybe (ExprQ annot, Maybe (ExprQ annot)) -> ExceptClauseQ annot
exceptClauseAC annot clause = do
  clause' <- T.mapM seqClause clause
  return $ ExceptClause clause' annot
  where
    seqClause (expr, mexpr) = do
      expr' <- expr
      mexpr' <- T.sequence mexpr
      return (expr', mexpr')

raiseExprV2C :: (Maybe (ExprQ annot, Maybe (ExprQ annot, Maybe (ExprQ annot)))) -> RaiseExprQ annot
raiseExprV2C clauses = do
  clauses' <- T.mapM seqClauses clauses
  return $ RaiseV2 clauses'
  where
    seqClauses (ex, minner) = do
      ex' <- ex
      inner' <- T.mapM seqInner minner
      return (ex', inner')
    seqInner (ex, mex) = do
      ex' <- ex
      mex' <- T.sequence mex
      return (ex', mex')

raiseExprV3C :: Maybe (ExprQ annot, Maybe (ExprQ annot)) -> RaiseExprQ annot
raiseExprV3C clauses = do
  clauses' <- T.mapM seqClauses clauses
  return $ RaiseV3 clauses'
  where
    seqClauses (ex, mex) = do
      ex' <- ex
      mex' <- T.sequence mex
      return (ex', mex')

-- Comprehensions

comprehensionC :: (Monoid annot) => Q e -> CompForQ annot -> ComprehensionQ e annot
comprehensionC = comprehensionAC mempty

comprehensionAC :: annot -> Q e -> CompForQ annot -> ComprehensionQ e annot
comprehensionAC annot e for = do
  e' <- e
  for' <- for
  return $ Comprehension e' for' annot

compForC :: (Monoid annot) => [ExprQ annot] -> ExprQ annot -> Maybe (CompIterQ annot) -> CompForQ annot
compForC = compForAC mempty

compForAC :: annot -> [ExprQ annot] -> ExprQ annot -> Maybe (CompIterQ annot) -> CompForQ annot
compForAC annot forExprs inExpr forIter = do
  forExprs' <- sequence forExprs
  inExpr' <- inExpr
  forIter' <- T.sequence forIter
  return $ CompFor forExprs' inExpr' forIter' annot

compIfC :: (Monoid annot) => ExprQ annot -> Maybe (CompIterQ annot) -> CompIfQ annot
compIfC = compIfAC mempty

compIfAC :: annot -> ExprQ annot -> Maybe (CompIterQ annot) -> CompIfQ annot
compIfAC annot ifExpr ifIter = do
  ifExpr' <- ifExpr
  ifIter' <- T.sequence ifIter
  return $ CompIf ifExpr' ifIter' annot

iterForC :: (Monoid annot) => CompForQ annot -> CompIterQ annot
iterForC = iterForAC mempty

iterForAC :: annot -> CompForQ annot -> CompIterQ annot
iterForAC annot iterFor = do
  iterFor' <- iterFor
  return $ IterFor iterFor' annot

iterIfC :: (Monoid annot) => CompIfQ annot -> CompIterQ annot
iterIfC = iterIfAC mempty

iterIfAC :: annot -> CompIfQ annot -> CompIterQ annot
iterIfAC annot compIf = do
  compIf' <- compIf
  return $ IterIf compIf' annot
