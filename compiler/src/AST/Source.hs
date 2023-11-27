{-# OPTIONS_GHC -Wall #-}
module AST.Source
  ( Expr, Expr_(..), VarType(..)
  , Def(..)
  , Pattern, Pattern_(..)
  , Type, Type_(..)
  , Module(..)
  , getName
  , getImportName
  , Import(..)
  , Value(..)
  , Union(..)
  , Alias(..)
  , Infix(..)
  , Port(..)
  , Effects(..)
  , Manager(..)
  , Docs(..)
  , Comment(..)
  , Exposing(..)
  , Exposed(..)
  , Privacy(..)
  )
  where


import Data.Name (Name)
import qualified Data.Name as Name

import qualified AST.Utils.Binop as Binop
import qualified AST.Utils.Shader as Shader
import qualified Elm.Float as EF
import qualified Elm.String as ES
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A



-- EXPRESSIONS


type Expr = A.Located Expr_


data Expr_
  = Chr ES.String
  | Str ES.String
  | Int Int
  | Float EF.Float
  | Var VarType Name
  | VarQual VarType Name Name
  | List [Expr]
  | Op Name
  | Negate Expr
  | Binops [(Expr, A.Located Name)] Expr
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [A.Located Def] Expr
  | Case Expr [(Pattern, Expr)]
  | Accessor Name
  | Access Expr (A.Located Name)
  | Update (A.Located Name) [(A.Located Name, Expr)]
  | Record [(A.Located Name, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  | Shader Shader.Source Shader.Types
  deriving  Show


data VarType = LowVar | CapVar
  deriving Show



-- DEFINITIONS


data Def
  = Define (A.Located Name) [Pattern] Expr (Maybe Type)
  | Destruct Pattern Expr
  deriving Show



-- PATTERN


type Pattern = A.Located Pattern_


data Pattern_
  = PAnything
  | PVar Name
  | PRecord [A.Located Name]
  | PAlias Pattern (A.Located Name)
  | PUnit
  | PTuple Pattern Pattern [Pattern]
  | PCtor A.Region Name [Pattern]
  | PCtorQual A.Region Name Name [Pattern]
  | PList [Pattern]
  | PCons Pattern Pattern
  | PChr ES.String
  | PStr ES.String
  | PInt Int
  deriving Show



-- TYPE


type Type =
    A.Located Type_


data Type_
  = TLambda Type Type
  | TVar Name
  | TType A.Region Name [Type]
  | TTypeQual A.Region Name Name [Type]
  | TRecord [(A.Located Name, Type)] (Maybe (A.Located Name))
  | TUnit
  | TTuple Type Type [Type]
  deriving Show



-- MODULE


data Module =
  Module
    { _name    :: Maybe (A.Located Name)
    , _exports :: A.Located Exposing
    , _docs    :: Docs
    , _imports :: [Import]
    , _values  :: [A.Located Value]
    , _unions  :: [A.Located Union]
    , _aliases :: [A.Located Alias]
    , _binops  :: [A.Located Infix]
    , _effects :: Effects
    }
    deriving Show


getName :: Module -> Name
getName (Module maybeName _ _ _ _ _ _ _ _) =
  case maybeName of
    Just (A.At _ name) ->
      name

    Nothing ->
      Name._Main


getImportName :: Import -> Name
getImportName (Import (A.At _ name) _ _) =
  name


data Import =
  Import
    { _import :: A.Located Name
    , _alias :: Maybe Name
    , _exposing :: Exposing
    }
    deriving Show


data Value = Value (A.Located Name) [Pattern] Expr (Maybe Type)
  deriving Show
data Union = Union (A.Located Name) [A.Located Name] [(A.Located Name, [Type])]
  deriving Show
data Alias = Alias (A.Located Name) [A.Located Name] Type
  deriving Show
data Infix = Infix Name Binop.Associativity Binop.Precedence Name
  deriving Show
data Port = Port (A.Located Name) Type
  deriving Show


data Effects
  = NoEffects
  | Ports [Port]
  | Manager A.Region Manager
  deriving Show


data Manager
  = Cmd (A.Located Name)
  | Sub (A.Located Name)
  | Fx (A.Located Name) (A.Located Name)
  deriving Show


data Docs
  = NoDocs A.Region
  | YesDocs Comment [(Name, Comment)]
  deriving Show


newtype Comment =
  Comment P.Snippet
  deriving Show



-- EXPOSING


data Exposing
  = Open
  | Explicit [Exposed]
  deriving Show


data Exposed
  = Lower (A.Located Name)
  | Upper (A.Located Name) Privacy
  | Operator A.Region Name
  deriving Show


data Privacy
  = Public A.Region
  | Private
  deriving Show
