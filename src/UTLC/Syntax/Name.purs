module UTLC.Syntax.Name
  where

import Prelude

import Data.List
import Data.Tuple
import Data.Maybe

import UTLC.Utils

-- data Bind a b = Bind a b

type Name = String

newtype Env a = Env (List (Tuple String a))

-- class Named a where
--   open :: Env a -> a -> a

-- instance Named (Name a) where
--   open x = x

-- | de Bruijn indices and de Bruijn levels
newtype Ix = Ix Int
newtype Lvl = Lvl Int

derive instance Eq Ix

data IxName =
  IxName
    { name :: Name
    , ix :: Ix
    }

data LvlName =
  LvlName
    { name :: Name
    , lvl :: Lvl
    }

-- | Locally nameless variables
data V a
  = FV Name
  | BV a

derive instance Functor V

type IxV = V IxName
type LvlV = V LvlName

lvlNameIx :: Lvl -> LvlName -> IxName
lvlNameIx (Lvl depth) (LvlName n) =
  IxName
    { name: n.name
    , ix: lvlIx depth n.lvl
    }

ixNameLvl :: Lvl -> IxName -> LvlName
ixNameLvl (Lvl depth) (IxName n) =
  LvlName
    { name: n.name
    , lvl: ixLvl depth n.ix
    }

ixHere :: Ix
ixHere = Ix 0

initialLevel :: Lvl
initialLevel = Lvl 0

showIx :: Ix -> String
showIx (Ix i) = show i

ixLvl :: Int -> Ix -> Lvl
ixLvl depth (Ix i) = Lvl $ depth - i - 1

lvlIx :: Int -> Lvl -> Ix
lvlIx depth (Lvl lvl) = Ix $ depth - lvl - 1

lookupIx :: forall a. List a -> Ix -> a
lookupIx Nil (Ix i) = error $ "Cannot find binding for ix " <> show i
lookupIx (Cons x _) (Ix 0) = x
lookupIx (Cons _ xs) (Ix i) = lookupIx xs (Ix (i - 1))

newtype NamingCtx' a = NamingCtx (List (Tuple Name a))
type NamingCtx = NamingCtx' Ix

derive instance Functor NamingCtx'

emptyNamingCtx :: forall a. NamingCtx' a
emptyNamingCtx = NamingCtx Nil

-- | When we go under a binder
liftNamingCtx :: Name -> NamingCtx -> NamingCtx
liftNamingCtx name ctx =
  let NamingCtx ctx' = map shiftIx ctx
  in
  NamingCtx $
    Tuple name ixHere
      :
    ctx'

shiftIx :: Ix -> Ix
shiftIx (Ix i) = Ix (i + 1)

nextLevel :: Lvl -> Lvl
nextLevel (Lvl i) = Lvl (i + 1)

-- TODO: Is this right?
mkLevel :: forall a. List a -> Lvl
mkLevel = Lvl <<< length

nameToIx :: NamingCtx -> Name -> Ix
nameToIx nCtx n =
  case nameToIx_maybe nCtx n of
    Just r -> r
    Nothing -> error "nameToIx"

nameToIx_maybe :: NamingCtx -> Name -> Maybe Ix
nameToIx_maybe (NamingCtx nCtx) n =
  case find ((_ == n) <<< fst) nCtx of
    Just (Tuple _ i) -> Just i
    Nothing -> Nothing

ixToName :: NamingCtx -> Ix -> Name
ixToName (NamingCtx nCtx) i =
  case find ((_ == i) <<< snd) nCtx of
    Just (Tuple n _) -> n
    Nothing -> error "ixToName"

