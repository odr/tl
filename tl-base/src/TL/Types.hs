{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
-- Description : Common types

{-|
Module      : TL.Types
Copyright   : (c) Olshanskydr, 2016
License     : BSD3
Maintainer  : olshanskydr@gmail.com
Stability   : experimental
Portability : GHC

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module TL.Types
    where

import GHC.TypeLits(Symbol, KnownSymbol, symbolVal', SomeSymbol(..))
import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import Data.Type.Equality(type (==))
import Data.Type.Bool(type (&&))
import Lens.Micro(Lens')
import GHC.Exts(Constraint)
import qualified Data.Text as T
import Data.Aeson(ToJSON(..), FromJSON(..), Value(..)
                , fromJSON, Result(..), object, (.:))
import Data.Aeson.Types(Parser)
import Data.Tagged
import Data.Maybe(maybeToList, listToMaybe)

-- | Simple Pair. a:::b:::c:::d == (((a,b),c),d)
type (:::) (a :: k1) (b :: k2) = '(a,b)
infixl 9 :::

class Curring (r:: *) (ts :: *) where
    type Curried r ts c
    curryN :: Proxy r -> (ts -> c) -> Curried r ts c
    uncurryN :: Proxy r -> Curried r ts c -> ts -> c

-- | Representation of singleton
class Single (rep:: *) where
    type Singl rep a
    single :: Proxy# rep -> a -> Singl rep a

class Rep (rep:: *) (a :: [(k,*)]) (b :: *) | rep a -> b

-------------------- Lenses --------------------------------
class (Rep rep b br, Rep rep a ar)
    => RecLens rep b a br ar    | rep b -> br
                                , rep a -> ar
  where
    recLens :: Proxy# '(rep,b,a) -> Lens' br ar

instance    ( Rep rep (b ': bs) br
            , Rep rep '[a] ar
            , RecLensB (a==b) rep (b ': bs) '[a] br ar
            )
            => RecLens rep (b ': bs) (a ': '[]) br ar
  where
    recLens p = recLensB p (proxy# :: Proxy# (a == b))

class (Rep rep b br, Rep rep a ar)
    => RecLensB (eq::Bool) rep b a br ar
                | rep b -> br, rep a -> ar
  where
    recLensB :: Proxy# '(rep,b,a) -> Proxy# eq -> Lens' br ar

recLens' :: ( Rep rep b br
            , Rep rep (ProjNames b a) ar
            , RecLens rep b (ProjNames b a) br ar
            )
    => Proxy '(rep,b,a) -> Lens' br ar
recLens' (_:: Proxy '(rep,b,a))
    = recLens (proxy# :: Proxy# '(rep, b, ProjNames b a))

-----------------------------------------------

-- | Representation `rep` for Named record `a`
type family VRec (rep:: *) (a :: [(k,*)]) :: *
type family RepToRec (rep :: *) (a :: *) :: [*]

type family VRec' (a :: [(k,*)]) :: [*] where
    VRec' '[]       = '[]
    VRec' ('(a,b) ': xs) = b ': VRec' xs


type family NRec (a :: [(k1,k2)]) :: [k1] where
    NRec '[] = '[]
    NRec ('(a,b) ': xs) = a ': NRec xs

pNRec :: Proxy a -> Proxy (NRec a)
pNRec (_::Proxy a) = Proxy :: Proxy (NRec a)

type family ContainNames (a :: [(k,k2)]) (b :: [k]) :: Constraint where
    ContainNames as '[] = ()
    ContainNames ('(a,v) ': as) '[a] = ()
    ContainNames ('(a,v) ': as) '[b] = ContainNames as '[b]
    ContainNames as (b1 ': b2 ': bs)
        = (ContainNames as '[b1],  ContainNames as (b2 ': bs))

type family Contain (a::[k]) (b::k) :: Constraint where
    Contain (a ': as) a = ()
    Contain (a ': as) b = Contain as b

type family Contains (a::[k]) (b::[k]) :: Constraint where
    Contains as '[] = ()
    Contains as (b1 ': bs) = (Contain as b1,  Contains as bs)

type family ContainFst (a::[(k,k1)]) (b::(k,k2)) :: Constraint where
    ContainFst ('(a,x) ': as) '(a,y) = ()
    ContainFst ('(a,x) ': as) '(b,y) = ContainFst as '(b,y)

type family ContainsFst (a::[(k,k1)]) (b::[(k,k2)]) :: Constraint where
    ContainsFst as '[] = ()
    ContainsFst as (b1 ': bs) = (ContainFst as b1,  ContainsFst as bs)

type family ContainNamess (a :: [(k,k2)]) (b :: [[k]]) :: Constraint where
    ContainNamess as '[] = ()
    ContainNamess as (b ': bs) = (ContainNames as b, ContainNamess as bs)

type family MinusNames (a :: [(k,*)]) (b :: [k]) :: [(k,*)] where
    MinusNames xs '[] = xs
    MinusNames ( '(a,b) ': xs ) '[a] = xs
    MinusNames ( '(a,b) ': xs ) '[c] = '(a,b) ': MinusNames xs '[c]
    MinusNames xs (y ': ys) = MinusNames (MinusNames xs '[y]) ys

type family ProjNames  (a :: [(k,*)]) (b :: [k]) :: [(k,*)] where
    ProjNames xs '[] = '[]
    ProjNames xs '[c] = '[ProjName xs c]
    ProjNames xs (y ': ys) = ProjName xs y ': ProjNames xs ys

type family (:++) (a::[k]) (b::[k]) ::[k] where
    '[] :++ a       = a
    (a ': as) :++ b = a ': (as :++ b)

type DataKeyDef a pk = MinusNames a pk :++ ProjNames a pk

type family ProjName  (a :: [(k,*)]) (b :: k) where
    ProjName ( '(a,b) ': xs ) a = '(a,b)
    ProjName ( '(a,b) ': xs ) c = ProjName xs c

------------ Names ---------------
class Names (x :: [Symbol]) where
    symbols :: Proxy# x -> [SomeSymbol]
    names   :: Proxy# x -> [String]
instance Names '[] where
    symbols _ = []
    names _ = []
instance (KnownSymbol s, Names ss) => Names (s ': ss) where
    symbols _ = SomeSymbol (Proxy :: Proxy s) : symbols (proxy# :: Proxy# ss)
    names _ = symbolVal' (proxy# :: Proxy# s) : names (proxy# :: Proxy# ss)
class Namess (x :: [[Symbol]]) where
    namess   :: Proxy# x -> [[String]]
instance Namess '[] where
    namess _ = []
instance (Names s, Namess ss) => Namess (s ': ss) where
    namess _ = names (proxy# :: Proxy# s) : namess (proxy# :: Proxy# ss)
class Names2 (x :: [(Symbol, Symbol)]) where
    names2   :: Proxy# x -> [(String, String)]
instance Names2 '[] where
    names2 _ = []
instance (KnownSymbol s1, KnownSymbol s2, Names2 ss) => Names2 ('(s1,s2) ': ss) where
    names2 _
        = (symbolVal' (proxy# :: Proxy# s1), symbolVal' (proxy# :: Proxy# s2))
        : names2 (proxy# :: Proxy# ss)

------------------ JSON -------------------------
class ToPairs (ra::k) ar | ra -> ar
  where
    toPairs :: Tagged ra ar -> [(T.Text, Value)] -> [(T.Text, Value)]

instance ToJSON (Tagged '(r,b,a,ar,kr,dr) ar) => ToJSON (Tagged '(r,b,a,ar,kr,dr) [ar])
  where
    toJSON = toJSON . sequence
instance ToJSON (Tagged '(r,a) ar) => ToJSON (Tagged '(r,a) [ar])
  where
    toJSON = toJSON . sequence

instance FromJSON (Tagged '(r,a) ar) => FromJSON (Tagged '(r,a) [ar])
  where
    parseJSON v = fmap sequence (parseJSON v :: Parser [(Tagged '(r,a) ar)])

instance ToJSON (Tagged '(r,a) [ar]) => ToJSON (Tagged '(r,a) (Maybe ar))
  where
    toJSON = toJSON . fmap maybeToList

instance FromJSON (Tagged '(r,a) ar) => FromJSON (Tagged '(r,a) (Maybe ar))
  where
    parseJSON v
        = fmap (retag . fmap listToMaybe)
            (parseJSON v :: Parser (Tagged '(r,a) [ar]))
--------------------------------------

-- | Does this type can have default (null) value.
--   True for Maybe and [] but not for String.
type family HasDef a :: Bool where
    HasDef (Maybe a) = True
    HasDef String = False
    HasDef [a] = True
    HasDef (a,b) = HasDef a && HasDef b
    HasDef () = True
    HasDef a = False

