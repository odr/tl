{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module TL.Plain where

import Control.Monad(mzero)
import Data.Tagged
import Data.Proxy(Proxy(..))
import Data.Aeson(ToJSON(..),FromJSON(..),Value(..)
                ,fromJSON, Result(..), object, (.:))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import GHC.TypeLits -- (Symbol, KnownSymbol, symbolVal', SomeSymbol(..), Nat)
import GHC.Prim(Proxy#, proxy#)
import Lens.Micro(Lens', (^.), (.~), (&), lens)

import TL.Types

-- | Type to define simple record representation as tuple (a,).(b,).(c,)....(z,) $ ()
data Plain
proxyPlain = Proxy :: Proxy Plain

type instance VRec Plain '[]              = ()
type instance VRec Plain ('(a,b) ': xs)   = (b, VRec Plain xs)

type instance RepToRec Plain () = '[]
type instance RepToRec Plain (a,b) = a ': RepToRec Plain b


instance Curring Plain (t,()) where
    type Curried Plain (t,()) c = t -> c
    curryN _ f t = f (t,())
    uncurryN _ f (t,()) = f t

instance Curring Plain (t2,ts) => Curring Plain (t1,(t2,ts)) where
    type Curried Plain (t1,(t2,ts)) c = t1 -> Curried Plain (t2,ts) c
    curryN _ f = curryN proxyPlain . curry f
    uncurryN _ f (t1,ts) = uncurryN proxyPlain (f t1) ts

instance Single Plain where
    type Singl Plain a = (a,())
    single _ = (,())

instance Rep Plain ('[] :: [(k,*)])  ()

instance (Rep Plain xs c) => Rep Plain ('(a,b) ': xs) (b, c)

instance    ( RecLens Plain bs '[a1] xs (y1,())
            , RecLens Plain bs (a2 ': as) xs (y2,ys)
            , Rep Plain (a1 ': a2 ': as) (y1,(y2,ys))
            )
            => RecLens Plain bs (a1 ': a2 ': as) xs (y1,(y2,ys))
  where
    recLens _ = lens get set
      where
        get = (\(a,_) b -> (a, b))
            <$> (^. recLens (proxy# :: Proxy# '(Plain,bs,'[a1])))
            <*> (^. recLens (proxy# :: Proxy# '(Plain,bs,(a2 ': as))))
        set = (\x (v1,v2) -> x
                & recLens (proxy# :: Proxy# '(Plain,bs,'[a1])) .~ (v1,())
                & recLens (proxy# :: Proxy# '(Plain,bs,(a2 ': as))) .~ v2
            )

instance (Rep Plain (a ': as) (x,xs), Rep Plain '[a] (x,()))
    => RecLensB True Plain (a ': as) '[a] (x,xs) (x,())
  where
    recLensB _ _ f (x,y) = fmap ((,y).fst) $ f (x,())

instance (Rep Plain (b ': bs) (x,xs), RecLens Plain bs '[a] xs (y,()))
    => RecLensB False Plain (b ': bs) '[a] (x,xs) (y,())
  where
    recLensB _ _ f (x,y)
        = fmap (x,)
        $ recLens (proxy# :: Proxy# '(Plain,bs,'[a])) f y

------------------ JSON -------------------------

instance ToPairs '(Plain,'[]) () where
    toPairs _ = id

instance    ( KnownSymbol n
            , ToPairs '(Plain,nvs) vr
            , ToJSON v
            )
    => ToPairs '(Plain, (n:::v) ': nvs) (v,vr)
  where
    toPairs (Tagged (v,vs))
        = ((T.pack $ symbolVal' (proxy# :: Proxy# n),  toJSON v) :)
        . toPairs (Tagged vs :: Tagged '(Plain, nvs) vr)

instance FromJSON (Tagged '(Plain,'[]) ())
  where
    parseJSON (Object v)
        | HM.null v = return (Tagged () :: Tagged '(Plain,'[]) ())
        | otherwise = mzero
    parseJSON _     = mzero

instance    ( KnownSymbol n
            , FromJSON v
            , FromJSON (Tagged '(Plain, nvs) vr)
            )
            => FromJSON (Tagged '(Plain, ((n ::: v) ': nvs)) (v,vr))
  where
    parseJSON (Object hm) = do
        (Tagged rest :: Tagged '(Plain, nvs) vr) <- parseJSON (Object hm')

        -- fmap ( (Proxy :: Proxy '(Plain, ((n ::: v) ': nvs)),)
        fmap (Tagged . (,rest)) $ hm .: name
      where
        name = T.pack (symbolVal' (proxy# :: Proxy# n))
        hm' = HM.delete name hm

-- (x,y) чтобы не перекрываться с [x]
instance ToPairs '(Plain, a) (x,y) => ToJSON (Tagged '(Plain,a) (x,y)) where
    toJSON = object . flip toPairs []
instance  ToJSON (Tagged '(Plain,a) (x,y))
        => ToJSON (Tagged '(Plain,b,a,ar,kr,dr) (x,y)) where
    toJSON = toJSON
        . (retag :: Tagged '(Plain,b,a,ar,kr,dr) (x,y)
                    -> Tagged '(Plain,a) (x,y)
          )
