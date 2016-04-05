{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
module TL.Form.Types where

import Lucid
import Lucid.Base
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal', Nat, KnownNat, natVal, natVal')
import Control.Monad.Trans.State
import Safe
import Control.Monad.Trans.Class
import Data.Functor.Identity

data HtmlTag
    = None
    | Hidden
    | Input [InputAttr]
    | Choose [(Symbol,Symbol)] [InputAttr]
    | Tab [(Symbol,HtmlTag)]

data InputAttr = ReadOnly | Attr Symbol Symbol

type MonadTLF m = HtmlT (StateT Int m)

class ToTLF a where
    toTLF :: Monad m => a -> MonadTLF m ()
    jsTLF :: a -> TL.Text
    jsTLF _ = mempty

renderText1 :: MonadTLF Identity a -> TL.Text
renderText1 = runIdentity . flip evalStateT 1 . renderTextT

getId :: Monad m => MonadTLF m T.Text
getId = lift $ fmap (T.pack . show) get <* modify (+1)

class ToHtmlText a where
    toHtmlText :: a -> T.Text

instance ToHtmlText () where
    toHtmlText _ = ""

instance ToHtmlText T.Text where
    toHtmlText = id

instance ToHtmlText String where
    toHtmlText = T.pack

instance ToHtmlText Int where
    toHtmlText = T.pack . show

instance ToHtmlText Integer where
    toHtmlText = T.pack . show

instance ToHtmlText Float where
    toHtmlText = T.pack . show

instance ToHtmlText Double where
    toHtmlText = T.pack . show

class GetAttrs (a :: HtmlTag) where
    getAttrs :: Proxy a -> [Attribute]

instance GetAttrs None where
    getAttrs _ = []

instance GetAttrs (Input '[]) where
    getAttrs _ = []

instance GetAttrs (Input as) => GetAttrs (Input (ReadOnly ': as)) where
    getAttrs (_ :: Proxy (Input (ReadOnly ': as)))
        = readonly_ "" : getAttrs (Proxy :: Proxy (Input as))

instance (GetAttrs (Input as), KnownSymbol n, KnownSymbol v)
    => GetAttrs (Input (Attr n v ': as))
  where
    getAttrs (_ :: Proxy (Input (Attr n v ': as)))
        = makeAttribute (T.pack $ symbolVal' (proxy# :: Proxy# n))
                        (T.pack $ symbolVal' (proxy# :: Proxy# v))
        : getAttrs (Proxy :: Proxy (Input as))

instance GetAttrs (Choose ss '[]) where
    getAttrs _ = []

instance GetAttrs (Choose ss as) => GetAttrs (Choose ss (ReadOnly ': as)) where
    getAttrs (_ :: Proxy (Choose ss (ReadOnly ': as)))
        = disabled_ "" : getAttrs (Proxy :: Proxy (Choose ss as))

instance (GetAttrs (Choose ss as), KnownSymbol n, KnownSymbol v)
    => GetAttrs (Choose ss (Attr n v ': as))
  where
    getAttrs (_ :: Proxy (Choose ss (Attr n v ': as)))
        = makeAttribute (T.pack $ symbolVal' (proxy# :: Proxy# n))
                        (T.pack $ symbolVal' (proxy# :: Proxy# v))
        : getAttrs (Proxy :: Proxy (Choose ss as))
