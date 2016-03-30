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
import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal', Nat, KnownNat, natVal, natVal')
import Control.Monad.Trans.State
import Safe
import Control.Monad.Trans.Class

data HtmlTag
    = None
    | Hidden
    | Input [InputAttr]
    | Choose [(Symbol,Symbol)] [InputAttr]

data InputAttr = ReadOnly | Attr Symbol Symbol

type MonadTLF m = HtmlT (StateT [Int] m)

class ToTLF a where
    toTLF :: Monad m => a -> MonadTLF m ()

getId :: Monad m => MonadTLF m T.Text
getId = lift
        $ fmap (T.intercalate ("-") . reverse . map (T.pack . show)) get
        <* modify a1
  where
    a1 [] = error "error in TL.Form.Types.getId. Empty list"
    a1 (x:xs) = x+1 : xs

nextIdLev :: Monad m => MonadTLF m ()
nextIdLev = lift $ modify (1:)

prevIdLev :: Monad m => MonadTLF m ()
prevIdLev = lift $ modify tail


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

{-
class GetId (a :: [Nat]) where
    getId  :: Proxy a  -> T.Text
    getId' :: Proxy# a -> T.Text

instance GetId '[] where
    getId _ = ""
    getId' _ = ""

instance (KnownNat x, GetId xs) => GetId (x ': xs) where
    getId _  = (let r = getId  (Proxy  :: Proxy  xs) in
                    r `mappend` if T.null r then "" else "-")
        `mappend` T.pack (show $ natVal  (Proxy  :: Proxy  x))

    getId' _ = (let r = getId' (proxy# :: Proxy# xs) in
                    r `mappend` if T.null r then "" else "-")
        `mappend` T.pack (show $ natVal' (proxy# :: Proxy# x))

-}
