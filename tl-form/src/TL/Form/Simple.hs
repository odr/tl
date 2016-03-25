{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
module TL.Form.Simple where

import Lucid
import qualified Data.Text as T
import Data.Tagged
import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import Data.String(fromString)
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal')

import TL.Types(Names2(..))
import TL.Form.Types

-- | Style of simple rendering.
data Simple

instance {-# OVERLAPPABLE #-} ToHtmlText a => ToHtml (Tagged Simple a) where
    toHtml      = toHtml    . untag . fmap toHtmlText
    toHtmlRaw   = toHtmlRaw . untag . fmap toHtmlText

instance {-# OVERLAPPING #-} ToHtml (Tagged Simple Bool) where
    toHtml    (Tagged b) = toHtml    (if b then "+" else "-" :: T.Text)
    toHtmlRaw (Tagged b) = toHtmlRaw (if b then "+" else "-" :: T.Text)

instance {-# OVERLAPPING #-} (ToHtml (Tagged Simple a))
    => ToHtml (Tagged Simple (Maybe a))
  where
    toHtml    = toHtml    . sequence
    toHtmlRaw = toHtmlRaw . sequence

instance ToHtml (Tagged Simple a) => ToHtml (Maybe (Tagged Simple a))
  where
    toHtml    = maybe (toHtml    (""::T.Text)) toHtml
    toHtmlRaw = maybe (toHtmlRaw (""::T.Text)) toHtmlRaw

type RPTI as a    = Tagged '(Simple, Input as    ) (Maybe a)
type RPTC vs ps a = Tagged '(Simple, Choose vs ps) (Maybe a)

instance {-# OVERLAPPABLE #-} (ToHtmlText a, GetAttrs (Input as)) => ToHtml (RPTI as a)
  where
    toHtml    (Tagged b :: RPTI as a)
        = input_
            . maybe id ((:) . value_ . toHtmlText) b
            $ getAttrs (Proxy :: Proxy (Input as))
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToHtml (RPTI as Bool)
  where
    toHtml    (Tagged b :: RPTI as Bool)
        = input_
            . (if b == Just True then (checked_ :) else id)
            . (type_ "checkbox" :)
            $ getAttrs (Proxy :: Proxy (Input as))
    toHtmlRaw = toHtml

inputNum :: (Num a, GetAttrs as, ToHtmlText a, Monad m) => Proxy as -> (Maybe a) -> HtmlT m ()
inputNum p v = input_ . maybe id ((:) . value_ . toHtmlText) v $ type_ "number" : getAttrs p

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToHtml (RPTI as Int) where
    toHtml (Tagged b :: RPTI as Int) = inputNum (Proxy :: Proxy (Input as)) b
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToHtml (RPTI as Integer) where
    toHtml (Tagged b :: RPTI as Integer) = inputNum (Proxy :: Proxy (Input as)) b
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToHtml (RPTI as Float) where
    toHtml (Tagged b :: RPTI as Float) = inputNum (Proxy :: Proxy (Input as)) b
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToHtml (RPTI as Double) where
    toHtml (Tagged b :: RPTI as Double) = inputNum (Proxy :: Proxy (Input as)) b
    toHtmlRaw = toHtml

instance (ToHtmlText a, Names2 vs, GetAttrs (Choose vs ps))
    => ToHtml (RPTC vs ps a)
  where
    toHtml (Tagged x :: RPTC vs ps a)
        = select_
            (mapM_  (\(v,n) -> with (option_ $ fromString n)
                                    (f v [value_ (fromString v)])
                    ) $ names2 (proxy# :: Proxy# vs)
            ) `with` getAttrs (Proxy :: Proxy (Choose vs ps))
      where
        val = fmap toHtmlText x
        f v | val == Just (T.pack v)    = (selected_ "" :)
            | otherwise                 = id
    toHtmlRaw = toHtml

instance {-# OVERLAPPABLE #-} ToHtml (THV h (Maybe v)) => ToHtml (THV h v) where
    toHtml = toHtml . fmap Just
    toHtmlRaw = toHtml

type THV (h :: HtmlTag) v = Tagged '(Simple, h) v
type TNHV (n :: Symbol) (h :: HtmlTag) v = Tagged '(Simple, n, h) v

instance (ToHtml (THV h v), KnownSymbol n) => ToHtml (TNHV n h v)
  where
    toHtml x = div_ $ do
        label_ (fromString $ symbolVal' (proxy# :: Proxy# n) ++ ": ")
        toHtml (retag x :: THV h v)
    toHtmlRaw = toHtml


