{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module TL.Form.Types where

import Lucid
import Data.Tagged
import qualified Data.Text as T
import Data.Proxy(Proxy(..))
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal')

data HtmlTag = None
            | Input [InputAttr]
            | Choose [(Symbol,Symbol)] [InputAttr]

data InputAttr = ReadOnly
    --  - | Type TypeInput

-- data TypeInput = Button | CheckBox | File | Hidden | Password |

-- -- | Render params
-- data RP a = RPT { rpStyle :: a
--                 , rpParam :: HtmlTag
--                 }

-- -- | Like ToHtml but without Raw
-- class ToHtml1 a where
--     toHtml' :: Monad m => a -> HtmlT m ()
--

-- instance ToHtml (Tagged t x) => ToHtml (Tagged (RPT t None) x) where
--     toHtml    = toHtml    . (retag :: (Tagged (RPT t None) x -> Tagged t x))
--     toHtmlRaw = toHtmlRaw . (retag :: (Tagged (RPT t None) x -> Tagged t x))

class ToHtmlText a where
    toHtmlText :: a -> T.Text

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

instance GetAttrs (Choose ss '[]) where
    getAttrs _ = []

instance GetAttrs (Choose ss as) => GetAttrs (Choose ss (ReadOnly ': as)) where
    getAttrs (_ :: Proxy (Choose ss (ReadOnly ': as)))
        = disabled_ "" : getAttrs (Proxy :: Proxy (Choose ss as))

