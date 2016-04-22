{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module TL.Form.Types where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.ByteString.Lazy      as BSL
import           Data.Functor.Identity
import           Data.Int                  (Int64)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Data.Proxy                (Proxy (..))
--import           Data.Tagged
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           GHC.Prim                  (Proxy#, proxy#)
import           GHC.TypeLits              (KnownSymbol, Symbol, symbolVal')
import           Lucid
import           Lucid.Base
-- import           Safe

data HtmlTag
    = None                                  -- ^ plain html
    | Hidden                                -- ^ <input type="hidden">
    | Input  [InputAttr]                    -- ^ <input>
    | Choose [(Symbol,Symbol)] [InputAttr]  -- ^ <select><option>...</select
    | Tab    [(Symbol,HtmlTag)]             -- ^ included table
    | Rec    [(Symbol,HtmlTag)]             -- ^ included record as table

data InputAttr = ReadOnly | Attr Symbol Symbol

data FormKind
    = LblTag        Symbol HtmlTag        -- ^ label with tag
    | RecAsTab      [(Symbol, HtmlTag)]   -- ^ editable record (as two-column table: label - input)
    | Table         [(Symbol, HtmlTag)]   -- ^ editable table
    | TableReadOnly [Symbol]              -- ^ read-only table
    | Group         [FormKind]            -- ^ composition of tables and records

data FormInternal
    = RowLblTag     Symbol HtmlTag        -- ^ row with two fields (label and tag)
    | RecInternal   [(Symbol, HtmlTag)]   -- ^ list of rows <label - input>
    | TableHeads    [Symbol]              -- ^ sequence of <th>
    | TableRow      [(Symbol, HtmlTag)]   -- ^ editable table row

data StateTLF = StateTLF
    { stlfPath    :: [T.Text]
    , stlfNameCnt :: M.Map [T.Text] Int
    } deriving (Show)

instance Monoid StateTLF where
    mempty = StateTLF mempty mempty
    (StateTLF p1 m1) `mappend` (StateTLF p2 m2)
        = StateTLF (p1 `mappend` p2) (m1 `mappend` m2)

-- type MonadTLF m = HtmlT (StateT ([Int],[Int]) m)
type MonadTLF m = HtmlT (StateT StateTLF m)

class ToTLF a where
    toTLF :: Monad m => a -> MonadTLF m ()

renderBST1 :: MonadTLF Identity a -> BSL.ByteString
renderBST1 = flip evalState mempty . renderBST

renderText1 :: MonadTLF Identity a -> TL.Text
renderText1 = flip evalState mempty . renderTextT

withName :: Monad m => MonadTLF m () -> MonadTLF m () -> MonadTLF m ()
withName tn h = do
    tid <- lift $ do
        name <- TL.toStrict <$> renderTextT tn
        (StateTLF ns m) <- get
        let k = fromMaybe 0 (M.lookup (name:ns) m)
        put $ StateTLF (name:ns) $ M.insert (name:ns) (k+1) m
        return $ T.intercalate "-" (reverse $ name:ns)
    h `with` [name_ tid]
    lift $ modify $ \s -> s { stlfPath = tail $ stlfPath s }

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

instance ToHtmlText Int64 where
    toHtmlText = T.pack . show

instance ToHtmlText Integer where
    toHtmlText = T.pack . show

instance ToHtmlText Float where
    toHtmlText = T.pack . show

instance ToHtmlText Double where
    toHtmlText = T.pack . show

instance ToHtmlText a => ToHtmlText (Maybe a) where
    toHtmlText = maybe "" toHtmlText

class GetAttrs (a :: HtmlTag) where
    getAttrs :: Proxy a -> [Attribute]

instance GetAttrs 'None where
    getAttrs _ = []

instance GetAttrs ('Input '[]) where
    getAttrs _ = []

instance GetAttrs ('Input as) => GetAttrs ('Input ('ReadOnly ': as)) where
    getAttrs (_ :: Proxy ('Input ('ReadOnly ': as)))
        = readonly_ "" : getAttrs (Proxy :: Proxy ('Input as))

instance (GetAttrs ('Input as), KnownSymbol n, KnownSymbol v)
    => GetAttrs ('Input ('Attr n v ': as))
  where
    getAttrs (_ :: Proxy ('Input ('Attr n v ': as)))
        = makeAttribute (T.pack $ symbolVal' (proxy# :: Proxy# n))
                        (T.pack $ symbolVal' (proxy# :: Proxy# v))
        : getAttrs (Proxy :: Proxy ('Input as))

instance GetAttrs ('Choose ss '[]) where
    getAttrs _ = []

instance GetAttrs ('Choose ss as) => GetAttrs ('Choose ss ('ReadOnly ': as)) where
    getAttrs (_ :: Proxy ('Choose ss ('ReadOnly ': as)))
        = disabled_ "" : getAttrs (Proxy :: Proxy ('Choose ss as))

instance (GetAttrs ('Choose ss as), KnownSymbol n, KnownSymbol v)
    => GetAttrs ('Choose ss ('Attr n v ': as))
  where
    getAttrs (_ :: Proxy ('Choose ss ('Attr n v ': as)))
        = makeAttribute (T.pack $ symbolVal' (proxy# :: Proxy# n))
                        (T.pack $ symbolVal' (proxy# :: Proxy# v))
        : getAttrs (Proxy :: Proxy ('Choose ss as))
