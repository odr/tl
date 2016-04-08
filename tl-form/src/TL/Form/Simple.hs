{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
module TL.Form.Simple where

import Lucid
import qualified Data.Text as T
import Data.Tagged
import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import Data.String(fromString)
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal', Nat, type (+))

import TL.Types(Names2(..), LFst, LSnd, ZipK2)
import TL.Form.Types

-- | Style of simple rendering.
data Simple

type TSimple (a::k) = Tagged '(Simple, a)
--------------- output-only values ------------------
instance {-# OVERLAPPABLE #-} ToHtmlText a => ToHtml (Tagged Simple a) where
    toHtml = toHtml . untag . fmap toHtmlText
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} ToHtml (Tagged Simple Bool) where
    toHtml (Tagged b) = toHtml (if b then "+" else "-" :: T.Text)
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (ToHtml (Tagged Simple a))
    => ToHtml (Tagged Simple (Maybe a))
  where
    toHtml = toHtml . sequence
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (ToTLF (Tagged Simple a), ToTLF (Tagged Simple as))
    => ToTLF (Tagged Simple (a,as))
  where
    toTLF x = do
        td_ $ toTLF $ fmap fst x
        toTLF $ fmap snd x

instance ToHtml (Tagged Simple a) => ToHtml (Maybe (Tagged Simple a))
  where
    toHtml = maybe (toHtml (""::T.Text)) toHtml
    toHtmlRaw = toHtml

----------------------- Rendering for Input --------------
type INone         = TSimple None
type IHidden       = TSimple Hidden           -- (Maybe a)
type IInput as     = TSimple (Input as)       -- (Maybe a)
type IChoose vs ps = TSimple (Choose vs ps)   -- (Maybe a)

instance ToHtml (Tagged Simple v) =>  ToTLF (INone v) 
  where
    toTLF x = toHtml (retag x :: Tagged Simple v)

-- Input field (text)
instance {-# OVERLAPPABLE #-} (ToHtmlText a, GetAttrs (Input as))
    => ToTLF (IInput as (Maybe a))
  where
    toTLF (Tagged b :: IInput as (Maybe a))
        = input_
            . maybe id ((:) . value_ . toHtmlText) b
            $ getAttrs (Proxy :: Proxy (Input as))
            
-- Input field (checkbox)            
instance {-# OVERLAPPING #-} (GetAttrs (Input as))
    => ToTLF (IInput as (Maybe Bool))
  where
    toTLF (Tagged b)
        = input_
            . (if b == Just True then (checked_ :) else id)
            . (type_ "checkbox" :)
            $ getAttrs (Proxy :: Proxy (Input as))

-- | Input field for numbers
inputNum :: (Num a, GetAttrs (Input as), ToHtmlText a, Monad m)
    => IInput as (Maybe a) -> MonadTLF m ()
inputNum (Tagged v :: IInput as (Maybe a))
    = input_ . maybe id ((:) . value_ . toHtmlText) v
    $ type_ "number" : getAttrs (Proxy :: Proxy (Input as))

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToTLF (IInput as (Maybe Int))
  where
    toTLF = inputNum

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToTLF (IInput as (Maybe Integer))
  where
    toTLF = inputNum

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToTLF (IInput as (Maybe Float))
  where
    toTLF = inputNum

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToTLF (IInput as (Maybe Double))
  where
    toTLF = inputNum

------------------- Rendering for Choose value ------------
instance (ToHtmlText a, Names2 vs, GetAttrs (Choose vs ps))
    => ToTLF (IChoose vs ps (Maybe a))
  where
    toTLF (Tagged x :: IChoose vs ps (Maybe a))
        = select_
            (mapM_  (\(v,n) -> with (option_ $ fromString n)
                                    (f v [value_ (fromString v)])
                    ) $ names2 (proxy# :: Proxy# vs)
            ) `with`
            ( getAttrs (Proxy :: Proxy (Choose vs ps)))
      where
        val = fmap toHtmlText x
        f v | val == Just (T.pack v)    = (selected_ "" :)
            | otherwise                 = id

------------------- Rendering for Hidden value ------------
instance ToHtmlText a => ToTLF (IHidden (Maybe a)) where
    toTLF (Tagged v :: IHidden (Maybe a))
        = input_
            (maybe id ((:) . value_ . toHtmlText) v [type_ "hidden"])

------------------- Non-maybe ---------------------------
instance {-# OVERLAPPABLE #-} ToTLF (TSimple h (Maybe v)) 
    => ToTLF (TSimple h v) 
  where
    toTLF = toTLF . fmap Just

----------------------------------
-- just text
type TN (n::Symbol) = TSimple n ()
-- sequence of <th>
type TTableHeads ns = TSimple (TableHeads ns)
-- label with tag
type TLblTag n h    = TSimple (LblTag n h)
-- row with two fields (label and tag)
type TRowLblTag n h = TSimple (RowLblTag n h)

---------------- Rendering with label -----------------------
instance KnownSymbol n => ToHtml (TN n) where
    toHtml _  = fromString $ symbolVal' (proxy# :: Proxy# n)
    toHtmlRaw = toHtml

instance ToTLF (TTableHeads '[] ()) where
    toTLF _  = return ()

instance (ToHtml (TN n), ToTLF (TTableHeads ns ())) => ToTLF (TTableHeads (n ': ns) ()) where
    toTLF _ = do
        withName tn $ th_ tn
        toTLF (Tagged () :: TTableHeads ns ())
      where
        tn :: (ToHtml (TN n), Monad m) => MonadTLF m ()
        tn = toHtml (Tagged () :: TN n)

labeledHtml :: (ToTLF (TSimple h v), ToHtml (TN n), Monad m)
            => TLblTag n h v -> MonadTLF m ()
labeledHtml (x :: TLblTag n h v) = withName tn $
    label_ $ do
        tn >> ": "
        toTLF (retag x :: TSimple h v)
  where
    tn :: (ToHtml (TN n), Monad m) => MonadTLF m ()
    tn = toHtml (Tagged () :: TN n)
         

instance {-# OVERLAPPABLE #-} (ToTLF (TSimple h v), ToHtml (TN n))
    => ToTLF (TLblTag n h v)
  where
    toTLF    = labeledHtml

instance {-# OVERLAPPING #-} (ToTLF (Tagged '(Simple, Hidden) v), ToHtml (TN n))
    => ToTLF (TLblTag n Hidden v)
  where
    toTLF x = labeledHtml x `with` [hidden_ ""]

---------------- Rendering as table raw -----------------------
rowHtml :: (ToTLF (TSimple h v), ToHtml (TN n), Monad m) 
    => TRowLblTag n h v -> MonadTLF m ()
rowHtml (x :: TRowLblTag n h v) = withName tn $ tr_ $ do
    td_ $ tn >> ": "
    td_ $ toTLF (retag x :: TSimple h v)
  where
    tn :: (ToHtml (TN n), Monad m) => MonadTLF m ()
    tn = toHtml (Tagged () :: TN n)

instance {-# OVERLAPPABLE #-} (ToTLF (TSimple h v), ToHtml (TN n))
    => ToTLF (TRowLblTag n h v)
  where
    toTLF = rowHtml

instance {-# OVERLAPPING #-}
    (ToTLF (Tagged '(Simple, Hidden) v), ToHtml (TN n))
    => ToTLF (TRowLblTag n Hidden v)
  where
    toTLF x = rowHtml x `with` [hidden_ ""]

----------------- Rendering table -------------------------------
-- editable record (as two-column table: label - input)
type TRecAsTab      rs = TSimple (RecAsTab rs)    -- (Maybe v)
type TRecInternal   rs = TSimple (RecInternal rs) -- (Maybe v)
type TTable         rs = TSimple (Table rs)       -- [v]
type TTableRow      rs = TSimple (TableRow rs)
type TTableReadOnly rs = TSimple (TableReadOnly rs)

instance ToTLF (TRecAsTab '[] (Maybe ()))
  where
    toTLF    _ = return ()

instance ToTLF (TRecInternal '[] (Maybe ()))
  where
    toTLF    _ = return ()

instance (ToTLF (TRowLblTag n h (Maybe v)), ToTLF (TRecInternal rs (Maybe vs)))
    => ToTLF (TRecInternal ('(n, h) ': rs) (Maybe (v,vs)))
  where
    toTLF x = do
        toTLF (retag $ fmap (fmap fst) x :: TRowLblTag n h (Maybe v))
        toTLF (retag $ fmap (fmap snd) x :: TRecInternal rs (Maybe vs))

instance {-# OVERLAPPING #-} ToTLF (TRecInternal (r ': rs) (Maybe xs))
    => ToTLF (TRecAsTab (r ': rs) (Maybe xs))
  where
    toTLF zs = do
        table_ $ toTLF (retag zs :: TRecInternal (r ': rs) (Maybe xs))

instance {-# OVERLAPPABLE #-} ToTLF (TRecAsTab rs (Maybe xs)) 
    => ToTLF (TRecAsTab rs xs)
  where
    toTLF    = toTLF . fmap Just

instance ToTLF (TTableRow '[] ()) where
    toTLF _ = return ()

instance (ToTLF (TSimple r v), ToTLF (TTableRow rs vs), ToHtml (TN n)) 
    => ToTLF (TTableRow ( '(n,r) ': rs) (v,vs))
  where
    toTLF x = do
        withName tn $ td_ $ toTLF (retag $ fmap fst x :: TSimple r v)
        toTLF (retag $ fmap snd x :: TTableRow rs vs)
      where
        tn :: (ToHtml (TN n), Monad m) => MonadTLF m ()
        tn = toHtml (Tagged () :: TN n)

instance (ToTLF (TTableHeads (LFst rs) ()), ToTLF (TTableRow rs v)) 
    => ToTLF (TTable rs [v])
  where
    toTLF (Tagged xs)
        = do
            table_ $ do
                tr_ $ toTLF (Tagged () :: TTableHeads (LFst rs) ())
                
                mapM_ (\x -> do
                        -- levBegin
                        tr_ $ toTLF $ (Tagged :: v -> TTableRow rs v) x
                        -- levEnd
                    ) xs

-- read-only table
type family AddNone (a :: [Symbol]) :: [(Symbol,HtmlTag)] where
    AddNone '[] = '[]
    AddNone (a ': as) = '(a,None) ': AddNone as

instance ToTLF (TTable (AddNone rs) vs) => ToTLF (TTableReadOnly rs vs)
  where
    toTLF x = toTLF (retag x :: TTable (AddNone rs) vs)
 
-- composition
type TGroup rss   = TSimple (Group rss)

instance ToTLF (TGroup '[] ()) where
    toTLF _ = mempty
    
instance (ToTLF (TSimple rs vs), ToTLF (TGroup rss vss)) 
    => ToTLF (TGroup (rs ': rss) (vs,vss)) 
  where
    toTLF x = do
        toTLF (retag $ fmap fst x :: TSimple rs vs)
        toTLF (retag $ fmap snd x :: TGroup rss vss)
        
instance ToTLF (TTable rs vs) => ToTLF (TSimple (Tab rs) vs)
  where
    toTLF x = toTLF (retag x :: TSimple (Table rs) vs)

instance ToTLF (TRecAsTab rs vs) => ToTLF (TSimple (Rec rs) vs)
  where
    toTLF x = toTLF (retag x :: TRecAsTab rs vs)
