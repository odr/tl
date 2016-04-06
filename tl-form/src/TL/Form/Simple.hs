{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TL.Form.Simple(Simple) where

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
type RPTH a       = Tagged '(Simple, Hidden      ) (Maybe a)
type RPTI as a    = Tagged '(Simple, Input as    ) (Maybe a)
type RPTC vs ps a = Tagged '(Simple, Choose vs ps) (Maybe a)

instance ToHtml (Tagged Simple v) =>  ToTLF (Tagged '(Simple, None) v) 
  where
    toTLF x = toHtml (retag x :: Tagged Simple v)

-- Input field (text)
instance {-# OVERLAPPABLE #-} (ToHtmlText a, GetAttrs (Input as))
    => ToTLF (RPTI as a)
  where
    toTLF (Tagged b :: RPTI as a)
        = input_
            . maybe id ((:) . value_ . toHtmlText) b
            $ getAttrs (Proxy :: Proxy (Input as))
            
-- Input field (checkbox)            
instance {-# OVERLAPPING #-} (GetAttrs (Input as))
    => ToTLF (RPTI as Bool)
  where
    toTLF    (Tagged b :: RPTI as Bool)
        = input_
            . (if b == Just True then (checked_ :) else id)
            . (type_ "checkbox" :)
            $ getAttrs (Proxy :: Proxy (Input as))

-- | Input field for numbers
inputNum :: (Num a, GetAttrs (Input as), ToHtmlText a, Monad m)
    => RPTI as a -> MonadTLF m ()
inputNum (Tagged v :: RPTI as a)
    = input_ . maybe id ((:) . value_ . toHtmlText) v
    $ type_ "number" : getAttrs (Proxy :: Proxy (Input as))

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToTLF (RPTI as Int)
  where
    toTLF = inputNum

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToTLF (RPTI as Integer)
  where
    toTLF = inputNum

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToTLF (RPTI as Float)
  where
    toTLF = inputNum

instance {-# OVERLAPPING #-} (GetAttrs (Input as)) => ToTLF (RPTI as Double)
  where
    toTLF = inputNum

------------------- Rendering for Choose value ------------
instance (ToHtmlText a, Names2 vs, GetAttrs (Choose vs ps))
    => ToTLF (RPTC vs ps a)
  where
    toTLF (Tagged x :: RPTC vs ps a)
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
instance ToHtmlText a => ToTLF (RPTH a) where
    toTLF (Tagged v :: RPTH a)
        = input_
            (maybe id ((:) . value_ . toHtmlText) v [type_ "hidden"])

------------------- Non-maybe ---------------------------
type THV (h :: HtmlTag) = Tagged '(Simple, h)

instance {-# OVERLAPPABLE #-} ToTLF (THV h (Maybe v)) => ToTLF (THV h v) where
    toTLF = toTLF . fmap Just

----------------------------------
-- just text
type TN    (n::Symbol)   = Tagged '(Simple,n) ()
-- sequence of <th>
type TNS   (n::[Symbol]) = Tagged '(Simple,n)
-- label with tag
type TNHV  (n :: Symbol) (h :: HtmlTag) = Tagged '(Simple, n, h)
-- row with two fields (label and tag)
type T_NHV (n :: Symbol) (h :: HtmlTag) = Tagged '(Simple, '(n, h))

---------------- Rendering with label -----------------------
instance KnownSymbol n => ToHtml (TN n) where
    toHtml _  = fromString $ symbolVal' (proxy# :: Proxy# n)
    toHtmlRaw = toHtml

instance ToTLF (TNS '[] ()) where
    toTLF _  = return ()

instance (ToHtml (TN n), ToTLF (TNS ns ())) => ToTLF (TNS (n ': ns) ()) where
    toTLF _ = do
        withName tn $ th_ tn
        toTLF (Tagged () :: TNS ns ())
      where
        tn :: (ToHtml (TN n), Monad m) => MonadTLF m ()
        tn = toHtml (Tagged () :: TN n)

labeledHtml :: (ToTLF (THV h v), ToHtml (TN n), Monad m)
            => TNHV n h v -> MonadTLF m ()
labeledHtml (x :: TNHV n h v) = withName tn $
    label_ $ do
        tn >> ": "
        toTLF (retag x :: THV h v)
  where
    tn :: (ToHtml (TN n), Monad m) => MonadTLF m ()
    tn = toHtml (Tagged () :: TN n)
         

instance {-# OVERLAPPABLE #-} (ToTLF (THV h v), ToHtml (TN n))
    => ToTLF (TNHV n h v)
  where
    toTLF    = labeledHtml

instance {-# OVERLAPPING #-} (ToTLF (Tagged '(Simple, Hidden) v), ToHtml (TN n))
    => ToTLF (Tagged '(Simple, (n :: Symbol), Hidden) v)
  where
    toTLF x = labeledHtml x `with` [hidden_ ""]

---------------- Rendering as table raw -----------------------
rowHtml :: (ToTLF (THV h v), ToHtml (TN n), Monad m) => T_NHV n h v -> MonadTLF m ()
rowHtml (x :: T_NHV n h v) = withName tn $ tr_ $ do
    td_ $ tn >> ": "
    td_ $ toTLF (retag x :: THV h v)
  where
    tn :: (ToHtml (TN n), Monad m) => MonadTLF m ()
    tn = toHtml (Tagged () :: TN n)

instance {-# OVERLAPPABLE #-} (ToTLF (THV h v), ToHtml (TN n))
    => ToTLF (T_NHV n h v)
  where
    toTLF = rowHtml

instance {-# OVERLAPPING #-}
    (ToTLF (Tagged '(Simple, Hidden) v), ToHtml (TN n))
    => ToTLF (Tagged '(Simple, '((n :: Symbol), Hidden)) v)
  where
    toTLF x = rowHtml x `with` [hidden_ ""]

----------------- Rendering table -------------------------------
-- editable record (as two-column table: label - input)
type TRS  (rs :: [(Symbol, HtmlTag)]) v = Tagged '(Simple, rs) (Maybe v)
-- editable record internal (as two-column table: label - input)
type TRSF (rs :: [(Symbol, HtmlTag)]) v = Tagged '(Simple, False, rs) (Maybe v)
-- editable table 
type TRSS (rs :: [(Symbol, HtmlTag)]) v = Tagged '(Simple, rs) [v]
-- editable table row
type THS  (rs :: [(Symbol, HtmlTag)]) = Tagged '(Simple, True, rs)

instance ToTLF (TRS '[] ())
  where
    toTLF    _ = return ()

instance ToTLF (TRSF '[] ())
  where
    toTLF    _ = return ()

instance ( ToTLF (T_NHV n h (Maybe v)), ToTLF (TRSF rs vs))
    => ToTLF (TRSF ('(n, h) ': rs) (v,vs))
  where
    toTLF x = do
        toTLF (retag $ fmap (fmap fst) x :: T_NHV n h (Maybe v))
        toTLF (retag $ fmap (fmap snd) x :: TRSF rs vs)

instance {-# OVERLAPPING #-} ToTLF (TRSF (r ': rs) xs) => ToTLF (TRS (r ': rs) xs)
  where
    toTLF zs = do
        table_ $ toTLF (retag zs :: TRSF (r ': rs) xs)

instance {-# OVERLAPPABLE #-} ToTLF (TRS rs xs) => ToTLF (Tagged '(Simple, rs) xs)
  where
    toTLF    = toTLF . fmap Just

instance ToTLF (THS '[] ()) where
    toTLF _ = return ()

instance (ToTLF (THV r v), ToTLF (THS rs vs), ToHtml (TN n)) 
    => ToTLF (THS ( '(n,r) ': rs) (v,vs))
  where
    toTLF x = do
        withName tn $ td_ $ toTLF (retag $ fmap fst x :: THV r v)
        toTLF (retag $ fmap snd x :: THS rs vs)
      where
        tn :: (ToHtml (TN n), Monad m) => MonadTLF m ()
        tn = toHtml (Tagged () :: TN n)

instance (ToTLF (TNS (LFst rs) ()), ToTLF (THS rs v)) => ToTLF (TRSS rs v)
  where
    toTLF (Tagged xs)
        = do
            table_ $ do
                tr_ $ toTLF (Tagged () :: TNS (LFst rs) ())
                
                mapM_ (\x -> do
                        -- levBegin
                        tr_ $ toTLF $ (Tagged :: v -> THS rs v) x
                        -- levEnd
                    ) xs

-- read-only table
instance ToTLF (Tagged '(Simple, ZipK2 rs None) [v]) => ToTLF (TNS rs [v])
  where
    toTLF x = toTLF (retag x :: Tagged '(Simple, ZipK2 rs None) [v])
 
-- composition
instance ToTLF (Tagged '(Simple, ('[]::[[(Symbol,HtmlTag)]])) ()) where
    toTLF _ = mempty
    
instance (ToTLF (Tagged '(Simple, rs) vs), ToTLF (Tagged '(Simple, rss) vss)) 
    => ToTLF (Tagged '(Simple, rs ': (rss  :: [[(Symbol, HtmlTag)]])) (vs,vss)) 
  where
    toTLF x = do
        toTLF (retag $ fmap fst x :: Tagged '(Simple, rs) vs)
        toTLF (retag $ fmap snd x :: Tagged '(Simple, rss) vss)
        
instance ToTLF (Tagged '(Simple, (rs :: [(Symbol, HtmlTag)])) v)
    => ToTLF (Tagged '(Simple, Tab rs) v)
  where
    toTLF x = toTLF (retag x :: Tagged '(Simple, (rs :: [(Symbol, HtmlTag)])) v)
