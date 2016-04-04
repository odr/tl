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

import TL.Types(Names2(..), LFst, LSnd)
import TL.Form.Types

-- | Style of simple rendering.
data Simple

--------------- output-only values ------------------
instance {-# OVERLAPPABLE #-} ToHtmlText a => ToTLF (Tagged Simple a) where
    toTLF = toHtml . untag . fmap toHtmlText

instance {-# OVERLAPPING #-} ToTLF (Tagged Simple Bool) where
    toTLF (Tagged b) = toHtml (if b then "+" else "-" :: T.Text)

instance {-# OVERLAPPING #-} (ToTLF (Tagged Simple a))
    => ToTLF (Tagged Simple (Maybe a))
  where
    toTLF = toTLF . sequence

instance {-# OVERLAPPING #-} (ToTLF (Tagged Simple a), ToTLF (Tagged Simple as))
    => ToTLF (Tagged Simple (a,as))
  where
    toTLF x = do
        td_ $ toTLF $ fmap fst x
        toTLF $ fmap snd x

instance ToTLF (Tagged Simple a) => ToTLF (Maybe (Tagged Simple a))
  where
    toTLF = maybe (toHtml (""::T.Text)) toTLF

----------------------- Rendering for Input --------------
type RPTH a       = Tagged '(Simple, Hidden      ) (Maybe a)
type RPTI as a    = Tagged '(Simple, Input as    ) (Maybe a)
type RPTC vs ps a = Tagged '(Simple, Choose vs ps) (Maybe a)

instance ToHtml v =>  ToTLF (Tagged '(Simple, None) v) 
  where
    toTLF = toHtml . untag

instance {-# OVERLAPPABLE #-} (ToHtmlText a, GetAttrs (Input as))
    => ToTLF (RPTI as a)
  where
    toTLF (Tagged b :: RPTI as a)
        = getId >>= \t -> input_
            . maybe id ((:) . value_ . toHtmlText) b
            . (id_ t :)
            $ getAttrs (Proxy :: Proxy (Input as))
instance {-# OVERLAPPING #-} (GetAttrs (Input as))
    => ToTLF (RPTI as Bool)
  where
    toTLF    (Tagged b :: RPTI as Bool)
        = getId >>= \t -> input_
            . (if b == Just True then (checked_ :) else id)
            . (id_ t :)
            . (type_ "checkbox" :)
            $ getAttrs (Proxy :: Proxy (Input as))

inputNum :: (Num a, GetAttrs (Input as), ToHtmlText a, Monad m)
    => RPTI as a -> MonadTLF m ()
inputNum (Tagged v :: RPTI as a)
    = getId >>= \t -> input_ . maybe id ((:) . value_ . toHtmlText) v
    $ id_ t : type_ "number" : getAttrs (Proxy :: Proxy (Input as))

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
        = getId >>= \t -> select_
            (mapM_  (\(v,n) -> with (option_ $ fromString n)
                                    (f v [value_ (fromString v)])
                    ) $ names2 (proxy# :: Proxy# vs)
            ) `with`
            ( id_ t : getAttrs (Proxy :: Proxy (Choose vs ps)))
      where
        val = fmap toHtmlText x
        f v | val == Just (T.pack v)    = (selected_ "" :)
            | otherwise                 = id

------------------- Rendering for Hidden value ------------
instance ToHtmlText a => ToTLF (RPTH a) where
    toTLF (Tagged v :: RPTH a)
        = getId >>= \t -> input_
            (maybe id ((:) . value_ . toHtmlText) v [id_ t, type_ "hidden"])

------------------- Non-maybe ---------------------------
type THV (h :: HtmlTag) = Tagged '(Simple, h)

instance {-# OVERLAPPABLE #-} ToTLF (THV h (Maybe v)) => ToTLF (THV h v) where
    toTLF = toTLF . fmap Just

----------------------------------

type TN    (n::Symbol)   = Tagged '(Simple,n) ()
type TNS   (n::[Symbol]) = Tagged '(Simple,n)
type TNHV  (n :: Symbol) (h :: HtmlTag) = Tagged '(Simple, n, h)
type T_NHV (n :: Symbol) (h :: HtmlTag) = Tagged '(Simple, '(n, h))

---------------- Rendering with label -----------------------
instance KnownSymbol n => ToTLF (TN n) where
    toTLF _  = fromString $ symbolVal' (proxy# :: Proxy# n)

instance ToTLF (TNS '[] ()) where
    toTLF _  = return ()

instance (ToTLF (TN n), ToTLF (TNS ns ())) => ToTLF (TNS (n ': ns) ()) where
    toTLF _ = do
        th_ $ toTLF (Tagged () :: TN n)
        toTLF (Tagged () :: TNS ns ())

labeledHtml :: (ToTLF (THV h v), ToTLF (TN n), Monad m)
            => TNHV n h v -> MonadTLF m ()
labeledHtml (x :: TNHV n h v) = label_ $ do
    toTLF (Tagged () :: TN n) >> ": "
    toTLF (retag x :: THV h v)

instance {-# OVERLAPPABLE #-} (ToTLF (THV h v), ToTLF (TN n))
    => ToTLF (TNHV n h v)
  where
    toTLF    = labeledHtml

instance {-# OVERLAPPING #-} (ToTLF (Tagged '(Simple, Hidden) v), ToTLF (TN n))
    => ToTLF (Tagged '(Simple, (n :: Symbol), Hidden) v)
  where
    toTLF x = labeledHtml x `with` [hidden_ ""]

---------------- Rendering as table raw -----------------------
rowHtml :: (ToTLF (THV h v), ToTLF (TN n), Monad m) => T_NHV n h v -> MonadTLF m ()
rowHtml (x :: T_NHV n h v) = tr_ $ do
    td_ $ toTLF (Tagged () :: TN n) >> ": "
    td_ $ toTLF (retag x :: THV h v)

instance {-# OVERLAPPABLE #-} (ToTLF (THV h v), ToTLF (TN n))
    => ToTLF (T_NHV n h v)
  where
    toTLF = rowHtml

instance {-# OVERLAPPING #-}
    (ToTLF (Tagged '(Simple, Hidden) v), ToTLF (TN n))
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
type THS  (rs :: [HtmlTag]) = Tagged '(Simple, rs)

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

instance (ToTLF (THV r v), ToTLF (THS rs vs)) => ToTLF (THS (r ': rs) (v,vs))
  where
    toTLF x = do
        td_ $ toTLF (retag $ fmap fst x :: THV r v)
        toTLF (retag $ fmap snd x :: THS rs vs)

instance (ToTLF (TNS (LFst rs) ()), ToTLF (THS (LSnd rs) v)) => ToTLF (TRSS rs v)
  where
    toTLF (Tagged xs)
        = do
            table_ $ do
                tr_ $ toTLF (Tagged () :: TNS (LFst rs) ())
                mapM_ (\x -> do
                        tr_ $ toTLF $ (Tagged :: v -> THS (LSnd rs) v) x
                    ) xs

instance (ToTLF (TNS rs ()), ToTLF (Tagged Simple v)) => ToTLF (TNS rs [v])
  where
    toTLF (Tagged x)
        = table_ $ do
            tr_ $ toTLF (Tagged () :: TNS rs ())
            mapM_ (tr_ . toTLF . (Tagged :: v -> Tagged Simple v)) x

instance ToTLF (Tagged '(Simple, ('[]::[[(Symbol,HtmlTag)]])) ()) where
    toTLF _ = mempty
    
instance (ToTLF (Tagged '(Simple, rs) vs), ToTLF (Tagged '(Simple, rss) vss)) 
    => ToTLF (Tagged '(Simple, rs ': (rss  :: [[(Symbol, HtmlTag)]])) (vs,vss)) 
  where
    toTLF x = do
        toTLF (retag $ fmap fst x :: Tagged '(Simple, rs) vs)
        toTLF (retag $ fmap snd x :: Tagged '(Simple, rss) vss)
        
