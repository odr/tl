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

instance {-# OVERLAPPING #-} (ToHtml (Tagged Simple a), ToHtml (Tagged Simple as))
    => ToHtml (Tagged Simple (a,as))
  where
    toHtml x = do
        td_ $ toHtml $ fmap fst x
        toHtml $ fmap snd x
    toHtmlRaw = toHtml

instance ToHtml (Tagged Simple a) => ToHtml (Maybe (Tagged Simple a))
  where
    toHtml    = maybe (toHtml    (""::T.Text)) toHtml
    toHtmlRaw = maybe (toHtmlRaw (""::T.Text)) toHtmlRaw

----------------------- Rendering for Input --------------
type RPTH (sid::[Nat]) a       = Tagged '(Simple, sid, Hidden      ) (Maybe a)
type RPTI (sid::[Nat]) as a    = Tagged '(Simple, sid, Input as    ) (Maybe a)
type RPTC (sid::[Nat]) vs ps a = Tagged '(Simple, sid, Choose vs ps) (Maybe a)

instance {-# OVERLAPPABLE #-} (ToHtmlText a, GetAttrs (Input as), GetId sid)
    => ToHtml (RPTI sid as a)
  where
    toHtml    (Tagged b :: RPTI sid as a)
        = input_
            . maybe id ((:) . value_ . toHtmlText) b
            . (id_ (getId' (proxy# :: Proxy# sid)) :)
            $ getAttrs (Proxy :: Proxy (Input as))
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (GetAttrs (Input as), GetId sid)
    => ToHtml (RPTI sid as Bool)
  where
    toHtml    (Tagged b :: RPTI sid as Bool)
        = input_
            . (if b == Just True then (checked_ :) else id)
            . (id_ (getId' (proxy# :: Proxy# sid)) :)
            . (type_ "checkbox" :)
            $ getAttrs (Proxy :: Proxy (Input as))
    toHtmlRaw = toHtml

inputNum :: (Num a, GetAttrs as, ToHtmlText a, Monad m, GetId sid)
    => Proxy# sid -> Proxy as -> (Maybe a) -> HtmlT m ()
inputNum s p v
    = input_ . maybe id ((:) . value_ . toHtmlText) v
    $ id_ (getId' s) : type_ "number" : getAttrs p

instance {-# OVERLAPPING #-} (GetAttrs (Input as), GetId sid)
    => ToHtml (RPTI sid as Int)
  where
    toHtml (Tagged b :: RPTI sid as Int)
        = inputNum (proxy# :: Proxy# sid) (Proxy :: Proxy (Input as)) b
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (GetAttrs (Input as), GetId sid)
    => ToHtml (RPTI sid as Integer)
  where
    toHtml (Tagged b :: RPTI sid as Integer)
        = inputNum (proxy# :: Proxy# sid) (Proxy :: Proxy (Input as)) b
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (GetAttrs (Input as), GetId sid)
    => ToHtml (RPTI sid as Float)
  where
    toHtml (Tagged b :: RPTI sid as Float)
        = inputNum (proxy# :: Proxy# sid) (Proxy :: Proxy (Input as)) b
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (GetAttrs (Input as), GetId sid)
    => ToHtml (RPTI sid as Double)
  where
    toHtml (Tagged b :: RPTI sid as Double)
        = inputNum (proxy# :: Proxy# sid) (Proxy :: Proxy (Input as)) b
    toHtmlRaw = toHtml

------------------- Rendering for Choose value ------------
instance (ToHtmlText a, Names2 vs, GetAttrs (Choose vs ps), GetId sid)
    => ToHtml (RPTC sid vs ps a)
  where
    toHtml (Tagged x :: RPTC sid vs ps a)
        = select_
            (mapM_  (\(v,n) -> with (option_ $ fromString n)
                                    (f v [value_ (fromString v)])
                    ) $ names2 (proxy# :: Proxy# vs)
            ) `with`
            ( id_ (getId' (proxy# :: Proxy# sid))
            : getAttrs (Proxy :: Proxy (Choose vs ps))
            )
      where
        val = fmap toHtmlText x
        f v | val == Just (T.pack v)    = (selected_ "" :)
            | otherwise                 = id
    toHtmlRaw = toHtml

------------------- Rendering for Hidden value ------------
instance (ToHtmlText a, GetId sid) => ToHtml (RPTH sid a) where
    toHtml (Tagged v :: RPTH sid a)
        = input_ $ maybe id ((:) . value_ . toHtmlText) v
        [ id_ (getId' (proxy# :: Proxy# sid))
        , type_ "hidden"
        ]
    toHtmlRaw = toHtml

------------------- Non-maybe ---------------------------
type THV (sid::[Nat]) (h :: HtmlTag) = Tagged '(Simple, sid, h)

instance {-# OVERLAPPABLE #-} ToHtml (THV sid h (Maybe v)) => ToHtml (THV sid h v) where
    toHtml = toHtml . fmap Just
    toHtmlRaw = toHtml

----------------------------------

type TN    (n::Symbol)   = Tagged '(Simple,n) ()
type TNS   (n::[Symbol]) = Tagged '(Simple,n)
type TNHV  (sid::[Nat]) (n :: Symbol) (h :: HtmlTag) = Tagged '(Simple, sid, n, h)
type T_NHV (sid::[Nat]) (n :: Symbol) (h :: HtmlTag) = Tagged '(Simple, sid, '(n, h))

---------------- Rendering with label -----------------------
instance KnownSymbol n => ToHtml (TN n) where
    toHtml _  = fromString $ symbolVal' (proxy# :: Proxy# n)
    toHtmlRaw = toHtml

instance ToHtml (TNS '[] ()) where
    toHtml _  = return ()
    toHtmlRaw = toHtml

instance (ToHtml (TN n), ToHtml (TNS ns ())) => ToHtml (TNS (n ': ns) ()) where
    toHtml _ = do
        th_ $ toHtml (Tagged () :: TN n)
        toHtml (Tagged () :: TNS ns ())
    toHtmlRaw = toHtml

labeledHtml :: (ToHtml (THV sid h v), ToHtml (TN n), Monad m)
            => TNHV sid n h v -> HtmlT m ()
labeledHtml (x :: TNHV sid n h v) = label_ $ do
    toHtml (Tagged () :: TN n) >> ": "
    toHtml (retag x :: THV sid h v)

instance {-# OVERLAPPABLE #-} (ToHtml (THV sid h v), ToHtml (TN n))
    => ToHtml (TNHV sid n h v)
  where
    toHtml    = labeledHtml
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} (ToHtml (Tagged '(Simple, sid, Hidden) v), ToHtml (TN n))
    => ToHtml (Tagged '(Simple, (sid :: [Nat]), (n :: Symbol), Hidden) v)
  where
    toHtml x = labeledHtml x `with` [hidden_ ""]
    toHtmlRaw = toHtml

---------------- Rendering as table raw -----------------------
rowHtml :: (ToHtml (THV sid h v), ToHtml (TN n), Monad m) => T_NHV sid n h v -> HtmlT m ()
rowHtml (x :: T_NHV sid n h v) = tr_ $ do
    td_ $ toHtml (Tagged () :: TN n) >> ": "
    td_ $ toHtml (retag x :: THV sid h v)

instance {-# OVERLAPPABLE #-} (ToHtml (THV sid h v), ToHtml (TN n))
    => ToHtml (T_NHV sid n h v)
  where
    toHtml    = rowHtml
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-}
    (ToHtml (Tagged '(Simple, sid, Hidden) v), ToHtml (TN n))
    => ToHtml (Tagged '(Simple, (sid :: [Nat]), '((n :: Symbol), Hidden)) v)
  where
    toHtml x  = rowHtml x `with` [hidden_ ""]
    toHtmlRaw = toHtml

----------------- Rendering table -------------------------------
type TRS  (sid::[Nat]) (rs :: [(Symbol, HtmlTag)]) v
    = Tagged '(Simple, sid, rs) (Maybe v)
type TRSF (sid::[Nat]) (rs :: [(Symbol, HtmlTag)]) v
    = Tagged '(Simple, sid, False, rs) (Maybe v)
type TRSS (sid::[Nat]) (rs :: [(Symbol, HtmlTag)]) v
    = Tagged '(Simple, sid, rs) [v]
type THS  (sid::[Nat]) (rs :: [HtmlTag]) = Tagged '(Simple, sid, rs)

instance ToHtml (TRS sid '[] ())
  where
    toHtml    _ = return ()
    toHtmlRaw _ = return ()

instance ToHtml (TRSF sid '[] ())
  where
    toHtml    _ = return ()
    toHtmlRaw _ = return ()

instance ( ToHtml (T_NHV (sid ': ids) n h (Maybe v))
         , ToHtml (TRSF (sid+1 ': ids) rs vs))
    => ToHtml (TRSF (sid ': ids) ('(n, h) ': rs) (v,vs))
  where
    toHtml x = do
        toHtml (retag $ fmap (fmap fst) x :: T_NHV (sid ': ids) n h (Maybe v))
        toHtml (retag $ fmap (fmap snd) x :: TRSF (sid+1 ': ids) rs vs)
    toHtmlRaw = toHtml

instance {-# OVERLAPPING #-} ToHtml (TRSF (1 ': sid) (r ': rs) (x,xs))
    => ToHtml (TRS sid (r ': rs) (x,xs))
  where
    toHtml zs = table_ $ toHtml (retag zs :: TRSF (1 ': sid) (r ': rs) (x,xs))
    toHtmlRaw = toHtml

instance {-# OVERLAPPABLE #-} ToHtml (TRS sid rs xs)
    => ToHtml (Tagged '(Simple, sid, rs) xs)
  where
    toHtml    = toHtml . fmap Just
    toHtmlRaw = toHtml

instance ToHtml (THS sid '[] ()) where
    toHtml _ = return ()
    toHtmlRaw = toHtml

instance (ToHtml (THV (sid ': ids) r v), ToHtml (THS (sid+1 ': ids) rs vs))
    => ToHtml (THS (sid ': ids) (r ': rs) (v,vs))
  where
    toHtml x = do
        td_ $ toHtml (retag $ fmap fst x :: THV (sid ': ids) r v)
        toHtml (retag $ fmap snd x :: THS (sid+1 ': ids) rs vs)
    toHtmlRaw = toHtml

instance (ToHtml (TNS (LFst rs) ()), ToHtml (THS (1 ': sid) (LSnd rs) v))
    => ToHtml (TRSS sid rs v)
  where
    toHtml (Tagged x)
        = table_ $ do
            tr_ $ toHtml (Tagged () :: TNS (LFst rs) ())
            -- !!! no mapM_ ! row id should be changed! Alas, probably, we need State monad...
            mapM_ (tr_ . toHtml . (Tagged :: v -> THS (1 ': sid) (LSnd rs) v)) x
    toHtmlRaw = toHtml

instance (ToHtml (TNS rs ()), ToHtml (Tagged Simple v)) => ToHtml (TNS rs [v])
  where
    toHtml (Tagged x)
        = table_ $ do
            tr_ $ toHtml (Tagged () :: TNS rs ())
            mapM_ (tr_ . toHtml . (Tagged :: v -> Tagged Simple v)) x
    toHtmlRaw = toHtml
