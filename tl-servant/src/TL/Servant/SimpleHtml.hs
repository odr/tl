{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module TL.Servant.SimpleHtml where

import           Data.Aeson
import           Data.Bifunctor (second)
import           Data.Tagged
import           Servant.Docs   (ToSample (..))

import           TL.Form.Simple
import           TL.Form.Types
import           TL.Pers.DDL
import           TL.Plain
import           TL.Types

data SimpleHtml
type TSimpleHtml = Tagged SimpleHtml

instance ToJSON a => ToJSON (TSimpleHtml a) where
    toJSON = toJSON . untag

instance (RepRecord Plain t ar
        , ToTLF (TRecAsTab (ZipK2 (LFst (RecordDef t)) ('Input '[])) (Maybe ar))
        ) => ToTLF (TSimpleHtml (Tagged '(Plain, (t::DataDef *)) (Maybe ar)))
  where
    toTLF = toTLF
        . (retag :: Tagged '(Plain, t) (Maybe ar)
                -> TRecAsTab (ZipK2 (LFst (RecordDef t)) ('Input '[])) (Maybe ar)
            )
        . untag

instance (RepRecord Plain t ar
        , ToTLF (TTableReadOnly (LFst (RecordDef t)) [ar])
        ) => ToTLF (TSimpleHtml (Tagged '(Plain, (t::DataDef *)) [ar]))
  where
    toTLF = toTLF
        . (retag :: Tagged '(Plain, t) [ar]
                -> TTableReadOnly (LFst (RecordDef t)) [ar]
            )
        . untag

instance ToSample a b => ToSample (Tagged SimpleHtml a) (Tagged SimpleHtml b)
  where
    toSample = fmap Tagged . toSample . fmap untag
    toSamples = fmap (second Tagged) . toSamples . fmap untag
