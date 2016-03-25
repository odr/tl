{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module TL.Closed where

import TL.Types
import TL.Plain
type family VRec (rep:: *) (a :: [(k,*)]) :: * where
    VRec Plain '[]              = ()
    VRec Plain '[ '(a,b)]       = (b,())
    VRec Plain ('(a,b) ': xs)   = (b, VRec Plain xs)

type family VRec'' (rep :: *) (a :: *) :: [*] where
    VRec'' Plain () = '[]
    VRec'' Plain (a,b) = a ': VRec'' Plain b

