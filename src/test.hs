{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Point = P1 { _x :: Float, _y :: Float } | P2 { _z :: Int, _w :: Int }

$(makeLenses ''Point)
