{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Point = P { _x :: Float, _y :: Float }

$(makeLenses ''Point)
