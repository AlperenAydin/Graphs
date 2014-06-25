{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

import Bars
import TrendGraph
import Date

import Data.Time.Clock

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine



addT a = addUTCTime a

rTF= realToFrac

fun :: UTCTime -> NominalDiffTime -> (UTCTime, Double)
fun t x = (addT (x*d) t , rTF(x^2))

main= do t <- getCurrentTime
         l <- return $  map (fun t) [0, 0.1 .. 10 ] 
         defaultMain ( circle 1 #fc red <>graph l  :: Diagram B R2)

