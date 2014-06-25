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
fun t x = (addT (x*d) t , rTF x)

main= do t <- getCurrentTime
         l <- return $  map (fun t) [0, 1 .. 100 ] 
         defaultMain ( graph l # pad 1.1 :: Diagram B R2)

