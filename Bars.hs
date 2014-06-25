{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Bars where

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as M
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text
import Diagrams.TrailLike

--Legend: e= element , f=frequency, c=colour, s=shape

--Type definitions for convenience:
type Shape = Diagram B R2

type C = Colour Double

type Dimensions = ( Double, Double)  

--Convenient Shapes:
hline :: Shape
hline = fromVertices $ map p2 [(-0.5,0),(0,0)] 

vline :: Shape
vline = fromVertices $ map p2 [(0,0.01),(0,-0.5)]

--Functions for convenience:
rt::Num b => Int -> b 
rt = fromIntegral

mx:: [(t,Int, C)] -> Int
mx l = Data.List.maximum $ map (\(e, f, c) -> f) l 

push :: (Double, Double) -> Shape -> Shape
push (x,y) s  = s # translate (r2(x,y))

barNoLines :: Double -> Double -> C -> Shape
barNoLines w h c = (rect w h) # fc c # lw none # push (0,h/2)  <> vline 

write :: Show a => a -> Shape
write t  = text (show t) 

line :: [(Double, Double)] -> Shape
line l = fromVertices $ map p2 l

--The Actual function:
histogramme :: Show t => [(t, Int, C)] -> Dimensions -> Shape
histogramme i (d,w) =
  let bars = foldr barFold mempty i
      h = length i
      coor = foldr coorFold mempty [0,1 ..h]
      background = strutX 
  in bars # push (d,0) <> coor <> line [(0,0),(0,1+ rt h)]
  where
    barFold (_,f,c) s = barNoLines w (rt f) c <> s # push (w+d,0)
    coorFold n s = write n # push (-1,0) <>line [(-0.5,0),(0,0)]<> s # push (0,1)
