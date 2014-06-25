{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module TrendGraph where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text
import Diagrams.TrailLike

import Data.Time.Clock
import Data.List

import Control.Concurrent
import Percentage
import Date

type Di = Diagram B R2

type Trend = (UTCTime, Double)

type UTime = UTCTime

type NominalTime = NominalDiffTime

--Too lazy
diffTime = diffUTCTime

push :: Double -> Double -> Di -> Di
push x y = translate (r2(x,y))

write = text.show

draw :: [(Double,Double)] -> Di
draw l = fromVertices $ map p2 l

timeToDouble :: NominalDiffTime -> Double
timeToDouble a = realToFrac a

tTD = timeToDouble

fi=fromIntegral

addt=addUTCTime

--Orders the data set:

order :: [Trend] -> [Trend]
order l = sortBy (\ (t1,i1) (t2,i2) -> compare t1 t2 ) l

--Avarage difference in time in a list
sumdiff :: [NominalDiffTime] -> NominalDiffTime
sumdiff [] = 0
sumdiff (_:[]) = 0
sumdiff (x1:x2:xs) = (x2-x1)+sumdiff(x2:xs)

avaragediff :: [UTCTime] -> NominalDiffTime
avaragediff t = sumdiff l / (genericLength l)
  where l = map (\ x -> diffTime x (head t)) t

--Transform a [UTCTime,Double] list into a list of points
trendToPoints :: [Trend] -> [P2]
trendToPoints t =
  let t'= order t
      
      firstDay = fst $ head t'
      l = let fromFirst (time, i) = ( diffTime time firstDay, i)
          in map fromFirst t'

      meandiff = (avaragediff $ map (\ (x,_) -> x) t')
             
      l'= map (\ (time, i) -> ( tTD (time/meandiff), i)) l
             
      in map p2 l'

hline :: Di
hline = draw [(-0.5,0),(0,0)]

vline :: Di
vline = draw [(0,0),(0,0.5)]

--TrendGraph


graph :: [Trend] -> Di
graph t =  position (zip l (repeat dot))<>mconcat[lines,yaxis,co,xaxis,bckgrnd]
  where
        -- The list of points which is the used
        --to create the lines and position the dots
        l = trendToPoints t 
        lines = fromVertices l #lc blue 
        dot = circle 0.2 # fc black
        --The axis:
        yaxis = draw [(0,0),(0,100)]
        ymark n  = hline <>  write (mkPercent n) # push (-3) 0
        co = position( map (\n -> (p2(0,n),ymark n)) [0, 20.0 .. 100.0])
        --The x-axis: Only has some values:
        lnth=length l

        xpos = let t' = trendToPoints $ map (\ (x,_) -> (x, 0)) t
                   f x xs = if x `mod` 10 ==0 then (t'!!x):xs else xs
               in foldr f [] [0..lnth]
        xmarks = let f x xs = if x `mod` 10 ==0 then (t!!x):xs else xs
                     t' = foldr f [] [0..lnth]
                     mark n = vline <> write (utcToDate n) # push 0 (-2)
                 in map (\ (time,i) -> mark time) t'
        xaxis = position (zip xpos xmarks)

        --The Background:
        bckgrnd= strutY 5 #push 0 (-5) <> strutX 5 # push (-5) 0
                 <> strutY 5 # push 0 100 <> sturX 5 # push l 0

        

        
