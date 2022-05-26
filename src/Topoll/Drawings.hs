{-# LANGUAGE OverloadedStrings #-}

module Topoll.Drawings where

import Data.List (dropWhileEnd)
import Graphics.Vega.VegaLite
import Topoll.ChainComplex.Field
import qualified Data.Text as T

drawPersistentHomologies :: (Float -> [BettiNumber]) -> ([BettiNumber] -> Bool) -> Float -> Int -> VegaLite
drawPersistentHomologies calcHomologies coloringCond step n =
  let homologiesData = map (\r -> (calcHomologies r, r)) $ take n $ enumFromThen 0 step
      h =
        concat $
          [ zipWith
                  ( \bn hn ->
                      dataRow
                        [("r", Number $ realToFrac r),
                         ("b", Number $ realToFrac bn),
                         ("h", Str  $ "H_" <> T.pack (show hn)),
                         ("c", Str $ if coloringCond bns then "1" else  "0")
                         ]
                  )
                  bns
                  [0 :: Integer ..]
            | (bns, r) <- homologiesData
          ]

      d = dataFromRows [] . foldr (.) id h
      enc =
        encoding
          . position X [PName "r", PmType Quantitative, PAxis [AxTickCount n]]
          . position Y [PName "b", PmType Quantitative, PAxis [AxTitleAngle 0, AxTitlePadding 10, AxTickCount (fromIntegral $ maximum $ concatMap fst homologiesData)]]
          . column [FName "h", FmType Nominal, FNoTitle]
          . color [MName "c", MLegend [], MScale [SRange (RStrings ["blue","red"])]]
   in toVegaLite
        [ d [],
          mark Point [],
          enc []
        ]

eqSphere :: [BettiNumber] -> Bool
eqSphere xs = dropWhileEnd (== 0) xs == [1, 0, 1]

eqTorus :: [BettiNumber] -> Bool
eqTorus xs = dropWhileEnd (== 0) xs == [1, 2, 1]
