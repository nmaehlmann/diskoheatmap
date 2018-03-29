{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.Reader
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Diagrams.Backend.SVG
import Diagrams.Prelude
import qualified Options as Opt


data HeatmapOptions = HeatmapOptions 
    { optOutfile :: FilePath
    , optClusterRadius :: Double
    , optMaxTemperature :: Int
    }

instance Opt.Options HeatmapOptions where
    defineOptions = HeatmapOptions
        <$> Opt.simpleOption "out" "out.svg" "File to save the generated heatmap."
        <*> Opt.simpleOption "clusterRadius" 0.1 "Radius of a circle which is used to cluster and display positions."
        <*> Opt.simpleOption "maxTemperature" 10 "Maximal number of positions in a cluster."

main = Opt.runCommand $ \opts (filename:_) -> do
    content <- readFile filename
    let positions = lines content
        positionsMap = runReader (collectPositions positions) opts
        positionCircles = map (\(p, n) -> runReader (createCircle p n) opts) (Map.toList positionsMap)
    renderSVG (optOutfile opts) (mkWidth 1000) (mergeDiagramms positionCircles)

collectPositions :: [String] -> Reader HeatmapOptions (Map.Map (Int, Int) Int)
collectPositions [] = return Map.empty
collectPositions (p:ps) = do
    previousMap <- collectPositions ps
    let [x, y] = splitOn "," p
    convertedX <- processPosition x
    convertedY <- processPosition y
    let convertedPos = (convertedX, convertedY)
        currentAmount = fromMaybe 0 (Map.lookup convertedPos previousMap)
    return $ Map.insert convertedPos (currentAmount + 1) previousMap

processPosition :: String -> Reader HeatmapOptions Int
processPosition s = do
    clusterRadius <- reader optClusterRadius
    return $ round (read s / (clusterRadius * 2))

createCircle :: (Int, Int) -> Int -> Reader HeatmapOptions (Diagram B)
createCircle (x, y) n = do
    circleRadius <- reader optClusterRadius
    calcOpacity <- calculateOpacity n
    let xPos = fromIntegral x * circleRadius * 2
        yPos = fromIntegral y * circleRadius * 2
    return $ circle circleRadius
        # translate (r2 (xPos, yPos)) 
        # lw 0 
        # fc (sRGB24 0 0 0) 
        # opacity calcOpacity

mergeDiagramms :: [Diagram B] -> Diagram B
mergeDiagramms = foldl1 atop

calculateOpacity :: Int -> Reader HeatmapOptions Double
calculateOpacity n = do
    maxTemperature <- reader optMaxTemperature
    return $ 0.1 + (0.9 / fromIntegral maxTemperature) * fromIntegral n