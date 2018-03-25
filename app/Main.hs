{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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
    let positionsMap = collectPositions opts positions
    let positionCircles = map (\(p, n) -> createCircle opts p n) (Map.toList positionsMap)
    renderSVG (optOutfile opts) (mkWidth 1000) (mergeDiagramms positionCircles)

collectPositions :: HeatmapOptions -> [String] -> Map.Map (Int, Int) Int
collectPositions _ [] = Map.empty
collectPositions options (p:ps) = Map.insert convertedPos (currentAmount + 1) previousMap
  where
    currentAmount = fromMaybe 0 (Map.lookup convertedPos previousMap)
    previousMap = collectPositions options ps
    convertedPos = (processPosition options x, processPosition options y)
      where
        [x, y] = splitOn "," p

processPosition :: HeatmapOptions -> String -> Int
processPosition options s = round (read s / (optClusterRadius options * 2))

createCircle :: HeatmapOptions -> (Int, Int) -> Int -> Diagram B
createCircle options (x, y) n =
    circle circleRadius
        # translate (r2 (xPos, yPos)) 
        # lw 0 
        # fc (sRGB24 0 0 0) 
        # opacity (calculateOpacity options n)
  where
    circleRadius = optClusterRadius options
    xPos = fromIntegral x * circleRadius * 2
    yPos = fromIntegral y * circleRadius * 2

mergeDiagramms :: [Diagram B] -> Diagram B
mergeDiagramms = foldl1 atop

calculateOpacity :: HeatmapOptions -> Int -> Double
calculateOpacity options n = 0.1 + (0.9 / fromIntegral (optMaxTemperature options)) * fromIntegral n