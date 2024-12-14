module PointUtil
  ( Point,
    mapPoint,
    showPoints,
  )
where

import Text.Printf

type Point = (Double, Double)

-- | Преобразует строку в точку
mapPoint :: String -> Point
mapPoint input = (read x, read y)
  where
    (x, y) = case words input of
      [xStr, yStr] -> (xStr, yStr)
      _ -> error "Invalid input"

-- Функция преобразует число в строку с точностью 2 знака после запятой
formatNumber :: Double -> String
formatNumber = printf "%.2f"

-- Функция преобразут список точек в строку
showPoints :: Maybe [Point] -> Maybe String
showPoints Nothing = Nothing
showPoints (Just points) = Just (unlines [unwords xCoords, unwords yCoords])
  where
    -- Разделим точки по x и y на два списка
    (xs, ys) = unzip points
    -- Преобразуем координаты в строки
    xCoords = map formatNumber xs
    yCoords = map formatNumber ys
