module EvalUtil
  ( evalPoint,
  )
where

import InterpolationUtil hiding (name, step, window)
import PointUtil
import SlidingWindowUtil (addElement)

-- | Применяем функцию интерполяции
evalPoint :: String -> Interpolation -> (Interpolation, Maybe String)
evalPoint str (Interpolation name window intFunc step) = (Interpolation name newWindow intFunc step, result)
    where
        point = mapPoint str -- Преобразуем строку в точку
        newWindow = addElement point window -- Добавляем точку в окно
        result = case showPoints (intFunc newWindow) of -- Применяем функцию к окну и преобразуем точки в строку
            Just s  -> Just (name ++ " interpolation: \n" ++ s) -- Если удалось преобразовать в строку
            Nothing -> Nothing
