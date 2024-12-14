module REPLUtil
  ( read_,
    eval_,
    print_,
    loop_,
  )
where

import Control.Monad (unless)
import Data.Maybe
import GHC.IO.Handle
import GHC.IO.Handle.FD
import EvalUtil
import InterpolationUtil hiding (window)
import SlidingWindowUtil
import Text.Read

-- | Вывод предложения ко вводу и чтение строки из input stream
read_ :: IO String
read_ =
  putStr "Enter point (x, y)> "
    >> hFlush stdout
    >> getLine

-- | Проверка введенных данных
validate_ :: String -> Bool
validate_ str = case words str of
  [x, y] -> isJust (readMaybe x :: Maybe Double) && isJust (readMaybe y :: Maybe Double)
  _ -> False

-- | Проверка, что новый X больше предыдущего
validateGrowingX :: String -> Interpolation -> Bool
validateGrowingX input (Interpolation _ window _ _) = case words input of
  [xStr, _] -> case head (getElements window) of
    Just (x0, _) -> x0 < read xStr -- Если Point определена и X_new > X_old
    _ -> True
  _ -> False

-- | Вычислене
eval_ :: String -> Interpolation -> (Interpolation, Maybe String)
eval_ = evalPoint

-- | Вывод результата(монадическое действие)
print_ :: Maybe String -> IO ()
print_ Nothing = return ()
print_ (Just str) = putStrLn str

-- | Read-eval-print цикл
loop_ :: [Interpolation] -> IO ()
loop_ windows = do
  input <- read_
  unless (input == "exit") $
    if validate_ input && validateGrowingX input (head windows)
      then do
        let (newWindows, results) = unzip $ map (eval_ input) windows -- Применяем методы интерполяции к вводу
        mapM_ print_ results -- Применяем моноидную функцию вывода к результатам
        loop_ newWindows --  Запускаем новый цикл интерполяции на основе новых окон
      else do
        putStrLn "Invalid input. Please enter a valid point (x, y). X should be greater than the previous X."
        loop_ windows
