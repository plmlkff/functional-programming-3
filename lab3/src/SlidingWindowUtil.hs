module SlidingWindowUtil
  ( SlidingWindow,
    newSlidingWindow,
    addElement,
    getElements,
  )
where

data SlidingWindow a = SlidingWindow Int [Maybe a] deriving (Show, Eq)

-- Создаем новое скользящее окно размера n, заполненное значениями Nothing
newSlidingWindow :: Int -> SlidingWindow a
newSlidingWindow size = SlidingWindow size (replicate size Nothing)

-- Добавление элемента в скользящее окно(слева)
addElement :: a -> SlidingWindow a -> SlidingWindow a
addElement x (SlidingWindow size elems) = SlidingWindow size (take size (Just x : init elems))

-- Получение элементов скользящего окна
getElements :: SlidingWindow a -> [Maybe a]
getElements (SlidingWindow _ elems) = elems
