main :: IO()
main = do
  checkEqual "mymin 1 2" (mymin 1 2) 1
  checkEqual "mymax 2 1" (mymax 2 1) 2
  checkEqual "myfunc 2 4" (myfunc 2 4) 10.0
  checkEqual "mygcd 12 18" (mygcd 12 18) 6
  checkEqual "mygcd 7 13" (mygcd 7 13) 1
  checkEqual "myfib 0" (myfib 0) 1
  checkEqual "myfib 1" (myfib 1) 1
  checkEqual "myfib 2" (myfib 2) 2
  checkEqual "myfib 7" (myfib 7) 21
  checkEqual "myfibIterative 0" (myfibIterative 0) 1
  checkEqual "myfibIterative 1" (myfibIterative 1) 1
  checkEqual "myfibIterative 2" (myfibIterative 2) 2
  checkEqual "myfibIterative 7" (myfibIterative 7) 21
  checkEqual "mymaxdivisor 64" (mymaxdivisor 64) 32
  checkEqual "mymaxdivisor 31" (mymaxdivisor 31) 1
  checkEqual "isInside 6 1 5" (isInside 6 1 5) False
  checkEqual "isInside 5 1 5" (isInside 5 1 5) True
  checkEqual "isLeapYear 2008" (isLeapYear 2008) True
  checkEqual "isLeapYear 2018" (isLeapYear 2018) False
  checkEqual "isValidDate 29 2 2004" (isValidDate 29 2 2004) True
  checkEqual "isValidDate 29 2 2005" (isValidDate 29 2 2005) False
  checkEqual "isValidDate 30 2 2004" (isValidDate 30 2 2004) False
  checkEqual "isValidDate 31 6 2018" (isValidDate 31 6 2018) False
  checkEqual "isValidDate 31 5 2018" (isValidDate 31 5 2018) True
  checkEqual "isValidDate 4 3 2018" (isValidDate 4 3 2018) True


{-
  Зад. 1. Да се напише функция mymin, която приема два аргумента и връща по-малкият от тях.
-}
mymin :: Int -> Int -> Int
mymin a b = if a < b then a else b


{-
  Зад. 2. Да се напише функция mymax, която приема два аргумента и връща по-големият от тях.
-}
mymax :: Int -> Int -> Int
mymax a b = if a > b then a else b


{-
  Зад. 3. Да се напише функция myfunc, която пресмята на средно аритметичното от квадратите на 2 числа.
-}
myfunc :: Double -> Double -> Double
myfunc a b = (a ^ 2 + b ^ 2) / 2


{-
  Зад. 4. Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи.
  (Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)

  Да се напише и итеративно решение.
-}
myfib :: Integer -> Integer
myfib n =
    if n <= 1 then 1
    else myfib (n - 1) + myfib (n - 2)


myfibIterative :: Integer -> Integer
myfibIterative n = helper 1 1 1
  where
    helper :: Integer -> Integer -> Integer -> Integer
    helper i prev cur =
      if i >= n then cur else helper (i + 1) cur (prev + cur)


{-
  Зад. 5. Да се напише функция mygcd a b, която връща НОД(a, b).
-}
mygcd :: Int -> Int -> Int
mygcd a b =
  if b == 0 then a
  else mygcd b (a `mod` b)


{-
  Зад. 6. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1,
за който d < x.
-}
mymaxdivisor :: Int -> Int
mymaxdivisor x = go (x - 1)
  where
    go :: Int -> Int
    go n
      | x `mod` n == 0 = n
      | otherwise = go (n - 1)


{-
  Зад. 7. Да се дефинира функцията isInside x a b, която проверява дали числото x се намира
  в затворения интервал [a .. b].
-}
isInside :: Integer -> Integer -> Integer -> Bool
isInside x a b = (x >= a && x <= b)


{-
  Зад. 8. Да се дефинира функцията isLeapYear year, която проверява дали годината year
  е високосна.
-}
isLeapYear :: Integer -> Bool
isLeapYear year
  | isDivBy 400 = True
  | isDivBy 100 = False
  | isDivBy 4   = True
  | otherwise   = False
  where
    isDivBy a = mod year a == 0


{-
  Зад. 9. Да се дефинира функцията isValidDate day month year, която връща дали датата
  (day, month, year) e валидна.
-}
isValidDate :: Integer -> Integer -> Integer -> Bool
isValidDate day month year =
    if month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 then
        if day >= 1 && day <= 31 then True
        else False
    else if month == 2 then
        if day >= 1 && day <= 29 && isLeapYear year then True
        else if day >= 1 && day <= 28 &&  isLeapYear year == False then True
        else False
    else
        if day >= 1 && day <= 30 then True
        else False


-- Code below is needed for test purposes
checkEqual :: (Eq a, Show a) => String -> a -> a -> IO()
checkEqual preface actual expected =
  print ((if null preface then "" else preface ++ ": ") ++
    if (actual /= expected)
    then  "expected: " ++ show expected ++ ", but got: " ++ show actual
    else "OK, result: " ++ show actual
)
