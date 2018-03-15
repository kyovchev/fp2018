main :: IO()
main = do
  print "ex02_1&2.hs"
  checkEqual "n ## k  12 2" (12 ## 2) 5
  checkEqual "n ## k  17 3" (17 ## 3) 344
  checkEqual "isNarcissistic 370" (isNarcissistic 370) True
  checkEqual "isAutomorphic 5" (isAutomorphic 5) True
  checkEqual "sumElems [1,2,3]" (sumElems [1,2,3]) 6
  checkEqual "countElems [1,2,3]" (countElems [1,2,3]) 3
  checkEqual "memberOf 3 [1,2,3]" (memberOf 3 [1,2,3]) True
  checkEqual "calcSum 3 2" (calcSum 3 2) 13
  checkEqual "calcSumFast 3 2" (calcSumFast 3 2) 13
  checkEqual "getInterval 1 3" (getInterval 1 3) [1,2,3]
  checkEqual "removeFirst 3 [1,2,3,4,5]" (removeFirst 3 [1,2,3,4,5]) [1,2,4,5]





{-
  Зад. 1. Напишете оператор n ## k, който приема n > 0 и k >= 0 и връща сумата
  от всяка цифра на n повдигната на степен k.

  Примери:
    12 ## 2 = 1 ^ 2 + 2 ^ 2 = 1 + 4   = 5
    17 ## 3 = 1 ^ 3 + 7 ^ 3 = 1 + 343 = 344
-}
(##) :: Int -> Int -> Int
n ## k = helper n 0
    where
        helper:: Int->Int->Int
        helper augN sumValue
            | augN `mod` 10 == 0 = sumValue
            | otherwise = helper (augN `div` 10) (sumValue + (augN `mod` 10) ^ k)


{-
  Зад. 2. Да се дефинира функцията isNarcissistic n, която приема като аргумент
  цялото положително число n и връща дали то е нарцистично. Нарцистични се наричат
  числата, които са равни на сбора на цифрите си (в десетична бройна система),
  всяка повдигната на степен броя на цифрите на числото.

  Пример за такова число е 153,
  тъй като 1 ^ 3 + 5 ^ 3 + 3 ^ 3 = 1 + 125 + 27 = 153.
-}

isNarcissistic:: Int->Bool
isNarcissistic n =
    helper n (digitCounter n 0) 0
    where
        helper::Int->Int->Int->Bool
        helper augN digitsCount sumOfDigits
            | augN== 0 = (n == sumOfDigits)
            | otherwise = helper (augN `div` 10) digitsCount (sumOfDigits + (augN `mod` 10) ^ digitsCount)

        digitCounter::Int->Int->Int
        digitCounter augN digitsCount
            | augN == 0 = digitsCount
            | otherwise = digitCounter (augN `div` 10) (digitsCount + 1)


{-
  Зад. 3. Да се дефинира предикат isAutomorphic, който приема число n и
  проверява дали n^2 завършва с цифрите на n.
-}
isAutomorphic :: Int -> Bool
isAutomorphic n = helper n (n^2)
    where
        helper::Int -> Int-> Bool
        helper augN n2
            | n `mod` 10 == n2 `mod` 10 && n `div` 10 == 0 = True
            | n `mod` 10 == n2 `mod` 10 = helper (n `div` 10) (n2 `div` 10)
            | otherwise = False


{-
  Зад. 4. По зададени x и n, да се изчисли сумата: 1 + x + x^2 + x^3 + ... + x^n.
-}
calcSum :: Double -> Int -> Double
calcSum x n = helper 0 0
    where
        helper::Int->Double->Double
        helper counter sumValue
            | counter == n + 1 = sumValue
            | otherwise = helper (counter + 1) sumValue + x^counter


{-
  Зад. 5. Да се реши задача 4, чрез използване на не повече от n умножения.
-}
calcSumFast :: Double -> Int -> Double
calcSumFast x n = helper x 0 0
    where
        helper::Double->Int->Double->Double
        helper augX counter sumValue
            | counter == 0 = helper 1 (counter + 1) 0
            | counter == n + 2 = sumValue
            | otherwise = helper (augX * x) (counter + 1) (sumValue + augX)


{-
  Зад. 6. Да се напише функция, която връща като списък целите числа в
  зададен интервал [a, b].
-}
getInterval :: Int -> Int -> [Int]
getInterval a b =
    if a > b then [] else a : getInterval(a + 1) b


{-
  Зад. 7. Напишете фукнция, която намира сумата на елементите на списък от числа.
-}
sumElems :: [Int] -> Int
sumElems xs = helper xs 0
    where
        helper::[Int] -> Int -> Int
        helper xs sumValue
            | null xs = sumValue
            | otherwise = helper (tail xs) (sumValue + head xs)


{-
  Зад. 8. Напишете фунция, която намира броя на елементите на списък.
-}
countElems :: [t] -> Int
countElems xs = helper xs 0
    where
        helper::[t] -> Int -> Int
        helper xs countElements
            | null xs = countElements
            | otherwise = helper (tail xs) (countElements + 1)


{-
  Зад. 9. Напишете предикат, който проверява дали даден елемент се среща в списък.
-}
memberOf :: Int -> [Int] -> Bool
memberOf x xs = helper xs
    where
        helper::[Int] -> Bool
        helper augXs
            | (head augXs) == x = True
            | null augXs = False
            | otherwise = helper (tail augXs)


{-
  Зад. 10. Напишете функция, която премахва първото срещане на x в списъка xs.
-}
removeFirst :: Int -> [Int] -> [Int]
removeFirst x xs
    | null xs == True = []
    | head xs == x =  tail xs 
    | otherwise = head xs : removeFirst x (tail xs)


-- Code below is needed for test purposes
checkEqual :: (Eq a, Show a) => String -> a -> a -> IO()
checkEqual preface actual expected =
  print ((if null preface then "" else preface ++ ": ") ++
    if (actual /= expected)
    then  "expected: " ++ show expected ++ ", but got: " ++ show actual
    else "OK, result: " ++ show actual
  )
