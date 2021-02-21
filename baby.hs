doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else doubleMe x

doubleSmallNumber' x = if x > 100
    then doubleSmallNumber x + 1
    else doubleSmallNumber x

multipleRange x y = take y [x, (x + x)..]

filterGreater xs ys = [x | x <- xs, x > ys]

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- 行列
calcMatrix xs ys = [x * y | x <- xs, y <- ys]

-- 配列の長さを取得
length' xs = sum [1 | _ <- xs]

-- 文字列から小文字を削除
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

{-
直角三角形を見つける

直角三角形の条件
- ３辺の長さは全て整数である。
- 各辺の長さは10以下である
- 周囲の長さは24に等しい
-}
rightTriangles = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a + b + c == 24]

-- 関数 (パターンマッチ)
-- `x = ...` は全てのパターンにマッチ
sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Twe"
sayMe 3 = "Three"
sayMe x = "Other number."

-- 関数 (再帰)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n + factorial (n - 1)

-- 関数 (タプル)
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- 関数 (リスト)
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy"
head' (x:_) = x

--  関数 (リスト + as)
tell :: (Show a) => [a] -> String
tell [] = "The list is empty."
tell (x:[]) = "The list has one element." ++ show x
tell (x:y:[]) = "The list has two elements." ++ show x ++ " and " ++ show y
tell all@(x:y:_) = "The list is long.(length=" ++  show (length all) ++ ";first=" ++ show x ++ ")"

