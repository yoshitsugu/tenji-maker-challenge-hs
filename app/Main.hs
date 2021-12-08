module Main where

import Data.List.Split (splitOn)

tenji :: String -> String
tenji a =
  let cs = map (\cs -> if length cs == 1 then (' ', last cs) else (head cs, last cs)) $ splitOn " " a
      flags = map toFlags cs
   in unwords (map (\(_, v) -> (toC . t1) v ++ (toC . t3) v) flags)
        ++ "\n"
        ++ unwords (map (\(c, v) -> (toC . t2) v ++ (toC . t2) c) flags)
        ++ "\n"
        ++ unwords (map (\(c, v) -> (toC . t1) c ++ (toC . t3) c) flags)
  where
    -- 特殊ケースのハンドリング (3, 5, 6), (1, 2, 4)
    toFlags :: (Char, Char) -> ((Bool, Bool, Bool), (Bool, Bool, Bool))
    toFlags ('Y', 'A') = ((True, False, False), (False, False, True))
    toFlags ('Y', 'U') = ((True, False, True), (False, False, True))
    toFlags ('Y', 'O') = ((True, True, False), (False, False, True))
    toFlags ('W', 'A') = ((True, False, False), (False, False, False))
    toFlags ('W', 'O') = ((True, True, False), (False, False, False))
    toFlags (' ', 'N') = ((True, True, True), (False, False, False))
    toFlags (a, b) = (consonantToFlags a, vowelToFlags b)

    -- 母音 -> 1, 2, 4
    vowelToFlags :: Char -> (Bool, Bool, Bool)
    vowelToFlags 'A' = (True, False, False)
    vowelToFlags 'I' = (True, True, False)
    vowelToFlags 'U' = (True, False, True)
    vowelToFlags 'E' = (True, True, True)
    vowelToFlags 'O' = (False, True, True)
    vowelToFlags _ = (False, False, False)

    -- 子音 -> 3, 5, 6
    consonantToFlags :: Char -> (Bool, Bool, Bool)
    consonantToFlags 'K' = (False, False, True)
    consonantToFlags 'S' = (False, True, True)
    consonantToFlags 'T' = (True, True, False)
    consonantToFlags 'N' = (True, False, False)
    consonantToFlags 'H' = (True, False, True)
    consonantToFlags 'M' = (True, True, True)
    consonantToFlags 'R' = (False, True, False)
    consonantToFlags _ = (False, False, False)

    t1 :: (Bool, Bool, Bool) -> Bool
    t1 (a, _, _) = a

    t2 :: (Bool, Bool, Bool) -> Bool
    t2 (_, b, _) = b

    t3 :: (Bool, Bool, Bool) -> Bool
    t3 (_, _, c) = c

    toC :: Bool -> String
    toC True = "o"
    toC False = "-"

test :: String -> String -> IO ()
test a b = do
  let ta = tenji a
  let result = if ta == b then "OK!" else "NG!"
  putStrLn $ "-------\n*" ++ result ++ "*\n\ninput: " ++ a ++ "\nexpected:\n" ++ b ++ "\nactual:\n" ++ ta

main :: IO ()
main = do
  test "A HI RU" "o- o- oo\n-- o- -o\n-- oo --"
  test "KI RI N" "o- o- --\no- oo -o\n-o -- oo"
  test "SI MA U MA" "o- o- oo o-\noo -o -- -o\n-o oo -- oo"
  test "NI WA TO RI" "o- -- -o o-\no- -- oo oo\no- o- o- --"
  test "HI YO KO" "o- -o -o\no- -o o-\noo o- -o"
  test "KI TU NE" "o- oo oo\no- -o o-\n-o o- o-"
