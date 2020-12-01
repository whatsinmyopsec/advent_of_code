import Data.List

combinations 0 lst = [[]]
combinations n lst = do (x:xs) <- tails lst
                        rest   <- combinations (n - 1) xs
                        return $ x : rest

with_factors ns len =
    product (head (filter (\x -> (sum x) == 2020) (combinations len ns)))

main = do input <- readFile "input.txt"
          let ns = map (read :: String -> Int) (lines input)
          print (with_factors ns 2)
          print (with_factors ns 3)
