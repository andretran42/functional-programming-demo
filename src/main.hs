-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage x = putStrLn (show x)

-- Write division here
division :: Double -> Double -> Maybe Double
division _ 0 = Nothing 
division x y = Just (x / y) 

-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Write factList here
factList :: Int -> [Int]
factList n = [factorial i | i <- [1..n]]
  where
    factorial 0 = 1
    factorial x = product [1..x]


-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge [] ls2 = ls2
merge ls1 [] = ls1
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

main :: IO ()
main = do

    -- Testing printAMessage
    putStrLn "\nPrint:\n"
    printAMessage "print \n"

    -- Testing division
    putStrLn "Testing division: \n"
    let z = division 1 2
        w = division 1 1
        g = division 6 2
    putStrLn $ "z: " ++ show z ++ ", w: " ++ show w ++ ", g: " ++ show g ++ "\n"

    -- Testing factorial
    putStrLn "\nTesting factorial: \n"
    let a = factorial 1
        b = factorial 7
    putStrLn $ "a: " ++ show a ++ ", b: " ++ show b ++ "\n"

    -- Testing factList
    putStrLn "\nTesting factList: \n"
    let testList = factList 5
    putStrLn $ "testList: " ++ show testList ++ "\n"
    
    -- Testing merge
    putStrLn "\nTesting merge: \n"
    let merged = merge [1, 3, 6] [2, 4, 5, 6, 7]
    putStrLn $ "merge: " ++ show merged ++ "\n"
