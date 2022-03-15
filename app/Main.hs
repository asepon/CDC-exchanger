{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Char (isDigit)
import Data.List

data Product = Product { prodtype :: String, balanceidr :: Int, balancecur :: Int, rate :: Int } deriving (Show, Eq)

type Prods = [Product]

getProd :: Prods -> (Maybe Product, Prods)
getProd (x:xs) = (Just x, xs)
getProd     [] = (Nothing,[])

getProdType :: Product -> String
getProdType Product { prodtype=p, balanceidr = _ , balancecur = _ , rate = _ } = p
-- >>> getProdType Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500}
-- "IDRUSD"

getUsd :: Product -> Int
getUsd Product { prodtype="IDRUSD" , balanceidr = _ , balancecur = p , rate = _ } = p
-- >>> getUsd Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500}
-- 1000

getEur :: Product -> Int
getEur Product { prodtype="IDREUR" , balanceidr = _ , balancecur = p , rate = _ } = p
-- >>> getEur Product {prodtype = "IDREUR", balanceidr = 16500000, balancecur = 1000, rate = 16500}
-- 1000

getRateUsd :: Product -> Int
getRateUsd Product { prodtype="IDRUSD" , balanceidr = _ , balancecur = _ , rate = p } = p
-- >>> getRateUsd Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500}
-- 14500

getRateEur :: Product -> Int
getRateEur Product { prodtype="IDREUR" , balanceidr = _ , balancecur = _ , rate = p } = p
-- >>> getRateEur Product {prodtype = "IDREUR", balanceidr = 16500000, balancecur = 1000, rate = 16500}
-- 16500

getIdr :: Product -> Int
getIdr Product { prodtype=_ , balanceidr = p , balancecur = _ , rate = _ } = p
-- >>> getIdr  (Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500})
-- 14500000

getRate :: String -> Prods -> Int
getRate str [] = 0
getRate str (p:ps)
            | getProdType p == str = if str == "IDRUSD" then getRateUsd p else getRateEur p
            | otherwise = getRate str ps
-- >>> getRate "IDREUR" [Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500},Product {prodtype = "IDREUR", balanceidr = 16500000, balancecur = 1000, rate = 16500}]
-- 16500

getIdrBal :: String -> Prods -> Int
getIdrBal str [] = 0
getIdrBal str (p:ps)
            | getProdType p == str = getIdr p
            | otherwise = getIdrBal str ps
-- >>> getIdrBal "IDRUSD" [Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500},Product {prodtype = "IDREUR", balanceidr = 16500000, balancecur = 1000, rate = 16500}]
-- 14500000

getBalance :: String -> Prods -> Int
getBalance str [] = 0
getBalance str (p:ps)
            | getProdType p == str = if str == "IDRUSD" then getUsd p else getEur p
            | otherwise = getBalance str ps
-- >>> getBalance "IDREUR" [Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500},Product {prodtype = "IDREUR", balanceidr = 16500000, balancecur = 1000, rate = 16500}]
-- 1000

getAllIdr :: Prods -> Int
getAllIdr = foldr ((+) . getIdr) 0
-- >>> getAllIdr [Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500},Product {prodtype = "IDREUR", balanceidr = 16500000, balancecur = 1000, rate = 16500}]
-- 31000000

loadProduct :: [String] -> Product
loadProduct str = do
                          let a = head str
                          let b = str !! 1
                          let c = str !! 2
                          let d = str !! 3
                          Product {prodtype = a, balanceidr = read b, balancecur = read c, rate = read d}
-- >>> loadProduct ["IDRUSD","14500000","1000","14500"]
-- Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500}

unProduct :: Product -> [String]
unProduct Product {prodtype = a, balanceidr = b, balancecur = c, rate = d} =  [a, show b, show c, show d]
-- >>> unProduct (Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500})

loadAllProduct :: [[String]] -> Prods
loadAllProduct = map loadProduct
-- >>> loadAllProduct [["IDRUSD","14500000","1000","14500"],["IDREUR","16500000","1000","16500"]]
-- [Product {prodtype = "IDRUSD", balanceidr = 14500000, balancecur = 1000, rate = 14500},Product {prodtype = "IDREUR", balanceidr = 16500000, balancecur = 1000, rate = 16500}]

loadShowRate ::  IO ()
loadShowRate = do
       writeLog' "\nload and show rate balance"
       contents <- readFile "./app/balance.txt"
       let listOfContents = map words $ lines contents
       let p = loadAllProduct listOfContents
       let idrBalance = getAllIdr p
       let usdBalance = getBalance "IDRUSD" p
       let eurBalance = getBalance "IDREUR" p
       let usdRate = getRate "IDRUSD" p
       let eurRate = getRate "IDREUR" p
       putStrLn $ "USD Rate :"++ show usdRate
       putStrLn $ "EUR Rate :"++ show eurRate
       putStrLn $ "IDR Liquidity balance :"++ show idrBalance
       putStrLn $ "USD Liquidity balance :"++ show usdBalance
       putStrLn $ "EUR Liquidity balance :"++ show eurBalance

data Order = Order { no :: Int, prod :: String, qty :: Int, orderrate :: Int, ordertype :: String } deriving (Show, Eq)

loadAllOrder :: [[String]] -> [Order]
loadAllOrder = map loadOrder

loadOrder :: [String] -> Order
loadOrder str = do
       let a = head str
       let b = str !! 1
       let c = str !! 2
       let d = str !! 3
       let e = str !! 4
       Order {no = read a, prod = b, qty = read c, orderrate = read d,ordertype=e}

askOrderQty :: MaybeT IO String
askOrderQty = MaybeT $ do
  a <- getLine
  writeLog' $ "\nOrder " ++ a ++ " entered"
  if all isDigit a
    then return $ Just a
    else return Nothing

unWrapOrder :: Maybe a -> a
unWrapOrder (Just x) = x

substring :: Int        -- Start Index
             -> Int     -- End Index
             -> String  -- Target String
             -> String
substring start end text = take (end - start) (drop start text)

addorder :: String    -- order type
            -> String -- Product name
            -> IO ()
addorder oType oProd = do
       writeLog' $ "\nAsking order " ++ oType ++ " " ++ oProd
       let oCur = substring 3 6 oProd
       print $ oType ++ "ing " ++ oCur
       -- Collect Product information
       balanceContents <- readFile "./app/balance.txt"
       evaluate (force balanceContents)
       let p = loadAllProduct $ map words $ lines balanceContents
       -- Ask User order qty
       print $ "How much you want to "++ oType ++"?"
       x <- runMaybeT askOrderQty
       if x == Nothing
              then do
                  print "Invalid number!"
                  writeLog' "\nOrder entry invalid!"
                  addorder oType oProd
              else do
                  let oQty = unWrapOrder x
                  orderContents <- readFile "./app/transaction.txt"
                  evaluate (force orderContents)
                  -- Collect order information
                  -- Get the last order number
                  let orderCount = 1 + length (lines orderContents)
                  let oRate = getRate oProd p
                  -- put all order information together into a string
                  let saveText = "\n" ++ show orderCount ++ " " ++ oProd ++ " " ++ oQty ++ " " ++ show oRate ++ " " ++ oType
                  putStrLn $ "You order " ++ oType ++ " " ++ oQty ++ " " ++ oCur
                  putStrLn $ "With rate " ++ show oRate
                  -- write order information to file
                  appendFile "./app/transaction.txt" saveText
                  writeLog' $ "\nwrite transaction success"
                  -- Collect balance information
                  -- get balance calculate after order
                  let listProdString = map words $ lines balanceContents
                  case oType of
                     "Buy"  -> do -- When buy, cur will increase and idr will decrease
                            let newCurBalance = getBalance oProd p + read oQty
                            let newIdrBalance = getIdrBal oProd p - (read oQty * oRate)
                            let w = updateBalance oProd (show newIdrBalance) (show newCurBalance) listProdString
                            --write new balance to file
                            writeBalance w
                     "Sell" -> do -- When sell, cur will decrease and idr will increase
                            let newCurBalance = getBalance oProd p - read oQty
                            let newIdrBalance = getIdrBal oProd p + (read oQty * oRate)
                            let w = updateBalance oProd (show newIdrBalance) (show newCurBalance) listProdString
                            --write new balance to file
                            writeBalance w
                  putStrLn "Successfully executed! "
                  menu

updateBalance :: String               -- Product Type 
                 -> String            -- BalanceIDR
                 -> String            -- BalanceCur 
                 -> [[String]]        -- List line file balance
              --    -> String            -- Transaction Type
                 -> [[String]]
updateBalance _ _ _ []  = []
updateBalance a b c (d:ds) =
                             if a == head d
                                then [a, b, c, d !! 3] : updateBalance a b c ds
                                else d : updateBalance a b c ds


writeBalance :: [[String]] -> IO ()
writeBalance [] = return ()
writeBalance x = do
       balanceContents <- readFile "./app/balance.txt"
       evaluate (force balanceContents)
       let savetext =  unwords (head x) ++ "\n" ++ unwords (x!!1)
       writeFile "./app/balance.txt" savetext
       writeLog' "\nWrite updated balance success!"

menu :: IO()
menu = do
       writeLog' "\nShow menu"
       putStrLn "Choose menu : \n(a)Buy USD   (b)Buy EUR   (c)Sell USD   (d)Sell EUR   (r)view Rate/Liquidity   (q)Exit "
       putStr "Your choice : "
       m <- getLine
       writeLog' $ "\nChoosing menu :" ++ m
       case m of
           "a" -> addorder "Buy" "IDRUSD"
           "b" -> addorder "Buy" "IDREUR"
           "c" -> addorder "Sell" "IDRUSD"
           "d" -> addorder "Sell" "IDREUR"
           "r" -> do
                  loadShowRate
                  menu
           "q" -> do
                        putStrLn "Exit"
                        writeLog' "\nExit"
           _   -> menu

main :: IO ()
main = do
       writeLog' "\nApplication Start" 
       loadShowRate
       menu


writeLog' :: String -> IO ()
writeLog' x = do 
       logContents <-  readFile "./app/app.logs"
       evaluate (force logContents)
       appendFile "./app/app.logs" x