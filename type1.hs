{- type与newtype区别
type是别名，newtype有强类型名, 直接是个类型构造子，可以也必须用来构造类型值
newtype的构造子只有一个参数(可以是组合参数)
newtype可能更快

简单理解: 可先视newtype为data的单一构造特例

-- https://stackoverflow.com/questions/2649305/why-is-there-data-and-newtype-in-haskell/2650051
-- https://stackoverflow.com/questions/5889696/difference-between-data-and-newtype-in-haskell
-}


-- 类型别名1
type Pos1 = (Int,Int)
mkPos1 :: Int -> Int -> Pos1
mkPos1 x y = (x,y)

test1 = do
    let p1 = mkPos1 3 5
    print p1

-- 新类型1
newtype Url = Url String deriving Show

test2 = do
    let url1 = Url "http://www.baidu.com"
    print url1

-- 新类型2
newtype Pos2 = Pos2 (Int,Int) deriving Show 

test3 = do
    let p2 = Pos2 (3,5)
    print p2    

main = do
    test1
    test2
    test3