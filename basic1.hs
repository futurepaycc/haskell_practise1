-- https://learnxinyminutes.com/docs/haskell/

-- 值绑定
size = 10

-- 函数绑定
doubleFn x = x * 2

-- 函数类型声明
lucky :: Integer -> String
lucky 7 = "good"
lucky others = "bad"

-- 分支
compFn x y =  if x > y
    then x
    else y

-- 自动类型推导声明: 范畴型的吧
maxThree x y z
  | (x >= y) && (x >= z) = x
  | y >= z = y
  | otherwise = z

-- 递归: 这种写法才有点感觉啊， 比较省心
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

fib2 x
  | x < 2 = 1
  | otherwise = fib2 (x - 1) + fib2 (x - 2)

-- 自定义类型
data Person = Person String Int 
            deriving Show   -- 不加上，无法print

-- 执行多条语句do
main = do
  -- print函数嵌套
  print (doubleFn size)
  print (compFn 5 10)
  print (lucky 7)
  print (maxThree 1 2 3)
  print (maxThree 'a' 'b' 'c')
  print (fib 10)
  print (fib2 10)
  print (Person "liunix" 40)

  -- let 语句
  let p1 = Person "jack" 25 in
    print p1