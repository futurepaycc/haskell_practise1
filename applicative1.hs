{- 简化理解:
applicative就是支持flatmap的functor
这里的floatmap比一般语言强化: 支持[fn1,fn2,fn3] <*> [int1,int2,int3]笛卡尔积, 也相当于多个map操作然后concat
-}

-- 应用函子 的偏函数性质
test_applicative1 = do
    let res1 = (^2) <$> [1,2,3]
    print res1
    let res2 = fmap (^2) [1,2,3]
    print res2
    -- 这里<$>相当于fmap的中缀操作
    let res3 = (^) <$> Just 2 <*> Just 3
    print res3

    -- 更简简的示例
    print $ Just (+2) <*> Just 3
    print $ [(+2)] <*> [1,2,3]

-- 应用函子 平行操作性质, 每个偏函数平行展开
test_applicative2 = do
    print "--------------------------"
    let res1 = fmap (+1) [1,2,3]
    print res1

    -- applicative简单理解相当于flatmap, 但和monad区别? 后者作用更宽?
    let res2 = [(+1),(+5)] <*> [1,2,3]
    print res2

    -- 一批一批的作用
    let res3 = [(*1),(^2)] <*> [1,2,3]
    print res3
    

main = do 
    test_applicative1
    test_applicative2
    