{- 简化理解
functor就是支持map over操作一些类型集合
-}


{- 
类型类测试: type class 
初步理解:   将 对象和态射 由 值范畴 投影到 盒子范畴
对象: 10            => Maybe 10
态射: Int -> Int    => Maybe -> Maybe
-}


{- 
-- 这样产生了重复定义，需要引入类型类
fmap :: (a -> b) -> [a] -> [b]
fmap = map

fmap :: (a->b) -> Maybe a -> Maybe b
fmap f (Just a) = Just (f a)
fmap _ Nothing  = Nothing 
-}

-- NOTE 下在名字都加了 1 ,避免与内部混淆

-- 类型类定义
class Functor1 f where
    -- 抽像高阶函数: 
    -- p1: 值范畴元素操作函数
    -- p2: 盒子范畴对象(f a)
    fmap1 :: ( a->b ) -> f a -> f b

-- 类型类实例1: 列表
instance Functor1 [] where
    -- 使用列表内部实验
    fmap1 = map

-- 类型类实例2: Maybe
instance Functor1 Maybe where
    -- f 指代 值范畴元素操作函数
    fmap1 f (Just a) = Just (f a)
    fmap1 _ Nothing = Nothing


main = do
    -- let后可以不写in
    let res1 = fmap1 (+1) (Just  10)
    print res1

    let res2 = fmap1 (+1) [1,2,3]
    print res2
