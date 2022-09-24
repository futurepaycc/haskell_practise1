{-NOTE: 简化理解
monad后一个计算问题可以用到前面的参数，可以级连 
monad也是具有前后相关产生式的底层实现
-}

-- 手工定义join, 一般也叫flatmap, concat也能起到类似作用?
join1 :: Monad m => m (m a) -> m a
join1 x = x >>= id

test_join1 = do
    print $ join1  [[1],[2],[3]]
    print $ concat [[1],[2],[3]]

{- 
Applicative需要满足的约束比Monad的弱，Applicative的<*>更接近计算(computation)的简单组合，而Monad的>>=有更多的计算结果的依赖。
Applicative的<*>不关心左边computation的结果，其两边的computation是相互独立的，因此具有更好的组合性
Monad的>>=会根据左边的comuptation的结果来确定后续的计算，因此>>=右边的computation和左边的computation是有依赖关系的。
 -}
test_monad1 = do 
{- 能够提供join函数的函子类型称为单子 => fmap + 脱壳 => 这不和applicative1一样了么???
join :: m (m a) -> m a
X >>= f = join $ fmap f X
-}

{- 
(>>=) :: m a -> (a -> m b) -> m b
[1,2,3]                 对应: m a              #盒子型输入
\item->[item,item^2]    对应: (a->m b)         #输入的一个元素如何对应一个输出类型的盒子
res1                    对应: m b  (结果)       #最终只保留一层盒子的输出(类型flatmap的把盒子拍平)
-}
    -- 一个一个的作用?
    let res1 = [1,2,3] >>= \item -> [item, item^2]
    print res1

    -- 使用无盒函数，不能 >>= 级连
    let res2 = [1,2,3] >>= \n -> "ab"   -- 无盒，这里没有使用 (a ->m b)直接是(a->b)也可以???
    print res2

{- 根据提示要换成const函数: 把第一个参数连接脱壳?
const "1" "2" => "1"
const ['a','b'] 5 => 'ab'
const ['a','b'] [1,2,3] => 'ab'
-}
    let res4 = [1,2,3] >>= const ['a','b']
    print res4


    -- 带盒函数
    let res5 = [1,2,3] >>= \n -> ["ab"]   --这里没有使用 (a ->m b)直接是(a->b)也可以???
    print res5
    let res5 = [1,2,3] >>= const ["ab"]   --这里没有使用 (a ->m b)直接是(a->b)也可以???
    print res5


{- 两阶阶理解
1. >>= 建立对应关系
2. >>= 根据对应关系应用产生式
-}
    let res6 = [1,2,3] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
    print res6    
    -- 产生式与上面monad等价
    let res7 = [(x,y)|x<-[1,2,3],y<-['a','b']]
    print res7


test_monad2 = do
    let res1 = map (show) $ map (^2) [1,2,3]
    print res1

    -- 可以用单子简单充当函数级连, 替换map和复合函数, 这里特别像组合子parser的process function后加result的壳
    let res2 = [1,2,3] >>= \x1 ->[x1^2] >>= \x2 -> [show x2]
    print res2

    -- 单子垂直操作的性质，后续步骤可以使用前置步骤的中间结果
    -- 也可以多步计算结果复合, 注意下面的x1,x2: x1代表原始值, x2代表第1步计算值: 1 + 1^2, 2+2^, 3+3^2
    let res3 = [1,2,3] >>= \x1 ->[x1^2] >>= \x2 -> [show $ x1 + x2]
    print res3

main = do
    test_join1
    test_monad1
    test_monad2