{- 类型类测试 -}

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

test_yesno = do
    -- 字面量需要加类型提示，否则是num，还必须加括号, 原因不明
    print $ yesno (15::Int)
    print $ yesno []
    print $ yesno (0::Int)
    print $ yesno [15]

main = do
    test_yesno