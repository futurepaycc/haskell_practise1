-- data单元素
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show,Eq,Ord,Enum,Bounded)

test_day = do
    let mon = Mon
    print mon
    print $ mon == Mon
    print $ Wed > Mon
    print (minBound :: Day)

-- data多元素
data Shape = Circle Float Float Float
    | Rectange Float Float Float Float

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectange x1 y1 x2 y2) = abs $ ( x2 - x1) * (y2 - y1)

test_surface = do
    let surface1 = surface $ Circle 10 20 10
    let surface2 = surface $ Rectange 10 10 20 20 
    print surface1
    print surface2

-- data record 多元素
data Person = Person String Int deriving Show
test_person = do
    let person1 = Person "liunix" 40
    print person1

main = do
    test_day
    test_surface
    test_person