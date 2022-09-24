-- https://oliverbalfour.github.io/haskell/2020/08/09/parsing-arithmetic-with-monads.html (系列文章)
{- 感觉基本是使用类型定义编程， 骨子里与cpp的tmp有点像 -}

import Data.Char (isDigit, isSpace)
import Control.Applicative ( Alternative(..) )


-- ------------------------------------------------------------------
--            一、基础解析定义部分
-- ------------------------------------------------------------------
{- 
newtype Parser a = Parser (String -> [(a, String)])
parse :: Parser a -> (String -> [(a, String)])
parse (Parser f) = f
-}
-- newtype定义一个通用类型Paser a构造子，有点像[a]
--record语法，与上面三行定义等价, 这种复合类型没法deriving Show
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- item定义为单字符解析器:  parse item "asdf" => [('a',"sdf")]
item :: Parser Char  -- NOTE 使用newtype定义出的通用类型Parser再定义Parser Char的方式
item = Parser (\cs -> case cs of
  "" -> []
  (c:cs) -> [(c,cs)])


-- 将Paser实例化成: 函子(用内置map函数定义fmap)
instance Functor Parser where
  fmap f (Parser p) = Parser (\cs ->
    map (\(x, cs') -> (f x, cs')) (p cs))

-- 将Paser实例化成: 应用函子(flatmap偏函数)
instance Applicative Parser where
  pure x = Parser (\cs -> [(x, cs)])
  f <*> a = Parser (\cs ->
    concat [parse (fmap fn a) cs' | (fn, cs') <- parse f cs])

-- 将Paser实例化成: 单子(flatmap垂直级联函数)
instance Monad Parser where
  return = pure
  p >>= f = Parser (\cs ->
    concat [parse (f a) cs' | (a, cs') <- parse p cs])

-- 将Paser实例化成: 选择组合子(有empty和<|>语义)
instance Alternative Parser where
  empty = Parser (\_ -> [])
  p <|> q = Parser (\cs ->
    let (p', q') = (parse p cs, parse q cs) in
    if length p' > 0 then p' else q')

-- 条件解析器，返回单字符解析器
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = item >>= (\c -> if pred c then pure c else empty)  -- NOTE 这里已经用到了单子操作>>= 和选择组合子能力 empty

-- 条件单字符解析: parse (char 'a') "adsf"
char :: Char -> Parser Char
char c = satisfy (== c)

-- 数字解析器:    ["8123"] >>= parse digit = parse digit "8123" => [('8',"123")]
digit :: Parser Int
digit = fmap (read . (:[])) (satisfy isDigit)


-- 多空格解析器, many函数能力来自Alternative: parse space "  df" => [("  ","df")]
space :: Parser String
space = many (satisfy isSpace)

-- 字符串解析器: parse (string "as") "asdf" => [("as","df")]
string :: String -> Parser String
string "" = return ""
string (c:cs) = (:) <$> char c <*> string cs

-- token解析器: ["hello"] >>= parse (token "hello") = parse (token "hello") "  hello" => [("hello","")]
token :: String -> Parser String
token symb = space *> string symb -- *> 算符来自Alternative

-- ------------------------------------------------------------------
--            二、代数运算表达式部分 FIXME 这里的算符解析如何直接输出List型结果???
-- ------------------------------------------------------------------
{- 
This Applicative instance is very useful for several reasons. 
First, we now have access to the *> and <* functions. *> is equivalent to >> for Monads, in that it sequences two monadic functions and returns the value of the second. 
<* returns the value of the first. This means we can write parsers like space *> digit <* space, which is a Parser Int expression that parses a digit surrounded by whitespace which is discarded
-}

-- 算符解析器: let m1 = parse add "3+"; :t m1 => [(Int -> Int -> Int, String)] -- ??? parse add "" 也能通过
-- let add1 = fst $ (parse add "3+") !! 0; :t add1 => Int -> Int -Int  -- 但是不能调用 add1 3 3 
add :: Parser (Int -> Int -> Int)
add = token "+" *> pure (+) <|> token "-" *> pure (-)

mul :: Parser (Int -> Int -> Int)
mul = token "*" *> pure (*) <|> token "/" *> pure div -- *>语义:只取右边: Just (+2) *> Just 3 => Just 3, [(+2)] *> [1,2,3] => [1,2,3]

pow :: Parser (Int -> Int -> Int)
pow = (token "^" <|> token "**") *> pure (^)

-- 一元负数解析器部分: parse (unary_minus digit) "-56" => [(-5,"6")]
unary_minus :: Parser Int -> Parser Int
unary_minus p = char '-' *> fmap negate p <|> p

-- 整数解析： parse integer "123" => [(123,"")]
integer :: Parser Int
integer = let positive = fmap read (some (satisfy isDigit))
          in space *> unary_minus positive

-- 解析器左、右连接组合???
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where rest a = ((op <*> pure a <*> p) >>= rest)  <|> return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = p >>= rest
  where rest a =  (op <*> pure a <*> (p >>= rest)) <|> return a

-- 表达式组合: parse expr "1+2" => [(3,"")]
expr = subexpr `chainr1` pow `chainl1` mul `chainl1` add
subexpr = token "(" *> expr <* token ")" <|> integer

-- ------------------------------------------------------------------
--            三、自定义组合测试
-- ------------------------------------------------------------------
add' :: Parser (Int -> Int -> Int)
add' = space *> char '+' *> pure (+) 
digit' = space *> digit <* space
-- parse expr' $ " + 3 3 " => [(6,"")]
expr' = add' <*> digit' <*> digit'


expr'' = do
  a <- digit'
  op <- mul
  b <- digit'
  return (op a b)


-- 常规无括号极简表达式: parse expr''' "1+2*3" => [(7,"")]
expr''' = integer `chainr1` pow `chainl1` mul `chainl1` add
-- 最简加法: parse expr'''' "3+3" => [(6,"")]
expr'''' = integer `chainl1` add


-- 实际解析执行: repl "1+2" => 3, repl "1+2*3" => 7
repl :: String -> String
repl cs = let results = parse expr cs in
  case results of
    [] -> "Invalid expression"
    ((num, _):_) -> show num

-- 使用IO库中的interact完成构造输入、输出交互能力
-- main = interact (unlines . map repl . lines)
main = do
  let res = parse expr "1+2"
  print res

  let res = repl "1+2*3"
  print res