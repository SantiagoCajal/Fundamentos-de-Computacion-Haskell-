{-#LANGUAGE GADTs#-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use lambda" #-}
{-# HLINT ignore "Use /=" #-}

module Listas where

import Naturales
import Prelude hiding (count,null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++), reverse,elem,concat,head,tail,last,init,(!!),fst,snd,take,drop,takeWhile,dropWhile,split)
import Data.ByteString.Lazy ()

null :: [a] -> Bool
null = \l -> case l of {
    [] -> True;
    x:xs -> False
}

length :: [a] -> N 
length = \l -> case l of {
    [] -> O;
    x:xs -> S(length xs)
}

duplicate :: [a] -> [a]
duplicate = \l -> case l of{
    [] -> [];
    x:xs -> x:x:(duplicate xs)
}

sum :: [N]-> N
sum = \l -> case l of {
    [] -> O;
    x:xs -> x + (sum xs)
}

prod :: [N] -> N
prod = \l -> case l of {
    [] -> O;
    [x] -> x;
    y:ys -> y * (prod ys);
}

map :: (a->b) -> [a] -> [b] 
map = \f l -> case l of{
    [] -> [];
    x:xs -> f x: (map f xs)
}

zipWith :: (a->b->c)-> [a]-> [b]-> [c]
zipWith = \f l n -> case l of {
    [] -> [];
    x:xs -> case n of{
        [] -> [];
        y:ys -> (f x y): (zipWith f xs ys)
    }
}

filter ::  (a->Bool) ->[a]-> [a]
filter = \p l -> case l of{
    [] -> [];
    x:xs -> case p x of {
        False -> filter p xs;
        True -> x: (filter p xs)
    }
}

and :: [Bool]-> Bool
and = \l -> case l of {
    [] -> True;
    x:xs -> x && (and xs)
}

or :: [Bool] -> Bool
or = \l -> case l of {
    [] -> False;
    x:xs -> x || (or xs)
}

count :: (a -> Bool) -> [a] -> N
count = \p l -> case l of{
    [] -> O;
    x:xs -> case p x of {
        True -> S(count p xs);
        False -> count p xs
    }
}

any :: (a->Bool) -> [a] -> Bool 
any = \p l -> case l of {
    [] -> False; 
    x:xs -> case p x of {
        False -> any p xs;
        True -> True
    }
}

all :: (a->Bool) -> [a] -> Bool
all = \p l -> case l of {
    [] -> True;
    x:xs -> case p x of {
        False -> False;
        True -> all p xs
    }
}

count1 :: (a -> Bool) -> [a] -> N
count1 = \p l -> length (filter p l) 

any1 :: (a->Bool) -> [a] -> Bool 
any1 = \p l -> not (length (filter p l) == O)      

all1 :: (a->Bool) -> [a] -> Bool
all1 = \p l -> (length l) == (length (filter p l))

(++) :: [a] -> [a] -> [a]
(++) = \l k -> case l of {
    [] -> k;
    x:xs -> x:(xs ++ k)
}

reverse :: [a]-> [a]
reverse = \l -> case l of{
    [] -> [];
    x:xs -> (reverse xs) ++ [x] 
}

elem :: Eq a => a -> [a]-> Bool
elem = \n l -> case l of{
    [] -> False;
    x:xs -> case n == x of {
        False -> elem n l;
        True -> True
    }
}

concat :: [[a]] -> [a]
concat = \l -> case l of {
    [] -> [];
    xs:xss -> case xs of {
        [] -> concat xss;
        y:ys -> (y: (concat [ys])) ++ (concat xss)
    }
}

fst :: (a,b) -> a
fst = \p -> case p of {
    (x,y) -> x
}

snd :: (a,b) -> b
snd = \p -> case p of {
    (x,y) -> y
}

zip :: [a]-> [b]-> [(a,b)]
zip = \l k -> case l of{
    [] -> [];
    x:xs -> case k of{
        [] -> [];
        y:ys -> (x,y): (zip xs ys)
    }
}
