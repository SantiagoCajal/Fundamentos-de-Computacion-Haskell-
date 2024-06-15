{-#LANGUAGE GADTs#-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Listas where

import Naturales
import Prelude hiding (null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++), reverse,elem,concat,head,tail,last,init,(!!),fst,snd,take,drop,takeWhile,dropWhile,split)

null :: [a] -> Bool
null = \l -> case l of {[] -> True; x:xs -> False}

length :: [a] -> N 
length = \l -> case l of {[] -> 0; x:xs -> S(length xs)}

duplicate :: [a] -> [a]
duplicate = \l -> case l of{[] -> []; x:xs -> x: (duplicate xs)}

sum :: [N] -> N 
sum = \l -> case l of {[] -> O; x:xs -> x + (sum xs)}

prod :: [N] -> N 
prod = \l -> case l of {[] -> 1; x:xs -> x * (prod xs)}

map :: (a->b) -> [a] -> [b] 
map = \f l -> case l of{ [] -> []; x:xs -> f x: (map f xs)}

zipWith :: (a->b->c)-> [a]-> [b]-> [c]
zipWith = \f l n -> case l of { [] -> []; x:xs -> case n of{ [] -> []; y:ys -> (f x y): (zipWith f xs ys)}}

filter ::  (a->Bool) ->[a]-> [a]
filter = \p l -> case l of{ [] -> []; x:xs -> case p x of { False -> filter p xs; True -> x: (filter p xs)}}

and :: [Bool]-> Bool
and = \l -> case l of { [] -> True ; x:xs -> x && (and xs)}

or :: [Bool] -> Bool
or = \l -> case l of {[] -> False ; x:xs -> x || (or xs)}

count :: (a->Bool) -> [a] -> N
count = \p l -> case l of {[] -> 0 ; x:xs -> case p x of { False -> count p xs ; True -> S (count p xs)}}

any :: (a->Bool) -> [a] -> Bool
any = \p l -> case l of{[] -> False ; x:xs -> case p x of {False -> any p xs ; True -> True}}

all :: (a->Bool) -> [a] -> Bool
all = \p l -> case l of {[] -> True ; x:xs -> case p x of {False -> False; True -> all p xs}}

(++) :: [a] -> [a] -> [a]
(++) = \l n -> case l of {x:xs -> x: ((++) xs n) ; [] -> case n of {y:ys -> y: ((++) [] ys) ; [] -> []}}

reverse :: [a]-> [a]
reverse = \l -> case l of{[] -> [] ; x:xs ->  (++) (reverse xs)  [x] }

elem :: Eq a => a -> [a]-> Bool
elem = \n l -> case l of{[] -> False ; x:xs -> case n == x of {False -> elem n l; True -> True}}

concat :: [[a]] -> [a]
concat = \l -> case l of {[[]] -> []; xs:xss -> case xs of {[] -> concat xss; y:ys -> (++) (y: (concat [ys])) (concat xss)}}



fst :: (a,b) -> a
fst = \p -> case p of {(x,y) -> x}

snd :: (a,b) -> b
snd = \p -> case p of {(x,y) -> y}

zip :: [a]-> [b]-> [(a,b)]
zip = \l z -> case l of{ [] -> [] ; x:xs -> case z of{[] -> [] ; y:ys -> (x,y): (zip xs ys)}}

menMay :: Ord a => [a] -> a -> ([a],[a])
menMay = \l n case l of{
    [] -> ([],[]);
    x:xs -> case x < n of {
        True -> (x: fst (menMay xs n), snd (menMay xs n));
        False -> (fst (menMay xs n), x: snd (menMay xs n)) 
    }
}