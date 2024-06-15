{-#LANGUAGE GADTs#-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Listas where

import Prelude hiding (snd,fst,null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++), reverse,elem,concat,head,tail,last,init,(!!),take,drop,takeWhile,dropWhile,split)

--Recursión Estructural
ordenada :: Ord a => [a] -> Bool
ordenada = \l -> case l of
    {[] -> True;
    [x] -> True;
    x:y:ys -> case (x <= y) of {
        True -> ordenada (y:ys);
        False -> False
    }  
} 

--Recursión Estructural
insert :: Ord a => a -> [a] -> [a]
insert = \e l -> case l of {
    []-> [e];
    x:xs -> case x <= e of {
        True -> x:(insert e xs);
        False -> e:x:xs;
    }    
}

insertSort :: Ord a => [a] -> [a]
insertSort = \l -> case l of {
    [] -> [];
    [x] -> [x];
    x:y:ys -> insert x (insertSort (y:ys))
}

minL :: Ord a => [a] -> a
minL = \l -> case l of {
    [] -> error "La lista no tiene elementos";
    [x] -> x;
    x:y:ys -> case (x >= y) of {
        True -> minL (y:ys);
        False -> minL (x:ys);
    }
}

borrar1 :: Eq a => a -> [a] -> [a]
borrar1 = \x l -> case l of {
    [] -> [];
    y:ys -> case (x == y) of
        True -> ys;
        False -> y:(borrar1 x ys)
}

selectSort :: Ord a => [a] -> [a]
selectSort = \l -> case l of {
    [] -> [];
    x:xs -> minL l:(selectSort (borrar1 (minL l) l))
}

firstL :: ([a],[a]) -> [a] 
firstL = \l -> case l of {
    ([],[]) -> [];
    ([],x:xs) ->[];
    (x:xs,[]) -> x:xs;
    (x:xs,y:ys) -> x:xs
}

secondL :: ([a],[a]) -> [a] 
secondL = \l -> case l of {
    ([],[]) -> [];
    ([],x:xs) ->x:xs;
    (x:xs,[]) -> [];
    (x:xs,y:ys) -> y:ys
}

split :: [a] -> ([a],[a])
split = \l -> case l of {
    [] -> ([],[]);
    [x] -> ([x],[]);
    x:y:ys -> (x:(firstL (split ys)), y:secondL (split ys)) 
}

merge ::  Ord a => [a] -> [a] -> [a]
merge = \l e -> case l of {
    [] -> e;
    x:xs -> case e of{
    [] -> x:xs;
    y:ys -> case x <= y of{
        True -> x:(merge (xs) (y:ys));
        False -> y:(merge (x:xs) (ys))
        }
    }
}

mergeSort :: Ord a => [a] -> [a]
mergeSort = \l -> case l of {
    [] -> [];
    [x] -> [x];
    _ -> merge (mergeSort (firstL (split l))) (mergeSort (secondL (split l)))
}