{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use if" #-}

module Naturales where
import Distribution.Simple.Setup (falseArg)
import Distribution.SPDX (LicenseId(TORQUE_1_1))
data N where { O :: N ; S :: N -> N } deriving Show

uno :: N
uno = S O

dos :: N
dos = S uno

tres :: N
tres = S dos

cuatro :: N
cuatro = S tres

cinco :: N
cinco = S cuatro

predecesor :: N -> N
predecesor = \n -> case n of {O -> O; S x -> x}

instance Eq N where 
    (==) = \n -> \m -> case n of {
        O -> case m of {
            O -> True;
            S y -> False
        };
        S x -> case m of {
            O -> False;
            S y -> x == y
        };
    }

instance Ord N where
    (<=) :: N -> N -> Bool
    (<=) = \n -> \m -> case n of {
        O -> True;
        S x -> case m of {
            O -> False;
            S y -> x <= y
        };
    }

minimo :: N -> N -> N
minimo = \n -> \m -> case n of {
    O -> O;
    S x -> case m of {
        O -> O;
        S y -> S (minimo x y)
    };
}

maximo :: N -> N -> N
maximo = \n -> \m -> case n of {
    O -> m;
    S x -> case m of {
        O -> n;
        S y -> S (maximo x y)
    };
}

min3 :: N -> N -> N -> N
min3 = \n1 -> \m -> \n2 -> minimo n1 (minimo m n2);


instance Num N where
    (+)= \n -> \m -> case n of {
        O -> m;
        S x -> case m of {
            O -> n;
            S y -> S (S (y + x))
        }
    }
    (-)= \n -> \m -> case m of {
        O -> n;
        S x -> case n of {
            O -> O;
            S y -> (y - x)
        }
    } 
    (*)= \n -> \m -> case n of {
        O -> O;
        S x -> case m of {
            O -> O;
            S y -> S y + (x * S y)
        }
    } 

(%) :: N -> N -> N
(%)= \n -> \m -> case m of {
    O -> S O;
    S x -> n * (n % x)
    }
 
doble ::  N -> N
doble = \n -> n * dos

sumi ::  N -> N
sumi = \n -> case n of {
    O -> O;
    S x -> S x + (sumi x)
}

fact :: N -> N
fact = \n -> case n of {
    O -> S O;
    S x -> S x * (fact x)
}

sumfi :: (N -> N) -> N -> N
sumfi = \f -> \n -> case n of {
    O -> f O;
    S x -> f (S x) + (sumfi f x)
}

sumpi :: (N -> Bool) -> N -> N
sumpi = \p -> \n -> case n of {
    O -> O;
    S x -> case p (S x) of {
        False -> sumpi p x;
        True -> S x + (sumpi p x)
    }
}

par :: N -> Bool
par = \n -> case n of {
    O -> True;
    S x -> not (par x)
}

impar :: N -> Bool
impar = \n -> not (par n)

--sumpfi toma un natural, un predicado y una funcion; hace una sumatoria de (f n) de todos los n que cumplen el predicado

sumpfi :: N -> (N -> Bool) -> (N -> N) -> N
sumpfi = \n -> \p -> \f -> case n of {
    O -> case (p O) of {
        True -> f O;
        False -> O
        };
    S x -> case p (S x) of {
        True -> (f (S x)) + (sumpfi x p f);
        False -> sumpfi x p f
        }
    }

--sumfpi toma un natural, una funcion y un predicado; hace una sumatoria de todos los (f n) que cumplen p (f n)

sumfpi :: N -> (N -> N) -> (N -> Bool) -> N
sumfpi = \n -> \f -> \p -> case n of {
    O -> case (p (f O)) of {
        True -> f O;
        False -> O
        };
    S x -> case (p (f (S x))) of {
        True -> (f (S x)) + sumfpi x f p;
        False -> sumfpi x f p
        }
    }
