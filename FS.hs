{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant bracket" #-}

module FS where 

import Prelude

-- Nombre: Estudiante 1
-- Número: Estudiante 1

-- Nombre: Estudiante 2
-- Número: Estudiante 2


type Nombre = String 

data Ext where { Txt::Ext ; Mp3::Ext ; Jar::Ext ; Doc::Ext ; Hs::Ext }
  deriving (Eq, Show)


data FS where {  A :: (Nombre,Ext) -> FS;
                 C :: Nombre -> [FS] -> FS }
  deriving (Eq, Show)

----
-- 1

cjazz :: FS 
cjazz = C "jazz" [A ("mumbles", Mp3)]

crock :: FS 
crock = C "rock" [ A ("clones", Mp3), A ("bajan", Mp3), A ("clara", Mp3)]

cmusica :: FS
cmusica = C "musica" [cjazz, crock, A ("clara", Mp3)]


-- Completar el resto de los componentes del FS

cort :: FS
cort = C "ort" [ cobls, A ("notas", Txt)]

cobls :: FS
cobls = C "obls" [ A ("p2", Txt), A ("notas", Jar), A ("notas", Hs)] 

csys :: FS
csys = C "sys" [ A ("sys", Txt), C "sys" []]

craiz :: FS
craiz = C "raiz" [cmusica, A ("notas", Txt), cort, csys]

----
-- 2

nombre :: FS -> Nombre
nombre = \f -> case f of {
    A (n,e) -> n;
    C m l -> m
}

----
-- 3

contenido :: FS-> [Nombre]
contenido = \f -> case f of{
  A (n,e) -> error "no es una carpeta";
  C m l -> map nombre l
}

----
-- 4

cantA :: FS -> Int
cantA = \f -> case f of{
  A (n,e) -> 1;
  C n l -> sum (map cantA l)
}

----
-- 5

anyArboles :: (a -> b -> Bool) -> a -> [b] -> Bool
anyArboles = \f -> \n -> \l -> case l of {
  [] -> False;
  x:xs -> case f n x of {
    True -> True;
    False -> anyArboles f n xs    
  }
} 

pertenece :: Nombre -> FS -> Bool
pertenece = \n -> \f -> case f of {
  A (m,e) -> n==m;
  C o l -> o==n || anyArboles pertenece n l
}

----
-- 6

existeCarpeta :: FS -> [FS] -> Bool
existeCarpeta = \f -> \l -> case f of {
  A (n,e) -> error "no es carpeta";
  C n f -> case l of {
    [] -> False;
    x:xs -> case x of {
      A (m,e) -> existeCarpeta (C n f) xs;
      C m i -> m == n || existeCarpeta (C n f) xs
    }
  }
}

existeArchivo :: FS -> [FS] -> Bool
existeArchivo = \f -> \l -> case f of {
  A (n,e) -> case l of {
    [] -> False;
    x:xs -> case x of {
      A (m,g) -> (n == m && e==g) || existeArchivo (A (n,e)) xs;
      C m i -> existeArchivo (A (n,e)) xs
    }
  };
  C n l -> error "no es archivo"
}

valido :: FS-> Bool
valido = \f -> case f of {
  A (n,e) -> True;
  C n l -> case l of {
    [] -> True;
    x:xs -> case x of {
      A (m,e) -> case (existeArchivo (A (m,e)) xs) of {
        True -> False;
        False -> valido (C n xs)
      };
      C o i -> case (existeCarpeta (C o i) xs) of {
        True -> False;
        False -> case (valido (C o i)) of {
          True -> valido (C n xs);
          False -> False
        }
      }  
    }
  }
} 

----
-- 7
archivosExt :: Ext -> FS -> [Nombre]
archivosExt = undefined
----
-- 8
cambiarNom :: Nombre -> Nombre -> FS -> FS
cambiarNom = undefined
----
-- 9
nivelesC :: FS -> Int
nivelesC = undefined
----
-- 10
borrar :: Nombre -> FS -> FS
borrar = undefined
----
-- 11
ordenar :: FS-> FS
ordenar = undefined

----
-- Extras

archivosNombresIguales :: FS -> FS -> Bool
archivosNombresIguales = \f -> \g -> case f of {
  C n l -> error "al menos uno de los dos archivos no es una archivo";
  A (n,e) -> case g of {
    C i l -> error "al menos uno de los dos archivos no es una archivo";
    A (m,f) -> m==n && e==f
  }  
}


carpetasNombresIguales :: FS -> FS -> Bool
carpetasNombresIguales = \f -> \g -> case f of {
  A (n,e) -> error "al menos uno de los dos archivos no es una carpeta";
  C n l -> case g of {
    A (n,e) -> error "al menos uno de los dos archivos no es una carpeta";
    C m i -> m==n
  }  
}