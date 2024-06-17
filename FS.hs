{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use lambda" #-}

module FS where 

import Prelude

-- Nombre: Santiago Cajal
-- Número: 282766

-- Nombre: Miles "Spiderman" Morales
-- Número: 424242


type Nombre = String 

data Ext where { Txt::Ext ; Mp3::Ext ; Jar::Ext ; Doc::Ext ; Hs::Ext }
  deriving (Eq, Show)


data FS where {  A :: (Nombre,Ext) -> FS;
                 C :: Nombre -> [FS] -> FS }
  deriving (Eq, Show)

----
-- 1

cjazz::FS
cjazz = C "jazz" [ A("mumbles",Mp3) ]

crock::FS
crock = C "rock" [ A("clones",Mp3), A("bajan",Mp3), A("clara",Mp3) ]

cmusica::FS
cmusica = C "musica"[ cjazz, crock, A("clara",Mp3) ]

-- Completar el resto de los componentes del FS

cort :: FS
cort = C "ort" [ cobls, A ("notas", Txt)]

cobls :: FS
cobls = C "obls" [ A ("p2", Txt), A ("p2", Jar), A ("fc", Hs)] 

csys :: FS
csys = C "sys" [ A ("sys", Txt), C "sys" []]

craiz :: FS
craiz = C "raiz" [cmusica, A ("notas", Txt), cort, csys]

----
-- 2

stringExtension :: FS -> String
stringExtension = \f -> case f of {
  A (n,e) -> case e of {
    Txt -> ".txt";
    Mp3 -> ".mp3";
    Jar -> ".jar";
    Doc -> ".doc";
    Hs -> ".hs";
  };
  C n l -> error "no tiene extension"
}

nombre :: FS -> Nombre
nombre = \f -> case f of {
    A (n,e) -> n ++ stringExtension (A (n,e));
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
archivosExt = \e -> \f -> case f of {
  A (n,g) -> case ( g == e) of {
    False -> [];
    True -> [n]
  };
  C n l -> case l of {
    [] -> [];
    x:xs -> case x of {
      A (m,g) -> case ( g == e) of {
        False -> archivosExt e (C n xs);
        True -> m:(archivosExt e (C n xs))
      };
      C m g -> (archivosExt e (C m g)) ++ ( archivosExt e (C n xs))
    }
  }
}

----
-- 8

mapArboles :: [FS] ->  ( Nombre -> Nombre -> FS -> FS) -> Nombre -> Nombre -> [FS] 
mapArboles = \l -> \f -> \n -> \m -> case l of {
  [] -> [];
  x:xs -> (f n m x):(mapArboles xs f n m)
}

cambiarNom :: Nombre -> Nombre -> FS -> FS
cambiarNom = \n -> \m -> \f -> case f of {
  A (o,e) -> case ( o == n) of {
    False -> A (o,e);
    True -> A (m,e)
  };
  C o l -> case ( o == n ) of {
    False -> C o (mapArboles l cambiarNom n m);
    True -> C m (mapArboles l cambiarNom n m);
  }
}

----
-- 9

nivelesC :: FS -> Int
nivelesC = \f -> case f of {
  A (n,e) -> 0;
  C n l -> case l of {
    [] -> 1;
    x:xs ->case xs of {
      [] -> 1 +  (nivelesC x);
      y:ys -> case ((nivelesC x) >= (nivelesC y)) of {
        True -> nivelesC (C n (x:ys));
        False -> nivelesC (C n (y:ys))
      }
    }
  }
}

----
-- 10

meterCarpeta :: FS -> FS -> FS
meterCarpeta = \f -> \e -> case f of {
  A (n,e) -> error "no es una carpeta";
  C n l -> C n (e:l)
}

borrar :: Nombre -> FS -> FS
borrar = \n -> \f -> case f of {
  A (m, e) -> A (m, e);
  C m l -> case l of {
    [] -> C m l;
    x:xs -> case x of {
      A (o,e) -> case (o ++ (stringExtension (A (o,e))) == n) of {
        True -> borrar n (C m xs);
        False -> meterCarpeta (borrar n (C m xs)) (A (o,e))
      };
      C o e -> case (o == n) of {
        True -> borrar n (C m xs);
        False -> meterCarpeta (borrar n (C m xs)) (borrar n (C o e))
      }
    }
  }
}

----
-- 11

alfabeticamente :: FS -> FS -> Bool
alfabeticamente = \f -> \g -> case f of {
  A (n,e) -> case g of {
    A (m,f) -> (n ++ (stringExtension (A (n,e)))) <=  (m ++ (stringExtension (A (m,f))));
    C m l ->  (n ++ (stringExtension (A (n,e))) <= m)
  };
  C n l -> case g of {
    A (m,f) -> n <=  (m ++ (stringExtension (A (m,f))));
    C m h -> n <= m
  }
}

insertarOrdenadamente :: FS -> FS -> FS
insertarOrdenadamente = \f -> \g -> case g of {
  A (n,e) -> error "no se puede insertar en un archivo";
  C n l -> case l of {
    [] -> C n [f];
    x:xs -> case (alfabeticamente (f) (x)) of {
      True -> C n (f:x:xs);
      False -> meterCarpeta (insertarOrdenadamente f (C n xs)) x
    }
  }
}

ordenar :: FS-> FS
ordenar = \f -> case f of {
  A (n,e) -> A (n,e);
  C n l -> case l of {
    [] -> C n l;
    x:xs -> case x of {
      A (m,e) -> insertarOrdenadamente (A (m,e)) (ordenar (C n xs));
      C m o -> insertarOrdenadamente (ordenar (C m o)) (ordenar (C n xs))
    }
  }
}

----
-- Extras
-- Funciones auxiliares que cree para otras funciones pero que al final no furon necesarias 

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


sustituirCarpeta :: FS -> [FS] -> FS
sustituirCarpeta = \f -> \e -> case f of {
  A (n,g) -> error "no es una carpeta";
  C n l -> C n e
}




