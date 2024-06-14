{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

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
-- Previo a las resoluciones de ejericios se volveran a definir funciones auxiliares empleadas en las resoluciones de los problemas
map :: (a -> b) -> [a] -> [b]
map = undefined

sum :: Num a => [a] -> a
sum = undefined

	
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
csys = C "sys" [ A ("sys", Txt), csys]

craiz :: FS
craiz = C "raiz" [cmusica, A ("notas", Txt), cort, csys]

----
-- 2

nombre :: FS -> Nombre
nombre = \f -> case f of{
  C n l -> n;
  A (n,e) -> case e of {
    Txt -> n + ".txt";
    Mp3 -> n + ".mp3";
    Jar -> n + ".jar";
    Doc -> n + ".doc";
    Hs -> n + ".hs"
  }
}

----
-- 3

contenido :: FS-> [Nombre]
contenido = \f -> case f of{
  A (n,e) -> error "no es una carpeta";
  C n l -> l
}

----
-- 4

cantA :: FS -> Int 
cantA = \f -> case f of{
  A (n,e) -> S(O)
  C n l -> sum (map cantA fs)

----
-- 5

pertenece :: Nombre -> FS -> Bool 
pertenece = undefined

----
-- 6
valido :: FS-> Bool
valido = undefined
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

Hola :: FS
Hola = undefined