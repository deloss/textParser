module Etapa3 where

import Tipos
import Data.Char (isSpace)

isOK :: Opcion -> Bool
isOK (OK x) = True
isOK (NOK x) = False

esputear :: Respuesta -> Respuesta
esputear (ESSAY) = ESSAY
esputear (FV bool) = FV bool
esputear (MO (xss)) =  if length (filter (not.isOK) xss) >= 0 then SHORT xss else  MO xss

modificarRespuesta :: QA -> (Respuesta -> Respuesta) -> QA
modificarRespuesta (Q x) _ =   Q x
modificarRespuesta (A x) f =  A $ f x

modificarCuerpo :: [ QA ] -> (Respuesta -> Respuesta) -> [ QA ]
modificarCuerpo xss f = [ modificarRespuesta x f | x <- xss ]

modificarEjercicio :: Ejercicio QA -> (Respuesta -> Respuesta) -> Ejercicio QA
modificarEjercicio (Ejercicio cUno nombre cuerpo cDos) f = Ejercicio cUno nombre ( modificarCuerpo cuerpo f) cDos

---FUNCION 1
mo2short  :: Cuestionario QA -> Cuestionario QA
mo2short xss = [ modificarEjercicio x esputear | x <- xss ]

ordenarOpciones ::[ Opcion ] -> [ Opcion ]
ordenarOpciones (xss) = sonOK ++ sonNOK
		where 
		sonOK = [ x | x <- xss, isOK x ]
		sonNOK = [ x | x <- xss, not (isOK x) ]

ordenar :: Respuesta -> Respuesta
ordenar (ESSAY) = ESSAY
ordenar (FV bool) = FV bool
ordenar (SHORT (xss)) = SHORT (xss)
ordenar (MO (xss))  =  MO (ordenarOpciones xss)
---FUNCION 2	
sortMO    :: Cuestionario QA -> Cuestionario QA
sortMO (xs) = [ modificarEjercicio x ordenar | x <- xs ]

removerEspaciosComienzo :: String -> String
removerEspaciosComienzo "" = ""
removerEspaciosComienzo (x:xs) 
	| isSpace x = removerEspaciosComienzo xs
	| otherwise = xs

removerEspaciosComienzoYFin :: String -> String
removerEspaciosComienzoYFin "" = ""
removerEspaciosComienzoYFin xs = (reverse.removerEspaciosComienzo.reverse.removerEspaciosComienzo) xs	 

removerEspaciosFragmento :: Fragmento -> Fragmento
removerEspaciosFragmento (TXT str) = TXT (removerEspaciosComienzoYFin str)
removerEspaciosFragmento (MATH str) = MATH (removerEspaciosComienzoYFin str)
removerEspaciosFragmento (CODE str) = CODE (removerEspaciosComienzoYFin str)

removerEspaciosOpcion :: Opcion -> Opcion
removerEspaciosOpcion (OK xss) = OK (map removerEspaciosFragmento xss)
removerEspaciosOpcion (NOK xss) = NOK (map removerEspaciosFragmento xss) 

quitarEspacios :: Respuesta -> Respuesta
quitarEspacios (ESSAY) = ESSAY
quitarEspacios (FV bool) = FV bool
quitarEspacios (SHORT (xss)) = SHORT (map removerEspaciosOpcion xss)
quitarEspacios (MO (xss)) = MO (map removerEspaciosOpcion xss)

---FUNCION 3
trim      :: Cuestionario QA -> Cuestionario QA
trim xss = [ modificarEjercicio x quitarEspacios | x <- xss ]

-- pegar aca cosas de nodupMO
removerMasDeUnEspacio :: String -> String
removerMasDeUnEspacio "" = ""
removerMasDeUnEspacio (x:y:xs)
  | isSpace x && isSpace y = x:(removerMasDeUnEspacio xs)
  | otherwise = x:(removerMasDeUnEspacio (y:xs))

removerVariosEspaciosFragmento :: Fragmento -> Fragmento
removerVariosEspaciosFragmento (TXT str) = TXT (removerMasDeUnEspacio str)
removerVariosEspaciosFragmento (MATH str) = MATH (removerMasDeUnEspacio str)
removerVariosEspaciosFragmento (CODE str) = CODE (removerMasDeUnEspacio str)

removerVariosEspaciosOpcion :: Opcion -> Opcion
removerVariosEspaciosOpcion (OK xss) = OK (map removerVariosEspaciosFragmento xss)
removerVariosEspaciosOpcion (NOK xss) = NOK (map removerVariosEspaciosFragmento xss) 


-- elimina todos los elementos de ys que sean iguales a elementos de xs
removerOpciones :: [Opcion] -> [Opcion] -> [Opcion]
removerOpciones xs ys = filter (\e -> not (e `elem` xs)) ys

---FUNCION 4
eliminarOpcionesRepetidas :: [Opcion] -> [Opcion]
eliminarOpcionesRepetidas ([]) = []
eliminarOpcionesRepetidas (x:xs)
  | length (iguales) > 0 = x:(eliminarOpcionesRepetidas $ removerOpciones iguales xs)
  | otherwise = x:(eliminarOpcionesRepetidas xs)
    where iguales = filter (== removerVariosEspaciosOpcion x) $ map (removerVariosEspaciosOpcion) xs

eliminarRepetidos :: Respuesta -> Respuesta
eliminarRepetidos (MO a) = MO (eliminarOpcionesRepetidas a)
eliminarRepetidos (SHORT a) = SHORT (eliminarOpcionesRepetidas a)
eliminarRepetidos x = x


nodupMO :: Cuestionario QA -> Cuestionario QA
nodupMO xss = [ modificarEjercicio x eliminarRepetidos | x <- xss ]
--

isFV :: Respuesta -> Bool
isFV (ESSAY) = False
isFV (FV bool) = True
isFV (SHORT (xss)) = False
isFV (MO (xss))  =  False

valorFV' :: Respuesta -> Bool
valorFV' (FV bool) = bool

buscarRespuestasFV :: QA -> (Respuesta -> Bool) -> Bool
buscarRespuestasFV (Q x) _ = False
buscarRespuestasFV (A x) f = f x

getRespuestasFV'' :: [ QA ] -> [ Bool ]
getRespuestasFV'' xss =  [buscarRespuestasFV x valorFV'  | x <- xss,buscarRespuestasFV x isFV]

getRespuestasFV' :: Ejercicio QA -> [Bool]
getRespuestasFV' (Ejercicio _ _ cuerpo _) = getRespuestasFV'' cuerpo

getRespuestasFV ::  Cuestionario QA -> [Bool]
getRespuestasFV [] = []
getRespuestasFV (x:xs) = getRespuestasFV' x ++ getRespuestasFV xs

isPobre :: [Bool] -> Bool
isPobre xss
	| x == 1 || x == 0 = False
	| x == length (filter (==True) xss) = True
	| x == length (filter (==False) xss) = True
	| otherwise = False
	where x = length xss

retirarFV'' :: [ QA ] -> [ QA ]
retirarFV'' xss = [ x | x <- xss, not (buscarRespuestasFV x isFV) ] 

retirarFV' :: Ejercicio QA -> Ejercicio QA
retirarFV' (Ejercicio cUno nombre cuerpo cDos) = Ejercicio cUno nombre ( retirarFV'' cuerpo) cDos

retirarFV :: Cuestionario QA -> Cuestionario QA
retirarFV xss = [ retirarFV' x | x <- xss ]

---FUNCION 5
filtroFV  :: Cuestionario QA -> Cuestionario QA
filtroFV xss 
	| isPobre rfv = retirarFV xss
	| otherwise = xss
	where rfv =  getRespuestasFV xss 
	      






















transformaciones = mo2short . sortMO . nodupMO . filtroFV . trim
