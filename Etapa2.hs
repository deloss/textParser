module Etapa2 where

import Debug.Trace (trace)
import Data.Char (isSpace)
import Tipos
import Etapa1

--- El programa leeMOE lee una lista hasta encontrar una marca que no
--- esté protegida por un ESCAPE. Si no encuentra
--- marcas, devuelve Nothing en el segundo argumento y la lista vacía en el
--- tercero. Si el ESCAPE es el ultimo elemento, devuelve Nothing.

leeMOE :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe ([a], Maybe a, [a])
leeMOE m e [] = Just ([], Nothing, [])
leeMOE m e [x]
   | e x       = Nothing
   | m x       = Just ([ ],  Just x, [])
   | otherwise = Just ([x], Nothing, [])
leeMOE m e (x:y:xs) 
   | m x = Just ([], Just x, y:xs)
   | m y && e x = case leeMOE m e xs of
		   Nothing -> Nothing
                   Just (antes,marca,resto) -> case marca of
                      				Nothing -> Just (x:y:antes, Nothing, []) 
                      				Just m -> Just (x:y:antes, Just m, resto)

   | otherwise = case leeMOE m e (y:xs) of
		   Nothing -> Nothing
                   Just (antes,marca,resto) -> case marca of
                      				Nothing -> Just (x:antes, Nothing, []) 
                      				Just m -> Just (x:antes, Just m, resto)

--- El programa leeMXE lee una lista hasta encontrar una marca que no
--- esté protegida por un ESCAPE. Si no encuentra
--- marcas, devuelve Nothing. Si el ESCAPE es el ultimo elemento,
--- devuelve Nothing.

leeMXE :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe ([a], a, [a])
leeMXE m e [] = Nothing
leeMXE m e [x]
   | e x       = Nothing
   | m x       = Just ([ ],x, [])
   | otherwise = Nothing
leeMXE m e (x:y:xs) 
   | m x = Just ([], x, y:xs)
   | m y && e x = case leeMXE m e xs of
		   Nothing -> Nothing
                   Just (antes,marca,resto) -> Just (x:y:antes, marca, resto)
   | otherwise = case leeMXE m e (y:xs) of 
		  Nothing -> Nothing
                  Just (antes,marca,resto) -> Just (x:antes, marca, resto)

--- El programa readFV recibe un String, y consume del mismo
--- una palabra que representa un valor booleano de acuerdo
--- a la gramatica de GIFT*.

readFV :: String -> Maybe (Bool, String)
readFV xs = do
              (principio, marca, resto) <- leeMX (\z -> z == '}') xs 
              if (filter (/=' ') principio )`elem` wTrue then Just (True, resto)
              else if (filter (/=' ') principio )`elem` wFalse then Just (False, resto)
              else Nothing 
  where wTrue  = [ "VERDADERO", "VERDAD", "V", "TRUE", "T" ]
        wFalse = [ "FALSO", "FALSE", "F" ]


getSeq :: ( a -> Maybe (b, a) ) -> a -> Maybe ([b], a)
getSeq f xs =
  case (f xs) of
       Nothing       -> do
			   	--output <- trace ("termine") (return xs)
				Just ([], xs)
       Just (fx, zs) -> do 
			  -- output <- trace ("estoy antes del llamado recursivo "    ) (return xs)
			   (bs, a) <- getSeq f zs-- a tiene un /n?
			   --output <- trace ("estoy despues del llamado recursivo " ) (return xs)
                           return (fx:bs, a)



---- str2qas consume todo un string y lo convierte en [ QA ]
---- Devuelve Nothing si no se puede consumir toda la entrada.

str2qas :: String -> Maybe [ QA ]
str2qas xs = do 
		output <- trace ("estoy antes del getsez str2qas " ++ xs) (return xs)
		(qs, zs) <- getSeq str2qa xs --nunca sale de aca
		output <- trace ("estoy despues del getseq str2qas " ++ zs ++ "AJAJAJANOOOOOO") (return xs)
                if (zs /= "") then Nothing else Just qs

---- Un QAs puede comenzar con una respuesta (marcada { ... }) o con
---- una pregunta (sin  marcas).


str2qa :: String -> Maybe (QA, String)
str2qa []    = Nothing
str2qa (x:xs)
      | x == '{' = do 
                      output <- trace ("ante de str2a en str2qa " ++ show (xs) ++"jajaja") (return xs)    
                      (respuesta, resto) <- str2a (dropWhile (\z -> isSpace z && z /= '\n') xs)
                      output <- trace ("despues de str2a en str2qa " ++ show resto ++"jajaj") (return resto)
                      return ( A respuesta, resto) --le devuelve /n a getsez
      | otherwise    = do 
		          output <- trace ("ante de str2q en str2qa " ++ show (xs) ++"jajaja") (return xs)				
			  (pregunta, resto) <- str2q (dropWhile (\z -> isSpace z && z /= '\n') xs)
		          output <- trace ("despues de str2q en str2qa " ++ show resto ++"jajaj") (return resto)
			  return ( Q pregunta, resto)

---- str2q procesa una Pregunta.

str2q :: String -> Maybe ( Pregunta  , String )
str2q = getSeq getFragmento 

---- Analizador de Fragmento
---- No se aceptan caracteres especiales sin escape dentro de un fragmento.
---- Los mismos son: {, }, =, ~

getFragmento :: String -> Maybe (Fragmento, String)
getFragmento      ""  = Nothing
getFragmento ( x :xs)
    | x `elem` ['{', '}', '=', '~'] =  Nothing
    | x == '$'                      = str2Math    xs
    | x == '`'                      = str2Code    xs
    | otherwise                     = str2Txt	  (x:xs)

--- Los MATH contienen cualquier caracter con escape que no sea $
str2Math :: String -> Maybe ( Fragmento , String )
str2Math str = do (math, _ , zs) <- leeMXE marca escape str
                  return ( MATH math , zs )
         where marca x = x == '$'

--- Los CODE contienen cualquier caracter con escape que no sea `
str2Code :: String -> Maybe ( Fragmento , String )
str2Code str = do (code, _ , zs) <- leeMXE marca escape str
                  return ( CODE code , zs )
         where marca x = x == '`'

procesarSaltosDeLinea :: String -> String
procesarSaltosDeLinea "" = ""
procesarSaltosDeLinea (x:xs) 
  | x == '\n' = "\n" ++ procesarSaltosDeLinea xs
  | otherwise = x:procesarSaltosDeLinea xs

--- Los TXT en Q contienen cualquier caracter con escape que no sea ` $ {
str2Txt :: String -> Maybe ( Fragmento , String )
str2Txt ['\n'] = Just (TXT "\n", [])
str2Txt str = do
                (txt, marca , zs) <- leeMXE marca escape $ procesarSaltosDeLinea str
		 	  --output <- trace ("estoy en txt " ++ zs ++ "probando" ++ marca:"gg" ++ "probando2"++zs++"probando3") (return zs)
                return ( TXT txt , marca:zs )

        where marca x = x `elem` ['`','$','{','}','~','=']


---- str2a procesa una respuesta.

str2a :: String -> Maybe ( Respuesta , String )
str2a    ""  = Nothing
str2a xxs@(x:xs)
  | isSpace x      = str2a xs
  | x == '='       = do 
                        (op, zs) <- getSeq getFragmento xs
                        case str2a zs of
                          Just (MO opciones, _) -> Just (MO (OK op : opciones), zs)
                          otherwise -> Nothing
  | x == '~'       = do 
                        (op, zs) <- getSeq getFragmento xs
                        case str2a zs of
                          Just (MO opciones, _) -> Just (MO (NOK op : opciones), zs)
                          otherwise -> Nothing

  | x == '}'       = Just (ESSAY ,xs)
  | otherwise      = do 
			--output <- trace ("estoy en str2a y voy a entrar a readfv " ++ show ( head xs)) (return xs)
			(b, ys)     <- readFV xxs
		        --output <- trace ("estoy en str2a y voy a salir de  readfv" ++ ys ++ "probando") (return ys) --aca llega
                        return (FV b, ys)

leerOpcion :: String -> Maybe ( Opcion , String )
leerOpcion ('}':xs) = Nothing
leerOpcion ('=':xs) = do (op, zs) <- getSeq getFragmento xs
                         return (OK op , zs)
leerOpcion ('~':xs) = do (op, zs) <- getSeq getFragmento xs
                         return (NOK op , zs)

--- Un QA puede abarcar varias líneas. Termina en un comentario.
--- Se devuelve una única línea.

instance CCuerpo QA where
   getCuerpo xs = do 
			(ys, zs) <- getCuerpo xs
			--output <- trace ("antes de str2qa " ++ ys) (return ys)
			qas <- str2qas (ys::Cuerpo Char)
			--output <- trace ("AJAJAJAJAJAJA " ++ show ( head ys)) (return ys)
			return (qas, zs)
			

--- Marca para detectar el escape

escape = (== '\\')
