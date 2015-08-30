module Routes where

import Data.List
import Data.Maybe

data PathPattern = Literal String | Capture String deriving (Eq, Show)

data Routes f = Route [PathPattern] f | Scope [PathPattern] (Routes f) | Many [Routes f] deriving Show

-- Ejercicio 1: Dado un elemento separador y una lista, se debera partir la lista en sublistas de acuerdo a la aparicíon del separador (sin incluirlo).

split :: Eq a => a -> [a] -> [[a]]
split n = foldr (\x r -> if (n/=x) then (\yss -> (x:head yss):(tail yss)) r
                                   else [[]] ++ r) [[]]

-- Ejercicio 2: A partir de una cadena que denota un patrón de URL se deberá construir la secuencia de literales y capturas correspondiente.

limpiar :: Eq a => [[a]] -> [[a]]
limpiar = foldr (\x r -> if x==[] then r else x:r) []

pattern :: String -> [PathPattern]
pattern path = if path == "" then [Literal ""] else
               (foldr (\x r -> if ((not (null x)) && (head x) == ':') then (Capture (tail x)):r 
                               else (Literal x):r) []) (limpiar (split '/' path))

-- Ejercicio 3: Obtiene el valor registrado en una captura determinada. Se puede suponer que la captura está definida en el contexto.
type PathContext = [(String, String)]

get :: String -> PathContext -> String
get s = foldr (\(key,value) r -> if key==s then value else r) s

-- Ejercicio 4: Dadas una ruta particionada y un patrón de URL, trata de aplicar el patrón a la ruta y devuelve, en caso de que
--              la ruta sea un prefijo válido para el patrón, el resto de la ruta que no se haya llegado a consumir y el contexto capturado hasta el punto alcanzado.
-- Se puede usar recursión explícita.

-- unir : agrega adelante del resultado final del procesamiento de una ruta 
--        el resultado de haber procesado un elemento de la ruta
unir :: PathContext -> Maybe ([String], PathContext) -> Maybe ([String], PathContext)
unir pc Nothing = Nothing
unir pc (Just (ss,xs)) = Just (ss,pc++xs)

-- matches : asumimos que si el PathContext tiene pide mas informacion de la que la ruta
--           nos puede brindar, devolvemos Nothing. A los literales y capturas los consumimos, y
--           solo que a las capturas ademas las procesamos y agregamos adelante en el resultado final.
--           (Se permite usar recursion explicita)
matches :: [String] -> [PathPattern] -> Maybe ([String], PathContext)
matches ss [] = Just (ss, [])
matches [] xs = Nothing
matches (s:ss) ((Literal x):xs) = if s==x then (matches ss xs) else Nothing
matches (s:ss) ((Capture x):xs) = unir [(x,s)] (matches ss xs)

-- DSL para rutas
route :: String -> a -> Routes a
route s f = Route (pattern s) f

scope :: String -> Routes a -> Routes a
scope s r = Scope (pattern s) r

many :: [Routes a] -> Routes a
many l = Many l

-- Ejercicio 5: Definir el fold para el tipo Routes f y su tipo. Se puede usar recursión explícita.
-- Routes f = Route [PathPattern] f | Scope [PathPattern] (Routes f) | Many [Routes f]
foldRoutes :: ([PathPattern] -> b -> c) -> ([PathPattern] -> c -> c) -> ([c] -> c) -> Routes b -> c
foldRoutes fRoute fScope fMany route = case route of
    Route xs f  -> fRoute xs f 
    Scope xs r  -> fScope xs (rec $ r)
    Many  lr    -> fMany (map rec lr)
    where rec = foldRoutes fRoute fScope fMany


-- Auxiliar para mostrar patrones. Es la inversa de pattern.
patternShow :: [PathPattern] -> String
patternShow ps = concat $ intersperse "/" ((map (\p -> case p of
  Literal s -> s
  Capture s -> (':':s)
  )) ps)

rutasFacultad = many [
                route "" "ver inicio",
                route "ayuda" "ver ayuda",
                scope "materia/:nombre/alu/:lu" $ many [
                        route "inscribir" "inscribe alumno 1", 
                        route "aprobar1/:nota"   "aprueba alumno 1"],
                scope "materia2" $ many [
                        route "inscribir" "inscribe alumno", 
                        route "aprobar2/:nota"   "aprueba alumno 2"],
                route "alu/:lu/aprobadas" "ver materias aprobadas por alumno 2" ]

rutasFac      =  many [
                  route "" "ver inicio",
                  route "ayuda" "ver ayuda",
                  route "alu/:lu/:edad/aprobadas" "ver materias aprobadas por alumno" ]


-- Ejercicio 6: Genera todos los posibles paths para una ruta definida.
paths :: Routes a -> [String]
paths = foldRoutes (\xs f -> [patternShow xs]) (\xs r -> map (((patternShow xs)++"/")++) r) 
                   (\xs -> concat xs)

-- Ejercicio 7: Evalúa un path con una definición de ruta y, en caso de haber coincidencia, obtiene el handler correspondiente 
--              y el contexto de capturas obtenido.
{-
Nota: la siguiente función viene definida en el módulo Data.Maybe.
 (=<<) :: (a->Maybe b)->Maybe a->Maybe b
 f =<< m = case m of Nothing -> Nothing; Just x -> f x
-}

-- Notas: _ [patternShow xs] es para pasar de lista de PathPattern a lista de String (sacar Literal y Capture)
--        _ i)   solo en caso de que el String matchee exactamente con la ruta, devolemos la funcion f que corresponde a esa ruta (que es completa)
--        _ ii)  
--        _ iii) tengo una lista de respuestas. si alguna matchea con s, tomo esa respuesta. Si no, devuelvo Nothing

-- r :: String -> Maybe (a, PathContext)

unirConBarra :: [String] -> String
unirConBarra = foldr (\x r -> if r=="" then x else x ++ "/" ++ r) ""

-- modificar px sy = la porcion aun no consumida de sy tras haber consumido todo lo de px posible
--                   Si no se pudo consumir todo px, devuelve "". Y si no se puede consumir nada de px, devuelve sy
-- Problema : s varia. Como hago para no usar recursion explicita?
modificar :: [PathPattern] -> String -> String
modificar [] s = s
modificar _ "" = ""
modificar (Literal x:xs) s = if x==head(split '/' s) then modificar xs (unirConBarra (tail (split '/' s))) else s
modificar (Capture x:xs) s = modificar xs (unirConBarra ((\y ys -> tail ys) x (split '/' s)))

-- procesar px sy = los datos obtenidos de px tras recorrer lo maximo posible de sy
procesar :: [PathPattern] -> String -> PathContext
procesar [] s   = []
procesar (Literal x:xs) s = procesar xs (unirConBarra (tail (split '/' s)))
procesar (Capture x:xs) s = ([(x,head listaS)] ++ (procesar xs (unirConBarra (tail listaS)))) where listaS = (split '/' s)


eval :: Routes a -> String -> Maybe (a, PathContext)
eval = foldRoutes (\xs f  -> \s -> (\(x,y) -> Just (f,y)) =<< (matches (split '/' s) xs))
                  (\xs r  -> \s -> (\(x,y) -> if modificar xs s == s then Nothing 
                                              else Just (x,(procesar xs s)++y)) =<< r (modificar xs s))
                  (\lr    -> \s -> (\recOk -> recOk s) =<< (foldr (\x r -> if isNothing (x s) then r else Just x) Nothing) lr)


-- Ejercicio 8: Similar a eval, pero aquí se espera que el handler sea una función que recibe como entrada el contexto 
--              con las capturas, por lo que se devolverá el resultado de su aplicación, en caso de haber coincidencia.
exec :: Routes (PathContext -> a) -> String -> Maybe a
exec routes path = undefined

-- Ejercicio 9: Permite aplicar una funci ́on sobre el handler de una ruta. Esto, por ejemplo, podría permitir la ejecución 
--              concatenada de dos o más handlers.
wrap :: (a -> b) -> Routes a -> Routes b
wrap f = undefined

-- Ejercicio 10: Genera un Routes que captura todas las rutas, de cualquier longitud. A todos los patrones devuelven el mismo valor. 
--               Las capturas usadas en los patrones se deberán llamar p0, p1, etc. 
--               En este punto se permite recursión explícita.
catch_all :: a -> Routes a
catch_all h = undefined
