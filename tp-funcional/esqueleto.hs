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
                        route "inscribir" "inscribe alumno", 
                        route "aprobar/:nota"   "aprueba alumno",
                        scope "detener/:causa" (route "castigo" "aplicar castigo ejemplar")],
                route "alu/:lu/aprobadas" "ver materias aprobadas por alumno" ]


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

-- eval : Primero, trabajamos con el path en su forma partida, para mayor prolijidad del codigo
--        caso Route [PathPattern] f : solo en caso de que el path considerado (ss) alcance para consumir todo el [PathPattern] considerado (xs),
--             (esto lo chequeamos con el matches ss xs) y si el path en consumido en su totalidad (null x), entonces devolvemos la funcion
--             correspondiente a la evaluacion de dicho path.
--        caso Scope [PathPattern] (Routes f) : primero se chequea que el path ss que nos pasan matchee con el [PathPattern] xs. Esto lo hacemos
--             con "matches ss xs". Si esto sucede, nos fijamos si la porcion del path ss que no se pudo consumir con xs (en el codigo, la x),
--             si esa porcion de string matchea con la ruta considerada dentro del Scope. Esto lo chequeamos con r (notar que 
--             fr :: [String] -> Maybe (a, PathContext)). Si efectivamente, en la recursion se logra matchear lo que queda del string ss con 
--             cierta ruta, devolvemos la funcion correspondiente al desarrollo de dicha ruta, y concatenamos al PathContext generado por 
--             esa ruta al PathContext que habiamos obtenido al consumir el path ss que nos habian pasado. Esto lo hacemos con y++y', donde y es lo
--             consumido por ss antes de entrar a Scope y y' lo consumido por ss ya dentro de la ruta del Scope. 
--        caso Many [Routes f] : se tiene una lista de resultados, candidatos a matchear con el path que nos pasan. si alguno de estos 
--             matchea con el path ss, tomamos ese como resultado. Si no matchea con ninguno, devuelve Nothing pues quiere decir que el path
--             ss no se corresonde con ninguno de los resultados disponibles. Notar que fx :: String -> Maybe (a,PathContext)

eval :: Routes a -> String -> Maybe (a, PathContext)
eval route s = eval' route (split '/' s)

eval' :: Routes a -> [String] -> Maybe (a, PathContext)
eval' = foldRoutes (\xs f  -> (\ss -> (\(x,y) -> if null x then Just (f,y) else Nothing) =<< (matches ss xs)))
                   (\xs fr -> (\ss -> (\(x,y) -> (\(x',y') -> Just (x', y++y')) =<< (fr x)) =<< (matches ss xs)))
                   (\lr    -> (\ss -> (foldr (\fx r -> if isNothing (fx ss) then r else (fx ss)) Nothing) lr))

-- Ejercicio 8: Similar a eval, pero aquí se espera que el handler sea una función que recibe como entrada el contexto 
--              con las capturas, por lo que se devolverá el resultado de su aplicación, en caso de haber coincidencia.
exec :: Routes (PathContext -> a) -> String -> Maybe a
exec routes path = (\px -> Just ((fst px) (snd px))) =<< (eval routes path)

-- Ejercicio 9: Permite aplicar una funci ́on sobre el handler de una ruta. Esto, por ejemplo, podría permitir la ejecución 
--              concatenada de dos o más handlers.
wrap :: (a -> b) -> Routes a -> Routes b
wrap f = foldRoutes (\xs g -> Route xs (f g))
                    (\xs r -> Scope xs r)
                    (\lr   -> Many lr)

-- Ejercicio 10: Genera un Routes que captura todas las rutas, de cualquier longitud. A todos los patrones devuelven el mismo valor. 
--               Las capturas usadas en los patrones se deberán llamar p0, p1, etc. 
--               En este punto se permite recursión explícita.
catch_all :: a -> Routes a
catch_all h = undefined
