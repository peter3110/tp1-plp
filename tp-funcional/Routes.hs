module Routes where

import Data.List
import Data.Maybe

data PathPattern = Literal String | Capture String deriving (Eq, Show)

data Routes f = Route [PathPattern] f | Scope [PathPattern] (Routes f) | Many [Routes f] deriving Show

-- Ejercicio 1: Dado un elemento separador y una lista, se debera partir la lista en sublistas de acuerdo a la aparicíon del separador (sin incluirlo).
--              Para contemplar el caso base que se nos pide, debemos devolver [] si nos pasan la lista vacia.
--              Si la lista que nos pasan no es vacia, la recorremos y cada vez que aparece un elemento igual al que nos pasan, agrego la sublista que
--              estaba generando en ese momento para la solucion final, y el resto de las sublistas las voy a agregar recursivamente a partir de la
--              porcion de lista aun no recorrida.
split :: Eq a => a -> [a] -> [[a]]
split n xs = if length xs /= 0 then split' n xs else []

split':: Eq a => a -> [a] -> [[a]]
split' n = foldr (\x r -> if (x/=n) then (\yss -> (x:head yss):(tail yss)) r else [[]] ++ r) [[]]

-- Ejercicio 2: A partir de una cadena que denota un patrón de URL se deberá construir la secuencia de literales y capturas correspondiente.
--              Como 'split' devuelve cadenas vacias cuando hay uso inadecuado de las barras "//", procesamos la particion del string a partir de 
--              la barra para tener una lista de literales y/o capturas representados de forma aceptable. A partir de esta lista de strings no
--              vacios podemos generar el [PathPattern] que se pide.
pattern :: String -> [PathPattern]
pattern xs = pattern' $ (foldr (\x r -> if x=="" then r else x:r) []) (split '/' xs)

pattern' :: [String] -> [PathPattern]
pattern' = (foldr (\x r -> if ((not (null x)) && (head x) == ':') then (Capture (tail x)):r 
               else (Literal x):r) [])

-- Ejercicio 3: Obtiene el valor registrado en una captura determinada. Se puede suponer que la captura está definida en el contexto.
--              A raiz de esta suposicion, sabemos que habra algun elemento del PathContext que se corresponda con el string que nos pasan.
--              Luego, cuando encontramos ese string devolvemos a su pareja.
type PathContext = [(String, String)]

get :: String -> PathContext -> String
get s = foldr (\(key,value) r -> if key==s then value else r) s

-- Ejercicio 4: Dadas una ruta particionada y un patrón de URL, trata de aplicar el patrón a la ruta y devuelve, en caso de que
--              la ruta sea un prefijo válido para el patrón, el resto de la ruta que no se haya llegado a consumir y el contexto capturado 
--              hasta el punto alcanzado. Se puede usar recursión explícita.

-- unir : lo usamos para agregar adelante del PathContext resultante del procesamiento de una ruta 
--        el resultado de haber procesado algun elemento de la misma. Lo utilizamos como funcion auxiliar de 'matches'
unir :: PathContext -> Maybe ([String], PathContext) -> Maybe ([String], PathContext)
unir pc Nothing = Nothing
unir pc (Just (ss,xs)) = Just (ss,pc++xs)

-- matches : asumimos que si el PathContext tiene pide mas informacion de la que la ruta
--           nos puede brindar, devolvemos Nothing. A los literales y capturas los consumimos, y
--           a las capturas ademas las procesamos y agregamos adelante en el resultado final, pues son los datos que debemos guardar.
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


-- Ejercicio 6: Genera todos los posibles paths para una ruta definida.
--              caso Route: devolvemos la unica ruta que se puede generar
--              caso Scope: a cada ruta generada en la recursion  sobre la ruta que toma Scope como parametro, se le agrega adelante
--                          el string patron que Scope considera
--              caso Many: como son todos resultados independientes, la concatenacion de estos son todos los paths que se pueden generar
paths :: Routes a -> [String]
paths = foldRoutes (\xs f -> [patternShow xs]) 
                   (\xs r -> map (\x -> if x==[] then ((patternShow xs) ++ x) else (((patternShow xs) ++ "/") ++ x)) r) 
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
--              Es evidente: si se obtiene una funcion al evaluar path, se aplica dicha funcion sobre el PathContext correspondiente 
--              a la evaluacion de path.
exec :: Routes (PathContext -> a) -> String -> Maybe a
exec routes path = (\px -> Just ((fst px) (snd px))) =<< (eval routes path)

-- Ejercicio 9: Permite aplicar una funcion sobre el handler de una ruta. Esto, por ejemplo, podría permitir la ejecución 
--              concatenada de dos o más handlers.
--              No hace falta explicarlo
wrap :: (a -> b) -> Routes a -> Routes b
wrap f = foldRoutes (\xs g -> Route xs (f g))
                    (\xs r -> Scope xs r)
                    (\lr   -> Many lr)

-- Ejercicio 10: Genera un Routes que captura todas las rutas, de cualquier longitud. A todos los patrones devuelven el mismo valor. 
--               Las capturas usadas en los patrones se deberán llamar p0, p1, etc. 
--               En este punto se permite recursión explícita.
catch_all :: a -> Routes a
catch_all h = undefined

