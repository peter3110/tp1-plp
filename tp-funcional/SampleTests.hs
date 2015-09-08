import Routes
import Test.HUnit
import Data.List (sort)
import Data.Maybe (fromJust, isNothing    )

rutasFacultad = many [
  route ""             "ver inicio",
  route "ayuda"        "ver ayuda",
  scope "materia/:nombre/alu/:lu" $ many [
    route "inscribir"   "inscribe alumno",
    route "aprobar"     "aprueba alumno"
  ],
  route "alu/:lu/aprobadas"  "ver materias aprobadas por alumno"
  ]

rutasStringOps = route "concat/:a/:b" (\ctx -> (get "a" ctx) ++ (get "b" ctx))

-- evaluar t para correr todos los tests
--t = runTestTT allTests

allTests = test [
    "patterns" ~: testsPattern,
    "get" ~: testsGet,
    "matches" ~: testsMatches,
    "paths" ~: testsPaths,
    "eval" ~: testsEval,
    "evalWrap" ~: testsEvalWrap,
    "evalCtxt"~: testsEvalCtxt,
    "execEntity" ~: testsExecEntity,
    "exec" ~: testsExec
    ]

splitSlash = split '/'

testsPattern = test [
  splitSlash "" ~=? [],
    splitSlash "/" ~=? ["",""],
    splitSlash "/foo" ~=? ["", "foo"],
    splitSlash "foo/bar/plp/teorica/lambda" ~=? ["foo","bar","plp","teorica","lambda"],
    splitSlash "bar/foo//tp1/tests/" ~=? ["bar","foo","","tp1","tests",""],
    splitSlash "/bar/foo//tp1/tests" ~=? ["","bar","foo","","tp1","tests"],
    splitSlash "default/:index/user/?=new/create/valid?" ~=? ["default",":index","user","?=new","create","valid?"],
    pattern "" ~=? [],
    pattern "/" ~=? [],
    pattern "lit1/:cap1/:cap2/lit2/:cap3" ~=? [Literal "lit1", Capture "cap1", Capture "cap2", Literal "lit2", Capture "cap3"],
    pattern "/alu/:lu/aprobadas" ~=? [Literal "alu", Capture "lu", Literal "aprobadas"],
    pattern "/alu/:lu//aprobadas//" ~=? [Literal "alu", Capture "lu", Literal "aprobadas"],
    pattern "foo/:bar/:plp/:dc/user/lambda" ~=?  [Literal "foo", Capture "bar", Capture "plp", Capture "dc", Literal "user", Literal "lambda"],
    pattern "lit1" ~=? [Literal "lit1"],
    pattern ":cap2" ~=? [Capture "cap2"]
    ]

testsGet = test [
    get "nombre" [("nombre","plp"),("lu","007-1")] ~=? "plp",
    get "a" [("a","b"),("a","c")] ~=? "b"
    ]

testsMatches = test [
    Just (["tpf"],[("nombreMateria","plp")]) ~=? matches (splitSlash "materias/plp/tpf") (pattern "materias/:nombreMateria"),
    matches (splitSlash "materia/plp/alu/007-01") (pattern "materia/:nombre") ~=? Just (["alu","007-01"],[("nombre","plp")]),
    matches (splitSlash "user/pepe/profile/v1") (pattern "user/:name/profile/:api") ~=? Just ([],[("name","pepe"),("api","v1")]),
    matches [] [Literal "algo"] ~=? Nothing,
    matches (splitSlash "alu/007-01") (pattern "alumno/materia/:lu") ~=? Nothing
    ]

path0 = route "foo" 1
path1 = scope "foo" (route "bar" 2)
path2 = scope "foo" (route ":bar" 3)
path3 = scope "" $ scope "" $ many [ scope "" $ route "foo" 1]

testsEvalCtxt = test [
    Just (1, []) ~=? eval path0 "foo",
    Just (2, []) ~=? eval path1 "foo/bar",
    isNothing (eval path1 "foo/bla") ~? "",
    Just (3, [("bar", "bla")]) ~=? eval path2 "foo/bla",
    Just (1, []) ~=? eval path3 "foo",
    eval (route "foo/:lu" "ver foo de alumno") "foo/001" ~=? Just("ver foo de alumno", [("lu","001")]),
    eval (scope "materia/:nombre" (route "aprobada?" "la materia se aprobo?")) "materia/plp/aprobada?" ~=? Just("la materia se aprobo?", [("nombre","plp")]),
    eval (many [
            route "foo/:lu" "ver foo de alumno",
            scope "materia/:nombre" (route "aprobada?" "la materia se aprobo?"),
            scope "alumno/:lu" (many [route "registrar" "registrar alumno", route "editar" "editar alumno", route "eliminar" "eliminar alumno"])
            ]) "alumno/001/editar" ~=? Just("editar alumno",[("lu","001")]),
    eval (route "foo/:lu" "ver foo de alumno") "foo/alu/001" ~=? Nothing,
    eval (scope "materia/:nombre" (route "aprobada?" "la materia se aprobo?")) "materia/nombre/plp/aprobada?" ~=? Nothing,
    eval (many [
            route "foo/:lu" "ver foo de alumno",
            scope "materia/:nombre" (route "aprobada?" "la materia se aprobo?"),
            scope "alumno/:lu" (many [route "registrar" "registrar alumno", route "editar" "editar alumno", route "eliminar" "eliminar alumno"])
            ]) "foo/001/editar" ~=? Nothing
    ]

path4 = many [
  (route "" 1),
  (route "lo/rem" 2),
  (route "ipsum" 3),
  (scope "folder" (many [
    (route "lorem" 4),
    (route "ipsum" 5)
    ]))
  ]


testsEval = test [
        1 ~=? justEvalP4 "",
        4 ~=? justEvalP4 "folder/lorem"
    ]
    where justEvalP4 s = fst (fromJust (eval path4 s))

path410 = wrap (+10) path4

testsEvalWrap = test [
        14 ~=? justEvalP410 "folder/lorem",
        eval (wrap reverse (route "foo/:lu" "ver foo de alumno")) "foo/001" ~=? Just("onmula ed oof rev",[("lu","001")]),
        eval (wrap (\f -> length f) (scope "materia/:nombre" (route "aprobada?" "la materia se aprobo?"))) "materia/plp/aprobada?" ~=? Just(length "la materia se aprobo?",[("nombre","plp")]),
        eval (wrap reverse rutasFacultad) "ayuda" ~=?  Just("aduya rev",[]),
        eval (wrap reverse (route "foo/:lu" "ver foo de alumno")) "foo/lu/001" ~=? Nothing,
        eval (wrap (\f -> length f) (scope "materia/:nombre" (route "aprobada?" "la materia se aprobo?"))) "materia/plp/aprobada" ~=? Nothing,
        eval (wrap reverse rutasFacultad) "ayuda/inscribir" ~=?  Nothing
    ]
    where justEvalP410 s = fst (fromJust (eval path410 s))


-- ejempo donde el valor de cada ruta es una función que toma context como entrada.
-- para estos se puede usar en lugar además de eval, la función exec para devolver
-- la applicación de la función con sobre el contexto determinado por la ruta
rest entity = many [
  (route entity (const (entity++"#index"))),
  (scope (entity++"/:id") (many [
    (route "" (const (entity++"#show"))),
    (route "create" (\ctx -> entity ++ "#create of " ++ (get "id" ctx))),
    (route "update" (\ctx -> entity ++ "#update of " ++ (get "id" ctx))),
    (route "delete" (\ctx -> entity ++ "#delete of " ++ (get "id" ctx)))
    ]))
  ]

path5 = many [
  (route "" (const "root_url")),
  (rest "post"),
  (rest "category")
  ]

testsPaths = test [
    sort ["","post","post/:id","post/:id/create","post/:id/update","post/:id/delete","category","category/:id","category/:id/create","category/:id/update","category/:id/delete"] ~=? sort (paths path5),
    paths (route "hola" "ver hola") ~=? ["hola"],
    paths (route "" "ver home") ~=? [""],
    paths (scope "foo/:bar" (route "hola" "ver hola")) ~=? ["foo/:bar/hola"],
    paths (scope "materia" (many [route "plp" "ver PLP", route "tleng" "ver TLeng", route "bbdd" "ver BBDD"])) ~=? ["materia/plp","materia/tleng","materia/bbdd"],
    paths (many [route "index" "ver indice", scope "alumno" (route "materia/:nombre" "ver materia")]) ~=? ["index","alumno/materia/:nombre"],
    paths (many [route "index" "ver indice", scope "alumno" (many [route "foo" "ver foo", route ":lu/bar" "ver bar"])]) ~=? ["index","alumno/foo","alumno/:lu/bar"],
    paths rutasFacultad ~=? ["","ayuda","materia/:nombre/alu/:lu/inscribir","materia/:nombre/alu/:lu/aprobar","alu/:lu/aprobadas"]
    ]


testsExecEntity = test [
    Just "root_url" ~=? exec path5 "",
    Just "post#index" ~=? exec path5 "post",
    Just "post#show" ~=? exec path5 "post/35",
    Just "category#create of 7" ~=? exec path5 "category/7/create"
    ]
    
testsExec = test [
    exec (route "foo/bar/:lu" length) "foo/bar/001" ~=? Just (1),
    exec (scope "alu/:lu/materia/:nombre/:cuatri" (route "aprobada?" length) ) "alu/002/materia/plp/2c2015/aprobada?" ~=? Just(3),
    exec (many [route "index" (\ctx -> 0) , scope "alumno" (many [route "foo" (\ctx -> 1) , route ":lu/bar" (\ctx -> 2) ])]) "alumno/foo" ~=? Just(1),
    exec (route "foo/bar/:lu" length) "foo/bar/lu/001" ~=? Nothing,
    exec (scope "alu/:lu/materia/:nombre/:cuatri" (route "aprobada?" length) ) "alu/002/materia/plp/2c2015" ~=? Nothing,
    exec (many [route "index" (\ctx -> 0) , scope "alumno" (many [route "foo" (\ctx -> 1) , route ":lu/bar" (\ctx -> 2) ])]) "alumno/001/foo" ~=? Nothing    
    ]

main = do
    runTestTT allTests







