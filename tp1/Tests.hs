import LPO
import HUnit

{-
	Para correr los tests deben cargar en hugs el módulo Tests
	y evaluar la expresión "main".

	Se incluye también la definición de (~~?) y (~~), que pueden usar
	para comparar listas sin importar el orden de sus elementos.
-}

main = runTestTT allTests

(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)

(~~) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Bool
expected ~~ actual = (sort expected) == (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)


{-
	Definición de los casos de test.
	
	No olviden agregar sus tests al listado allTests para que se ejecuten.
-}
 
allTests = test [ 
	"join" ~: testsJoin,
	"parentizar" ~: testsParentizar,
    "literal" ~: testsLiteral,
    "foldTermino" ~: testsFoldTermino,
    "foldFormula" ~: testsFoldFormula,
    "showTermino" ~: testsShowTermino,
    "showFormula" ~: testsShowFormula,
    "eliminarImplicaciones" ~: testsEliminarImplicaciones,
    "show aFNN" ~: testsAFNN,
    "fv" ~: testsFV,
    "evaluar" ~: testsEvaluar,
    "actualizarAsignacion" ~: testsActualizarAsignacion,
    "vale" ~: testsVale
	]

testsJoin = test [
	join "," [] ~=? "",
	join "," ["x"] ~=? "x",
	join "," ["x", "y"] ~=? "x,y"
	]

testsParentizar = test [
	parentizar "f" [] ~=? "f",
	parentizar "f" ["x"] ~=? "f(x)",
	parentizar "f" ["x", "y"] ~=? "f(x,y)"
	]

testsLiteral = test [
    esLiteral p1 ~=? True,
    esLiteral (No p1) ~=? True,
    esLiteral (Y p1 p2) ~=? False
    ]

{- auxiliar para test de foldTermino -}
mostrarTermino :: Termino -> Nombre
mostrarTermino = foldTermino id (\nombre resultados -> nombre ++ "(" ++ (concat resultados) ++ ")")
    
testsFoldTermino = test [
    mostrarTermino t1 ~=? "f(x)",
    mostrarTermino t2 ~=? "g(f(x)x)"
    ]
    
{- auxiliar para test de foldFormula -}
mostrarFormula :: Formula -> Nombre
mostrarFormula = foldFormula
					(\nombre terminos -> mostrarTermino (Func nombre terminos))
					(\resultado -> "No " ++ resultado)
					(\resultado1 resultado2 -> "(" ++ resultado1 ++ " Y " ++ resultado2 ++ ")")
					(\resultado1 resultado2 -> "(" ++ resultado1 ++ " O " ++ resultado2 ++ ")")
					(\resultado1 resultado2 -> "(" ++ resultado1 ++ " => " ++ resultado2 ++ ")")
					(\nombre resultado -> "((A" ++ nombre ++ ")" ++ resultado ++ ")")
					(\nombre resultado -> "((E" ++ nombre ++ ")" ++ resultado ++ ")")

testsFoldFormula = test [
    mostrarFormula f1 ~=? "p(g(f(x)x))",
    mostrarFormula f3 ~=? "(p(g(f(x)x)) Y No p(g(f(x)x)))"
    ]

testsShowTermino = test [
    show t1 ~=? "f(X)",
    show t2 ~=? "g(f(X),X)",
    show (Func "f" [Var "x", Var "y", Func "c" []]) ~=? "f(X,Y,c)"
    ]

testsShowFormula = test [
    show f1 ~=? "P(g(f(X),X))",
    show f2 ~=? "¬P(g(f(X),X))",
    show (Y f1 f2) ~=? "(P(g(f(X),X))∧¬P(g(f(X),X)))",
    show (A "x" (Imp (Pred "p" [Var "x"])(Pred "p" [Var "x"]))) ~=? "∀X.(P(X)⊃P(X))"
    ]

testsEliminarImplicaciones = test [
    show (eliminarImplicaciones (Imp f1 f1)) ~=? "(¬P(g(f(X),X))∨P(g(f(X),X)))",
    show (eliminarImplicaciones (Imp f1 f2)) ~=? "(¬P(g(f(X),X))∨¬P(g(f(X),X)))"
    ]
        
testsAFNN = test [
    show (aFNN (No(E "x" (Pred "P" [Var "x"])))) ~=? "∀X.¬P(X)",
    show (aFNN (No(O (Pred "P" [Var "x"]) (Pred "Q" [Var "x"])))) ~=? "(¬P(X)∧¬Q(X))",
    show (aFNN (No(No(Pred "P" [Var "x"])))) ~=? "P(X)",
    show (aFNN (No(Y(Pred "Q" [Var "x", Var "y"]) (Pred "R" [Var "Z"])))) ~=? "(¬Q(X,Y)∨¬R(Z))",
    show (aFNN (E "Y" (No(E "X" (Imp (Pred "P" [Var "x"]) (Pred "Q" [Var "x", Var "y"])))))) ~=? "∃Y.∀X.(P(X)∧¬Q(X,Y))"
    ]
    
testsFV = test [
    fv (A "y" (Imp (Pred "p" [Var "x", Var "y"]) (Pred "p" [Var "x"]))) ~~? ["x"],
    fv (E "x" (Imp (Pred "p" [Var "x", Var "y"]) (Pred "p" [Var "x"]))) ~~? ["y"],
    fv (E "x" (Pred "p" [Var "x"])) ~~? []
    ]

testsEvaluar = test [
    (evaluar asignacion1 (fTerm ejemploNat) $ Func "suma" [Func "suc" [Var "X"], Var "Y"]) ~=? 2,
    (evaluar asignacion1 (fTerm ejemploNat) $ Func "suma" [Func "suc" [Var "X"], Var "X"]) ~=? 1,
    (evaluar asignacion1 (fTerm ejemploNat) $ Func "suc" [Func "suc" [Var "X"]]) ~=? 2
    ]
    
testsActualizarAsignacion = test [
    ((actualizarAsignacion "Z" 4 asignacion1) "Z") ~=? 4,
    ((actualizarAsignacion "Z" 4 asignacion1) "X") ~=? 0
    ]
    
testsVale = test [
    (vale ejemploNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "Y", Var "X"])) ~=? True, 
    (vale ejemploNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "X", Func "suc" [Var "X"]])) ~=? False,
    (vale ejemploNat [0,1] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"]))) ~=? True,
    (vale ejemploNat [0] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"]))) ~=? False,
    -- interpretacion propia
    (vale ejemploListas [[], ["a"]] (\v -> ["b"]) (E "Y" (Pred "esMasCorta" [Var "Y", Var "X"]))) ~=? True,
    (vale ejemploListas [[], ["a"]] (\v -> ["b"]) (E "Y" (Pred "esMasLarga" [Var "Y", Var "X"]))) ~=? False,
    (vale ejemploListas [[1], [2]] (\v -> [2]) (A "Y" (Pred "esMasLarga" [Var "Y", Func "vacia" []]))) ~=? True,
    (vale ejemploListas [] (\v -> if v == "X" then [1] else [2]) (Pred "esMasLarga" [Func "concatenar" [Var "X", Var "X"], Var "X"])) ~=? True
    ]

{- valores -} 
var = Var "x"

p1 = Pred "p" [var]
p2 = Pred "q" [var]
p3 = Pred "r" [var]

t1 = Func "f" [var]
t2 = Func "g" [t1, var]
t3 = Func "j" []
t4 = Func "g" [t2, t3]

f1 = Pred "p" [t2]
f2 = No f1
f3 = Y f1 f2

asignacion1::Asignacion Int
asignacion1 "X" = 0
asignacion1 "Y" = 1
asignacion1 "Z" = 2

ejemploNat::Interpretacion Int
ejemploNat = I fTerminos fPredicados where
  fTerminos nombreF | nombreF == "0" = const 0
            | nombreF == "suc" = \xs -> head xs + 1
            | nombreF == "suma" = sum
  fPredicados nombreP | nombreP == "esCero" = \xs -> head xs == 0
              | nombreP == "esPar" = \xs -> mod (head xs) 2 == 0
              | nombreP == "mayor" = \xs -> (head xs) > (head (tail xs))
              | nombreP == "menor" = \xs -> (head xs) < (head (tail xs))
              
ejemploListas::Interpretacion [a]
ejemploListas = I fTerminos fPredicados where
  fTerminos nombreF | nombreF == "vacia" = const []
                    | nombreF == "cola" = \xss -> tail (head xss)
                    | nombreF == "concatenar" = \xss -> concat xss
  fPredicados nombreP | nombreP == "estaVacia" = \xss -> null (head xss)
                      | nombreP == "esMasLarga" = \xss -> (length (head xss)) > (length (head (tail xss)))
                      | nombreP == "esMasCorta" = \xss -> (length (head xss)) < (length (head (tail xss)))
