module LPO where

import Data.Char
import List

type Nombre = String

data Termino = Var Nombre | Func Nombre [Termino]

data Formula = Pred Nombre [Termino] | No Formula | Y Formula Formula | O Formula Formula | Imp Formula Formula | A Nombre Formula | E Nombre Formula

esLiteral :: Formula -> Bool
esLiteral (Pred _ _) = True
esLiteral (No (Pred _ _)) = True
esLiteral _ = False

{- ejemplo de fold en listas
sumatoria :: Num a => [a] -> a
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

sumatoria = foldr (+) 0
-}

foldTermino ::
           (Nombre -> b)           -- funcion para Var Nombre
        -> (Nombre -> [b] -> b)    -- funcion para Func Nombre [Termino]
        -> Termino
        -> b
foldTermino fVar fFunc (Var nombre) = fVar nombre
foldTermino fVar fFunc (Func nombre terminos) = fFunc nombre (map (foldTermino fVar fFunc) terminos)

{-test-}
concatTermino :: Termino -> Nombre
concatTermino = foldTermino id (\nombre resultados -> nombre ++ "(" ++ (concat resultados) ++ ")")

var = Var "x"
term1 = Func "f" [var]
term2 = Func "g" [term1, var]
term3 = Func "j" []
term4 = Func "g" [term2, term3]


{-
concatTermino term3 == "g(f(x)x)"
-}

--data Formula = Pred Nombre [Termino] | No Formula | Y Formula Formula | O Formula Formula | Imp Formula Formula | A Nombre Formula | E Nombre Formula

--Esquema de recursión estructural para fórmulas.
foldFormula ::
		(Nombre -> [Termino] -> b)	-- funcion para Pred
	 -> (b -> b)					-- funcion para No
	 -> (b -> b -> b)				-- funcion para Y
	 -> (b -> b -> b)				-- funcion para O
	 -> (b -> b -> b)				-- funcion para Imp
	 -> (Nombre -> b -> b)			-- funcion para A
	 -> (Nombre -> b -> b)			-- funcion para E
	 -> Formula
	 -> b
	 
foldFormula fPred fNo fY fO fImp fA fE (Pred nombre terminos) = fPred nombre terminos
foldFormula fPred fNo fY fO fImp fA fE (No resultado) = fNo (foldFormula fPred fNo fY fO fImp fA fE resultado)
foldFormula fPred fNo fY fO fImp fA fE (Y resultado1 resultado2) = fY (fold resultado1) (fold resultado2)
																where fold = foldFormula fPred fNo fY fO fImp fA fE
foldFormula fPred fNo fY fO fImp fA fE (O resultado1 resultado2) = fO (fold resultado1) (fold resultado2)
																where fold = foldFormula fPred fNo fY fO fImp fA fE
foldFormula fPred fNo fY fO fImp fA fE (Imp resultado1 resultado2) = fImp (fold resultado1) (fold resultado2)
																where fold = foldFormula fPred fNo fY fO fImp fA fE
foldFormula fPred fNo fY fO fImp fA fE (A nombre resultado) = fA nombre (fold resultado)
																where fold = foldFormula fPred fNo fY fO fImp fA fE
foldFormula fPred fNo fY fO fImp fA fE (E nombre resultado) = fE nombre (fold resultado)
																where fold = foldFormula fPred fNo fY fO fImp fA fE
{- test
concatFormula :: Formula -> Nombre
concatFormula = foldFormula
					(\nombre terminos -> concatTermino (Func nombre terminos))
					(\resultado -> "No " ++ resultado)
					(\resultado1 resultado2 -> "(" ++ resultado1 ++ " Y " ++ resultado2 ++ ")")
					(\resultado1 resultado2 -> "(" ++ resultado1 ++ " O " ++ resultado2 ++ ")")
					(\resultado1 resultado2 -> "(" ++ resultado1 ++ " => " ++ resultado2 ++ ")")
					(\nombre resultado -> "((A" ++ nombre ++ ")" ++ resultado ++ ")")
					(\nombre resultado -> "((E" ++ nombre ++ ")" ++ resultado ++ ")")
-}					
form1 = Pred "p" [term2]
form2 = No form1
form3 = Y form1 form2

-- concatFormula form3 == "(p(g(f(x)x)) Y No p(g(f(x)x)))"

--Esquema de recursión primitiva para fórmulas.

recFormula ::
		(Nombre -> [Termino] -> b)				-- funcion para Pred
     -> (Formula -> b -> b)						-- funcion para No
     -> (Formula -> Formula -> b -> b -> b)		-- funcion para Y
     -> (Formula -> Formula -> b -> b -> b)		-- funcion para O
     -> (Formula -> Formula -> b -> b -> b) 	-- funcion para Imp
     -> (Formula -> Nombre -> b -> b)			-- funcion para A
     -> (Formula -> Nombre -> b -> b)			-- funcion para E
     -> Formula
     -> b
     
recFormula fPred fNo fY fO fImp fA fE (Pred nombre terminos) = fPred nombre terminos
recFormula fPred fNo fY fO fImp fA fE (No formula) = fNo formula (recFormula fPred fNo fY fO fImp fA fE formula)
recFormula fPred fNo fY fO fImp fA fE (Y formula1 formula2) = fY formula1 formula2 (rec formula1) (rec formula2)
																where rec = recFormula fPred fNo fY fO fImp fA fE
recFormula fPred fNo fY fO fImp fA fE (O formula1 formula2) = fO formula1 formula2 (rec formula1) (rec formula2)
																where rec = recFormula fPred fNo fY fO fImp fA fE
recFormula fPred fNo fY fO fImp fA fE (Imp formula1 formula2) = fImp formula1 formula2 (rec formula1) (rec formula2)
																where rec = recFormula fPred fNo fY fO fImp fA fE
recFormula fPred fNo fY fO fImp fA fE (A nombre formula) = fA formula nombre (rec formula)
																where rec = recFormula fPred fNo fY fO fImp fA fE
recFormula fPred fNo fY fO fImp fA fE (E nombre formula) = fE formula nombre (rec formula)
																where rec = recFormula fPred fNo fY fO fImp fA fE
{-
concatFormulaRec :: Formula -> Nombre
concatFormulaRec = recFormula
					(\nombre terminos -> concatTermino (Func nombre terminos))
					(\formula resultado -> "No " ++ resultado)
					(\formula1 formula2 resultado1 resultado2 -> "(" ++ resultado1 ++ " Y " ++ resultado2 ++ ")")
					(\formula1 formula2 resultado1 resultado2 -> "(" ++ resultado1 ++ " O " ++ resultado2 ++ ")")
					(\formula1 formula2 resultado1 resultado2 -> "(" ++ resultado1 ++ " => " ++ resultado2 ++ ")")
					(\formula nombre resultado -> "((A" ++ nombre ++ ")" ++ resultado ++ ")")
					(\formula nombre resultado -> "((E" ++ nombre ++ ")" ++ resultado ++ ")")
-}
				
-- EJERCICIO 5
instance Show Termino where
  show = foldTermino (map toUpper) (\nombre resultados -> parentizar nombre resultados)
    
join::[a]->[[a]]->[a]
join separador = foldr (\x res->if null res then x else x++separador++res) []

{- Toma un nombre de función y una lista de argumentos ya convertidos en String, y termina de armar la representación visual. -}
parentizar :: Nombre -> [String] -> String
parentizar s res = if null res then s else s++"("++(join "," res)++")"

instance Show Formula where
-- Operadores lógicos: "¬","∧","∨","⊃","∀","∃"
    show = recFormula
				(\nombre terminos -> parentizar ((map toUpper) nombre)  (map show terminos))
				(\formula resultado -> "¬" ++ (if (not (esLiteral formula)) then "(" ++ resultado ++ ")" else resultado))
				(\formula1 formula2 resultado1 resultado2 -> "(" ++ resultado1 ++ "∧" ++ resultado2 ++ ")")
				(\formula1 formula2 resultado1 resultado2 -> "(" ++ resultado1 ++ "∨" ++ resultado2 ++ ")")
				(\formula1 formula2 resultado1 resultado2 -> "(" ++ resultado1 ++ "⊃" ++ resultado2 ++ ")")
				(\formula nombre resultado -> "∀" ++ nombre ++ "." ++ resultado)
				(\formula nombre resultado -> "∃" ++ nombre ++ "." ++ resultado)
--Ejemplo: A "x" (Imp (Pred "p" [Var "x"]) (Pred "p" [Var "x"])) se ve como ∀X.(P(X)⊃P(X))
--CONSULTAR

--eliminarImplicaciones :: Dar tipo e implementar.
eliminarImplicaciones::Formula->Formula
eliminarImplicaciones = foldFormula 
	(\nombre terminos -> (Pred nombre terminos)) 
	(\res -> No res) 
	(\res1 res2 -> Y res1 res2)
	(\res1 res2 -> O res1 res2)
	(\res1 res2 -> O (No res1) res2)
	(\nombre res -> A nombre res)
	(\nombre res -> E nombre res)

--eliminarImplicaciones (Imp form1 form1) == No P(g(f(X),X)) O P(g(f(X),X))
--eliminarImplicaciones (Imp form1 form2) == No P(g(f(X),X)) O No P(g(f(X),X))

aFNN :: Formula -> Formula
aFNN = foldFormula
            (\nombre terminos -> Pred nombre terminos) -- Pred
            (\resultado -> if esLiteral (No resultado) then (No resultado) else (aFNNauxNo resultado)) -- No
            (\resultado1 resultado2 -> Y resultado1 resultado2)   -- Y
            (\resultado1 resultado2 -> O resultado1 resultado2)   -- O
            (\resultado1 resultado2 -> aFNN (eliminarImplicaciones (Imp resultado1 resultado2)))   -- Imp
            (\nombre resultado -> A nombre resultado)                                              -- A
            (\nombre resultado -> E nombre resultado)                                              -- E
            
aFNNauxNo :: Formula -> Formula
aFNNauxNo (Pred nombre terminos) = Pred nombre terminos
aFNNauxNo (No formula) = formula
aFNNauxNo (Y formula1 formula2) = aFNN (O (No formula1) (No formula2))
aFNNauxNo (O formula1 formula2) = aFNN (Y (No formula1) (No formula2))
aFNNauxNo (Imp formula1 formula2) = aFNN (No (aFNN (Imp formula1 formula2)))
aFNNauxNo (A nombre formula) = aFNN (E nombre (No formula))
aFNNauxNo (E nombre formula) = aFNN (A nombre (No formula))

                
{-
-- ¬E(x)P(x) -> A(x)¬P(x)                
aFNN (No(E "x" (Pred "P" [Var "x"]))) == ∀x.¬P(X)

-- ¬(P(x) v Q(x)) -> (¬P(x) ^ ¬Q(x))
aFNN (No(O (Pred "P" [Var "x"]) (Pred "Q" [Var "x"]))) == (¬P(X)∧¬Q(X))

¬¬P (X) -> P (X)
aFNN (No(No(Pred "P" [Var "x"]))) == P(X)

¬(Q(X, Y) ∧ R(Z)) -> ¬Q(X, Y) ∨ ¬R(Z)
aFNN (No(Y(Pred "Q" [Var "x", Var "y"]) (Pred "R" [Var "Z"]))) == (¬Q(X,Y)∨¬R(Z))

∃Y.(¬∃X.(P (X) ⊃ Q(X, Y ))) -> ∃Y.(∀X.(P (X) ∧ ¬Q(X, Y )))
aFNN (E "Y" (No(E "X" (Imp (Pred "P" [Var "x"]) (Pred "Q" [Var "x", Var "y"]))))) == ∃Y.∀X.(P(X)∧¬Q(X,Y))

∀X.(∃Y (P (X) ⊃ Q(X, Y ))) -> ∀X.(∃Y.(¬P (X) ∨ Q(X, Y )))
-}


--fv:: Dar tipo e implementar.

--Interpretación en un dominio a. Una función para términos y otra para predicados.
--Basta con que las funciones estén bien definidas para su dominio esperado.
data Interpretacion a = I {fTerm :: (Nombre->[a]->a), fPred :: (Nombre->[a]->Bool)}

--Ejemplo para pruebas:
ejemploNat::Interpretacion Int
ejemploNat = I fTerminos fPredicados where
  fTerminos nombreF | nombreF == "0" = const 0
            | nombreF == "suc" = \xs -> head xs + 1
            | nombreF == "suma" = sum
  fPredicados nombreP | nombreP == "esCero" = \xs -> head xs == 0
              | nombreP == "esPar" = \xs -> mod (head xs) 2 == 0
              | nombreP == "mayor" = \xs -> (head xs) > (head (tail xs))
              | nombreP == "menor" = \xs -> (head xs) < (head (tail xs))

--Proyectores (ya están predefinidos).
{-
fTerm :: Interpretacion a -> (Nombre->[a]->a)
fTerm (I fT _) = fT

fPred :: Interpretacion a -> (Nombre->[a]->Bool)
fPred (I _ fP) = fP
-}

type Asignacion a = Nombre -> a

--Ejemplo para pruebas:
asignacion1::Asignacion Int
asignacion1 "X" = 0
asignacion1 "Y" = 1
asignacion1 "Z" = 2

evaluar::Asignacion a->(Nombre->[a]->a)->Termino->a
evaluar = error "Falta implementar."

--Ejemplo: evaluar asignacion1 (fTerm ejemploNat) $ Func "suma" [Func "suc" [Var "X"], Var "Y"]

--actualizarAsignacion :: Implementar y dar el tipo.

--Se usa una asignación de valores a las variables libres. Pueden usar recursión explícita, pero aclaren por qué no encaja bien en fold ni rec.
--Se puede hacer con fold cambiando el orden de los parámetros (flip), pero no es natural/sencillo. ¿por qué?
vale::Eq a =>Interpretacion a -> [a] -> Asignacion a -> Formula -> Bool
vale = error "Falta implementar."

-- Ejemplos (agreguen alguno con otra interpretación).

-- vale ejemploNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "Y", Var "X"])
-- True

-- vale ejemploNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "X",Func "suc" [Var "X"]])
-- False

--vale ejemploNat [0,1] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"]))
--True

--vale ejemploNat [0] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"]))
--False
