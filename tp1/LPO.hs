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

foldTermino ::
           (Nombre -> b)           -- funcion para Var Nombre
        -> (Nombre -> [b] -> b)    -- funcion para Func Nombre [Termino]
        -> Termino
        -> b
foldTermino fVar fFunc (Var nombre) = fVar nombre
foldTermino fVar fFunc (Func nombre terminos) = fFunc nombre (map (foldTermino fVar fFunc) terminos)

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
				(\formula nombre resultado -> "∀" ++ (map toUpper) nombre ++ "." ++ resultado)
				(\formula nombre resultado -> "∃" ++ (map toUpper) nombre ++ "." ++ resultado)
                
                
eliminarImplicaciones::Formula->Formula
eliminarImplicaciones = foldFormula 
	(\nombre terminos -> (Pred nombre terminos)) 
	(\res -> No res) 
	(\res1 res2 -> Y res1 res2)
	(\res1 res2 -> O res1 res2)
	(\res1 res2 -> O (No res1) res2)
	(\nombre res -> A nombre res)
	(\nombre res -> E nombre res)

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

fv::Formula->[Nombre]
fv = foldFormula 
        (\nombre terminos -> nub (concat (map listarVariables terminos)))
        (\res -> res) 
        (\res1 res2 -> nub (res1 ++ res2))
        (\res1 res2 -> nub (res1 ++ res2))
        (\res1 res2 -> nub (res1 ++ res2))
        (\nombre res -> filter (\e -> e /= nombre) res)
        (\nombre res -> filter (\e -> e /= nombre) res)

listarVariables::Termino->[Nombre]
listarVariables = foldTermino (\nombre -> [nombre]) (\nombre resultados -> concat resultados)

--Interpretación en un dominio a. Una función para términos y otra para predicados.
--Basta con que las funciones estén bien definidas para su dominio esperado.
data Interpretacion a = I {fTerm :: (Nombre->[a]->a), fPred :: (Nombre->[a]->Bool)}

--Proyectores (ya están predefinidos).
{-
fTerm :: Interpretacion a -> (Nombre->[a]->a)
fTerm (I fT _) = fT

fPred :: Interpretacion a -> (Nombre->[a]->Bool)
fPred (I _ fP) = fP
-}

type Asignacion a = Nombre -> a

evaluar::Asignacion a->(Nombre->[a]->a)->Termino->a
evaluar asignacion funciones = foldTermino
                                    (\nombre -> asignacion nombre)                    -- Var
                                    (\nombre elementos -> funciones nombre elementos) -- Func

actualizarAsignacion::Nombre -> a -> Asignacion a -> Asignacion a
actualizarAsignacion nombreNuevo valor asignacion = (\nombre -> if (nombre == nombreNuevo) then valor else asignacion nombre)

--Se usa una asignación de valores a las variables libres. Pueden usar recursión explícita, pero aclaren por qué no encaja bien en fold ni rec.
--Se puede hacer con fold cambiando el orden de los parámetros (flip), pero no es natural/sencillo. ¿por qué?

{-vale::Eq a =>Interpretacion a -> [a] -> Asignacion a -> Formula -> Bool
vale interpretacion dominio asignacion = foldFormula
                                                (\nombre terminos -> (fPredicados intepretacion nombre) (map (evaluar asignacion (fTerminos interpretacion)) terminos))
                                                not
                                                (\bool1 bool2 -> and [bool1, bool2])
                                                (\bool1 bool2 -> or [bool1, bool2])
                                                (\bool1 bool2 -> if (not bool1) then True else bool2
                                                ...
                                                ...
    Problema: tanto fold como rec asumen que pueden calcular los valores de las subfórmulas antes que el valor de las fórmulas que la incluyen.
    Ahora bien, si una fórmula empieza con un ∀ o con un ∃, sus subfórmulas pueden tener variables libres, y en este caso no puede calcularse
    su valor de verdad por sí solas (sino que debe necesariamente evaluarse el cuantificador primero). Es por este motivo que resolver "vale"
    con rec o fold es difícil, pues la función asociada al constructor "Pred" debería poder manejar el caso en que haya variables libres.
    Una forma podría ser que devuelva un "tercer valor de verdad" que indique que el valor es indefinido hasta que no se evalúe algún cuantificador.
-}

vale::Eq a =>Interpretacion a -> [a] -> Asignacion a -> Formula -> Bool
vale interpretacion dominio asignacion (Pred nombre terminos) = (fPred interpretacion nombre) (map (evaluar asignacion (fTerm interpretacion)) terminos)
vale interpretacion dominio asignacion (No formula) = not (vale interpretacion dominio asignacion formula)
vale interpretacion dominio asignacion (Y formula1 formula2) = and [aplicar formula1, aplicar formula2]
                                                                    where aplicar = vale interpretacion dominio asignacion
vale interpretacion dominio asignacion (O formula1 formula2) = or [aplicar formula1, aplicar formula2]
                                                                    where aplicar = vale interpretacion dominio asignacion
vale interpretacion dominio asignacion (A nombre formula) = and [vale interpretacion dominio asignacion_nueva formula | asignacion_nueva <- asignaciones]
                                                                    where asignaciones = [actualizarAsignacion nombre valor asignacion | valor <- dominio]
vale interpretacion dominio asignacion (E nombre formula) = or [vale interpretacion dominio asignacion_nueva formula | asignacion_nueva <- asignaciones]
                                                                    where asignaciones = [actualizarAsignacion nombre valor asignacion | valor <- dominio]

