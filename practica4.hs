-- Pre Codigo --

data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"


--Definicion de los tipos para la practica
type Interpretacion = [(String, Bool)]
type Estado = (Interpretacion, [Clausula])



data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void

--Funcion para devolver el segundo elemento de una tupla
segundoElemento :: (a, b) -> b
segundoElemento (_, y) = y

-- Ejercicios --

-- 1. Conflicto. Determina si la claúsula vacía, representada como una lista vacia, forma parte del conjunto de cláusulas. Puesto que no existe modelo que satisfaga a la claúsula vacía, la busqueda del modelo falla. 

conflict :: Estado -> Bool
conflict (_, []) = False
conflict (_, (x:xs)) = if x == [] then True else conflict ([], xs)

-- 2. Exito. Determina si la búsqueda del modelo ha sido exitosa, esto sucede cuando el conjunto de cláusulas es vacío.

success :: Estado -> Bool
success (_, clausulas) = if clausulas == [] then True else False

-- 3. Clausula unitaria. Si "l" es una literal que pertenece a una claúsula unitaria, entonces basta con agregar "l" al modelo y seguir con la búsqueda...
-- Aún no está al 100 !
unit :: Estado -> Estado
unit (modelo, []) = (modelo, [])
unit (modelo, (x:xs)) = if length x == 1 
                        then case head x of
                            Var p -> ((p, True) : modelo, xs)
                            Not (Var p) -> ((p, False) : modelo, xs)
                            _     -> (modelo, xs)
                        else (modelo, xs)

                        
-- 4. Eliminación. Si "l" es una literal que pertenece al modelo M y se tiene la clausula "l v C" entonces, dado que "l" es verdadera, "l v C" también lo es, por lo que se elimina la claúsula "l v C" del conjunto de claúsulas. 

elim :: Estado -> Estado

-- 5. Reducción. Si "l" es una literal que pertenece al modelo M y se tiene qe la claúsula (l^C) es falsa, por lo que solo es de interés saber si "C" es satisfacible.

red :: Estado -> Estado

-- 6. Separación. Dada una literal "l" se procede a buscar que "M", "l" sea modelo de "F", o que "M", "l^C" lo sea. 

sep :: Literal -> Estado -> (Estado, Estado)
sep (Var p) (i, xs) = ((i ++ [(p, True)], xs), (i ++ [(p, False)], xs))
sep (Not (Var p)) (i, xs) = ((i ++ [(p, True)], xs), (i ++ [(p, False)], xs))


-- 7. Implementación de algoritmo DPLL P2. Al ser DPLL un algoritmo no determinista, debemos definir una heurística para decidir sobre qué literal se aplicará la regla de separación. 

heuristicsLiteral :: [Clausula] -> Literal


--Funcion para construir el arbol dpll desde un estado dado
--Deben estar ya implementadas las funciones unit,red,sep,elim y heuristicsLiteral
construirArbolDPLL :: Estado -> ArbolDPLL
construirArbolDPLL estado 
    | conflict estado = Node estado Void
    | success estado = Node estado Void
    | (segundoElemento propuesto) /= (segundoElemento estado) = Node estado (construirArbolDPLL propuesto)
    | otherwise = Branch estado (construirArbolDPLL izq) (construirArbolDPLL der)
    where
        propuesto = red (elim (unit (estado))) --Tratamos de aplicar unit, elim y/o red. Si hay cambios en las clausulas, se aplico alguna de las reglas.
        literal = heuristicsLiteral (segundoElemento estado) --Sacamos una literal
        (izq,der) = sep literal estado --Aplicamos separacion

--Funcion para explorar el arbol dpll construido con un estado dado
explorarArbolDPLL :: ArbolDPLL -> Estado -> Estado
explorarArbolDPLL Void estado = estado
explorarArbolDPLL (Node estado t) _ = explorarArbolDPLL t estado
explorarArbolDPLL (Branch estado t1 t2) _ = if conflict x --Si hay conflicto con el lado izquierdo, devolvemos el resultado del lado derecho
                                        then explorarArbolDPLL t2 estado
                                        else x
                                    where 
                                        x = explorarArbolDPLL t1 estado



-- 8. Usando los ejercicios previamente, la funcion principal seria...

dpll :: [Clausula] -> Interpretacion
dpll clausulas = if conflict ultimoEstado
                    then []
                    else primerElemento ultimoEstado
                where
                    inicial = ([], clausulas)
                    arbol = construirArbolDPLL inicial
                    ultimoEstado = explorarArbolDPLL arbol inicial
