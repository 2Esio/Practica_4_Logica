-- Pre Codigo --

type Interpretacion = [(String, Bool)]
type Estado = (Interpretacion, [Clausula])


-- Ejercicios --

-- 1. Conflicto. Determina si la claúsula vacía, representada como una lista vacia, forma parte del conjunto de cláusulas. Puesto que no existe modelo que satisfaga a la claúsula vacía, la busqueda del modelo falla. 

conflict :: Estado -> Bool

-- 2. Exito. Determina si la búsqueda del modelo ha sido exitosa, esto sucede cuando el conjunto de cláusulas es vacío.

success :: Estado -> Bool

-- 3. Clausula unitaria. Si "l" es una literal que pertenece a una claúsula unitaria, entonces basta con agregar "l" al modelo y seguir con la búsqueda...

unit :: Estado -> Estado

-- 4. Eliminación. Si "l" es una literal que pertenece al modelo M y se tiene la clausula "l v C" entonces, dado que "l" es verdadera, "l v C" también lo es, por lo que se elimina la claúsula "l v C" del conjunto de claúsulas. 

elim :: Estado -> Estado

-- 5. Reducción. Si "l" es una literal que pertenece al modelo M y se tiene qe la claúsula (l^C) es falsa, por lo que solo es de interés saber si "C" es satisfacible.

red :: Estado -> Estado

-- 6. Separación. Dada una literal "l" se procede a buscar que "M", "l" sea modelo de "F", o que "M", "l^C" lo sea. 

sep :: Literal -> Estado -> (Estado, Estado)

-- 7. Implementación de algoritmo DPLL P2. Al ser DPLL un algoritmo no determinista, debemos definir una heurística para decidir sobre qué literal se aplicará la regla de separación. 

heuristicsLiteral :: [Clausula] -> Literal

-- 8. Usando los ejercicios previamente, la funcion principal seria...

dpll :: [Clausula] -> Interpretacion
