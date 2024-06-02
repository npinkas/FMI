module Library where
import PdePreludat

--Parte 1

type Dinero = Number
type Poblacion = Number
type RecursoNatural = String

data Pais = UnPais {
    ingresoPerCapita :: Dinero,
    poblacionActivaSectorPublico :: Poblacion,
    poblacionActivaSectorPrivado :: Poblacion,
    recursosNaturales :: [RecursoNatural],
    deudaMillones :: Dinero
} deriving (Show, Eq)

namibia :: Pais
namibia = UnPais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50


--Parte 2
type Estrategia = Pais -> Pais
type Receta = [Estrategia]

--Estrategia 1

prestarNMillonesAlPais :: Number -> Estrategia 
prestarNMillonesAlPais nMillones = modificarDeuda (aumentarPorInteres nMillones 150) 

modificarDeuda :: Dinero -> Pais -> Pais
modificarDeuda x pais = pais {deudaMillones = deudaMillones pais + x}

aumentarPorInteres :: Number -> Number -> Number
aumentarPorInteres valor porcentaje= valor * (porcentaje/100)

-- Estrategia 2

reducirPuestosSectorPublico :: Number -> Estrategia 
reducirPuestosSectorPublico cantPuestos pais
    |poblacionActivaSectorPublico pais > 100 = modificarSectorPublico 80 cantPuestos pais
    |otherwise = modificarSectorPublico 85 cantPuestos pais

modificarSectorPublico :: Number -> Number -> Estrategia 
modificarSectorPublico ingreso cantPuestos = modificarIngresoPerCapita ingreso . modificarPuestosSectorPublico (negate cantPuestos)

modificarIngresoPerCapita :: Number -> Pais -> Pais
modificarIngresoPerCapita ingreso pais = pais {ingresoPerCapita = ingresoPerCapita pais + ingreso}

modificarPuestosSectorPublico :: Number -> Pais -> Pais
modificarPuestosSectorPublico cantPuestos pais = pais {poblacionActivaSectorPublico = poblacionActivaSectorPublico pais + cantPuestos}

--Estrategia 3

darExplotacionRecursoNatural ::RecursoNatural ->  Estrategia
darExplotacionRecursoNatural recursoExplotado =  perderRecursoNatural recursoExplotado . modificarDeuda (-2)

perderRecursoNatural :: RecursoNatural -> Pais -> Pais
perderRecursoNatural recursoNatural pais = pais {recursosNaturales = encontrarRecursoNatural recursoNatural pais}

encontrarRecursoNatural :: RecursoNatural -> Pais -> [RecursoNatural]
encontrarRecursoNatural recursoExplotado pais = filter (/= recursoExplotado) (recursosNaturales pais) 

--Estrategia 4

blindaje :: Estrategia
blindaje pais = (modificarPuestosSectorPublico (-500) . modificarDeuda (calcularPBI pais/2)) pais

calcularPBI :: Pais -> Number
calcularPBI pais = sumaPoblacionActiva pais * ingresoPerCapita pais

sumaPoblacionActiva :: Pais -> Poblacion
sumaPoblacionActiva pais = poblacionActivaSectorPublico pais + poblacionActivaSectorPrivado pais  

--Parte 3

receta1 :: Receta
receta1 = [darExplotacionRecursoNatural "Mineria", prestarNMillonesAlPais 200]

aplicarEstrategia :: Pais -> Estrategia -> Pais
aplicarEstrategia pais estrategia = estrategia pais

aplicarReceta :: Pais -> Receta -> Pais
aplicarReceta pais receta = foldl aplicarEstrategia pais receta

{-
Es posible lograr el efecto colateral a pesar de que Haskell carezca de efecto gracias a la composicion y aplicacion parcial de las funciones auxiliares.

-}

--Parte 4

puedenZafar :: [Pais] -> [Pais]
puedenZafar paises = filter (elem "Petroleo" . recursosNaturales) paises


totalDeuda :: [Pais] -> Number
totalDeuda = sum . map deudaMillones 

{-
Funciones de Orden superior aparecen en ambos casos, por ejemplo, podemos mencionar el uso de filter en puedenZafar. La ventaja de utilziar esta funciones es que vuelven más declarativo y expresivo el codigo, ya que evitan usar bucles para recorrer listas.

La composicion se puede apreciar en ambas funciones, por ejemplo en totalDeuda se compone map con sum. Esto se puede ya que la imagen de map deudaMillones es el dominio de sum. La composicion es util para usar funciones auxiliares y hacer el codigo mas compacto.

La aplicacion parcial se puede ver en puedenZafar cuando se hace elem "Petroleo". Esto facilita la reutilizacion de funciones. 

-}

--Parte 4

recetasOrdenadas :: Pais -> [Receta] ->  Bool
recetasOrdenadas _ [] = True
recetasOrdenadas pais (r : rs) 
    |calcularPBI (aplicarReceta pais r) < calcularPBI(aplicarReceta pais (head rs)) = recetasOrdenadas pais rs
    |otherwise = False

--Parte 5

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

{-
Si evaluamos la funcion 4.a) nunca obtendremos un resultado, ya que Haskell continuará evaluando infinitamente.

En la 4.b) si devolverá un resultado ya que lo que se está evaluando en este caso es la deuda de cada país, no sus recursos naturales. Por ende, como Haskell utiliza evaluación diferida, no importa que los recursos naturales sean infinitos, ya que Haskell solo evalúa lo que necesita. 
-}