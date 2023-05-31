module Library where
import PdePreludat
import GHC.OldList (find)

--Defino mis tipos
type Nombre = String
type Edad = Number
type Peso = Number
type Enfermedad = String
type Hierba = Raton -> Raton
type Terminacion = String
type Medicamento = [Hierba]

data Raton = UnRaton {
    nombre :: Nombre,
    edad :: Edad,
    peso :: Peso,
    enfermedadesRaton :: [Enfermedad]
} deriving Show

--Funciones para delegar
noEsEnfermedad :: Terminacion -> Enfermedad -> Bool
noEsEnfermedad terminacion enfermedad = terminacion /= drop (length enfermedad - length terminacion) enfermedad

eliminarEnfermedades :: Terminacion -> [Enfermedad] -> [Enfermedad]
eliminarEnfermedades terminacion = filter (noEsEnfermedad terminacion)

eliminarEnfermedadPorLongitud :: Enfermedad -> Bool
eliminarEnfermedadPorLongitud enfermedad = length enfermedad > 10

eliminarEnfermedadesPorLongitud :: [Enfermedad] -> [Enfermedad]
eliminarEnfermedadesPorLongitud = filter eliminarEnfermedadPorLongitud

perderPeso :: Peso -> Peso
perderPeso peso 
    |peso > 2 = peso*0.9
    |otherwise = peso*0.95

perderPesoFijo :: Peso -> Peso
perderPesoFijo peso 
    |peso > 0.1 = peso - 0.1
    |otherwise = 0

--Punto 1
cerebro :: Raton
cerebro = UnRaton "Cerebro" 9 0.2 ["Brucelosis", "Sarampion", "Tuberculosis"]

bicenterrata :: Raton
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []

huesudo :: Raton
huesudo = UnRaton "Huesudo" 4 10 ["Alta Obesidad", "Sinusitis"]

--Punto 2
hierbaBuena :: Hierba
hierbaBuena raton = raton {edad = sqrt(edad raton)}

hierbaVerde :: Terminacion -> Hierba
hierbaVerde terminacion raton = raton {enfermedadesRaton = eliminarEnfermedades terminacion (enfermedadesRaton raton)}

alcachofa :: Hierba
alcachofa raton = raton {peso = perderPeso (peso raton)}

hierbaZort :: Hierba
hierbaZort raton = UnRaton "Pinky" 0 (peso raton) []

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = raton {peso = perderPesoFijo (peso raton), enfermedadesRaton = eliminarEnfermedadesPorLongitud (enfermedadesRaton raton)}

--Punto 3
sufijosInfecciosas = ["sis", "itis", "emia", "cocos"]

pondsAntiAge :: Medicamento
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]

reduceFatFast :: Number -> Medicamento
reduceFatFast potencia = hierbaVerde "Obesidad":replicate potencia alcachofa

pdepCilina :: Medicamento
pdepCilina = [hierbaVerde ((!!) sufijosInfecciosas 0), hierbaVerde ((!!) sufijosInfecciosas 1), hierbaVerde ((!!) sufijosInfecciosas 2), hierbaVerde ((!!) sufijosInfecciosas 3)]

aplicarHierba :: Raton -> Hierba -> Raton
aplicarHierba raton hierba = hierba raton

aplicarMedicamento :: Medicamento -> Raton  -> Raton
aplicarMedicamento medicamento raton = foldl aplicarHierba raton medicamento

--Punto 4
numerosNaturales = [1..]
comunidad = [huesudo]

obtenerPeso :: Raton -> Peso
obtenerPeso = peso

obtenerLongitudEnfermedades :: Raton -> Number
obtenerLongitudEnfermedades raton = length (enfermedadesRaton raton)

lograEstabilizar :: Medicamento -> Bool
lograEstabilizar medicamento = all ((<1).obtenerPeso.aplicarMedicamento medicamento) comunidad && all ((<3).obtenerLongitudEnfermedades.aplicarMedicamento medicamento) comunidad


