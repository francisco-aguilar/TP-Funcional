module MicroEntrega1 where

type Acumulador = Float
type Posiciones = Int
type Contador = Int 
type Etiqueta = String 
type Instruccion = Microprocesador -> Microprocesador 

-- Justificación de la estructura elegida: creamos una estructura de tipo data, para sumar expresividad y evitar definiciones que podríamos considerar “redundantes”.
-- La memoria de datos la declaramos como lista porque la consigna la define como una estructura con una "gran cantidad de posiciones" conteniendo las mismas datos del mismo tipo. 
-- Los acumuladores son de tipo Float para poder trabajar con ellos a lo largo del programa. 
-- El program counter es de tipo Int, dado que se debe incrementar cada vez que realizamos una instrucción, siendo este siempre un número entero. 
-- La etiqueta es de tipo String ya que nos debe imprimir en pantalla un mensaje de error. 

data Microprocesador = Microprocesador {memoriaDeDatos :: [Posiciones], acumuladorA :: Acumulador, acumuladorB :: Acumulador, programCounter :: Contador, etiqueta :: Etiqueta}

xt8088 = Microprocesador {memoriaDeDatos = [1, 2, 3, 4, 5], acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = ""}
fp20 = Microprocesador {memoriaDeDatos = [], acumuladorA = 7, acumuladorB = 24, programCounter = 0, etiqueta = ""}
at8086 = Microprocesador {memoriaDeDatos = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = ""}

incrementarPC microprocesador = microprocesador {programCounter = (programCounter microprocesador) + 1}

nop :: Instruccion 
nop = incrementarPC  

punto2 xt8088 = (nop.nop.nop) xt8088

-- El concepto que interviene para lograr este punto es el de composición.


lodV :: Microprocesador -> Float -> Microprocesador
lodV microprocesador val = microprocesador {acumuladorA = val}

swap :: Instruccion 
swap microprocesador = (incrementarPC.swap1) microprocesador

swap1 :: Instruccion 
swap1 microprocesador = microprocesador {acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador}

add :: Instruccion 
add microprocesador = (incrementarPC.add1) microprocesador

add1 :: Instruccion 
add1 microprocesador = (resetearAcumuladorB.sumarAcumuladores) microprocesador

sumarAcumuladores :: Instruccion 
sumarAcumuladores microprocesador = microprocesador {acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador}

resetearAcumuladorB :: Instruccion 
resetearAcumuladorB microprocesador = microprocesador {acumuladorB = 0}

punto3 :: Microprocesador -> Microprocesador
punto3 xt8088 = (add.(lodV 22).swap.(lodV 10)) xt8088

divide :: Instruccion 
divide microprocesador = (incrementarPC.divide1) microprocesador

divide1 :: Instruccion
divide1 microprocesador = (resetearAcumuladorB.divisionAcumuladores) microprocesador

divisionAcumuladores :: Instruccion 
divisionAcumuladores microprocesador = microprocesador {acumuladorA  = acumuladorA microprocesador / acumuladorB microprocesador}


lod :: Microprocesador -> Int -> Microprocesador
lod microprocesador addr = (incrementarPC.(lod microprocesador)) addr  

lod1 :: Microprocesador -> Int -> Microprocesador
lod1 microprocesador addr = microprocesador {acumuladorA  = (!!) (memoriaDeDatos microprocesador) addr}


str :: Microprocesador -> Int -> Int -> Microprocesador
str microprocesador addr val = (incrementarPC.((str1 microprocesador) addr val))

str1 :: Microprocesador -> Int -> Int -> Microprocesador
str1 microprocesador addr val = microprocesador {memoriaDeDatos =(((++) (listaPrimerosElementos addr microprocesador)).((agregarElementoNuevo val).(colaDeLaLista addr)) microprocesador)}

listaPrimerosElementos :: Int -> Microprocesador -> [Posiciones]
listaPrimerosElementos addr microprocesador = take (addr-1) (memoriaDeDatos microprocesador)

agregarElementoNuevo :: Int -> [Posiciones] -> [Posiciones]
agregarElementoNuevo val unaLista = (:) val unaLista

colaDeLaLista :: Int -> Microprocesador -> [Posiciones]
colaDeLaLista addr microprocesador = drop addr (memoriaDeDatos microprocesador)

punto4 xt8088 = (divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)) xt8088
