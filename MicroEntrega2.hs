module MicroEntrega1 where

type Acumulador = Int
type Posiciones = Int
type Contador = Int 
type Etiqueta = String 
type Instruccion = Microprocesador -> Microprocesador 

-- Justificación de la estructura elegida: creamos una estructura de tipo data, para sumar expresividad y evitar definiciones que podríamos considerar “redundantes”.
-- La memoria de datos la declaramos como lista porque la consigna la define como una estructura con una "gran cantidad de posiciones" conteniendo las mismas datos del mismo tipo. 
-- Los acumuladores son de tipo Float para poder trabajar con ellos a lo largo del programa. 
-- El program counter es de tipo Int, dado que se debe incrementar cada vez que realizamos una instrucción, siendo este siempre un número entero. 
-- La etiqueta es de tipo String ya que nos debe imprimir en pantalla un mensaje de error. 

data Microprocesador = Microprocesador {
										memoriaDeDatos :: [Posiciones],
										acumuladorA :: Acumulador,
										acumuladorB :: Acumulador,
										programCounter :: Contador,
										etiqueta :: Etiqueta,
										programas :: [Instruccion]
										}

xt8088 = Microprocesador {memoriaDeDatos = [1, 2, 3, 4, 5], acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = ""}
fp20 = Microprocesador {memoriaDeDatos = [], acumuladorA = 7, acumuladorB = 24, programCounter = 0, etiqueta = ""}
at8086 = Microprocesador {memoriaDeDatos = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = ""}


nop :: Instruccion 
nop microprocesador = microprocesador {programCounter = (programCounter microprocesador) + 1}

-- ejecutarInstruccion (nop.nop.nop) xt8088

-- El concepto que interviene para lograr este punto es el de composición.


lodV :: Int -> Instruccion 
lodV val microprocesador = microprocesador {acumuladorA = val}

swap :: Instruccion 
swap microprocesador = microprocesador {acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador}

add :: Instruccion 
add microprocesador = (resetearAcumuladorB.sumarAcumuladores) microprocesador

sumarAcumuladores :: Instruccion 
sumarAcumuladores microprocesador = microprocesador {acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador}

resetearAcumuladorB :: Instruccion 
resetearAcumuladorB microprocesador = microprocesador {acumuladorB = 0}

-- ejecutarInstruccion (add.(lodV 22).swap.(lodV 10)) xt8088

divide :: Instruccion
divide microprocesador = (resetearAcumuladorB.divisionAcumuladores) microprocesador

divisionAcumuladores :: Instruccion 
divisionAcumuladores microprocesador = microprocesador {acumuladorA  = div (acumuladorA microprocesador) (acumuladorB microprocesador)}

lod :: Microprocesador -> Int -> Microprocesador
lod microprocesador addr = microprocesador {acumuladorA  = (!!) (memoriaDeDatos microprocesador) addr}

str :: Microprocesador -> Int -> Int -> Microprocesador
str microprocesador addr val = microprocesador {memoriaDeDatos = (strInsertarEnPosicion addr (memoriaDeDatos microprocesador) val)}

strInsertarEnPosicion :: Int -> [Posiciones] -> Int -> [Posiciones]
strInsertarEnPosicion 1 [] val = val:[]
strInsertarEnPosicion 1 (x:xs) val = val:(x:xs)
strInsertarEnPosicion n (x:xs) val = x:strInsertarEnPosicion (n-1) xs val


-- ejecutarInstruccion (divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)) xt8088

--------------------------------------------------------------------------------------------------------------------------------------------

--Punto 3.1.--

cargarPrograma  :: Instruccion -> Instruccion
cargarPrograma programa microprocesador = microprocesador {programas = programa : (programas microprocesador)}

--cargarPrograma ((add.(lodV 22).swap.(lodV 10))) xt8088
--cargarPrograma ((divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2))) xt8088

--------------------------------------------------------------------------------------------------------------------------------------------

--Punto 3.2.--

ejecutarInstruccion :: Microprocesador -> Instruccion -> Microprocesador
ejecutarInstruccion microprocesador instruccion 	| (etiqueta microprocesador) /= "" = microprocesador
													| otherwise = (nop.instruccion) microprocesador

--------------------------------------------------------------------------------------------------------------------------------------------

--Punto 3.3.--

ifnz :: Instruccion -> Instruccion
ifnz instrucciones (Microprocesador memoriaDeDatos 0 acumuladorB programCounter etiqueta programas) = (Microprocesador memoriaDeDatos 0 acumuladorB programCounter etiqueta programas)
ifnz instrucciones microprocesador = instrucciones microprocesador

--------------------------------------------------------------------------------------------------------------------------------------------

--Punto 3.4.--

depurar microprocesador = filter (not.estanDatosEnCero) (map (ejecutarInstruccion xt8088) (programas microprocesador))
estanDatosEnCero (Microprocesador memoriaDeDatos acumuladorA acumuladorB programCounter etiqueta programa) = acumuladorA == 0 && acumuladorB == 0 && all (==0) memoriaDeDatos

--------------------------------------------------------------------------------------------------------------------------------------------

--Punto 3.5.--

memoriaOrdenada :: Microprocesador -> Bool
memoriaOrdenada microprocesador = controlarOrden (memoriaDeDatos microprocesador) 
controlarOrden :: [Posiciones] -> Bool
controlarOrden [] = True
controlarOrden [_] = True
controlarOrden (x:y:xs) = (x<=y) && controlarOrden (y:xs)

--------------------------------------------------------------------------------------------------------------------------------------------

--Punto 3.6.--

microMmInfinita = Microprocesador {memoriaDeDatos = [0..], acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = "", programas = []}

--Si queremos cargar y ejecutar el programa que sume 10 y 22 se obtiene el resultado deseado sin ningún tipo de problema,
--ya que estas operaciones no requieren del uso de la memoria.

--Si queremos saber si la memoria está ordenada, nunca terminará de evaluarla ya que los elementos de la misma son infinitos.

--Se van a presentar conflictos cuando se quiera evaluar la memoria de datos ya que al ser esta una lista infinita,
-- nunca va a lograr evaluar todos los elementos que contiene por lo que va a evaluar indefinidamente.

---------------------------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------Casos de Prueba---------------------------------------------------------------

--Punto 4.2.--

--(ejecutarInstruccion.cargarPrograma (add.(lodV 22).swap.(lodV 10))) xt8088

--(ejecutarInstruccion.cargarPrograma (divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2))) xt8088

--Punto 4.3.--

--(ifnz (swap.(lodv 3)))  fp20

--(ifnz (swap.(lodv 3)))  xt8088

--Punto 4.4.--

--(depurar.((str 2 0).(str 1 3).(lodv 0).(lodv 133).nop.swap)) xt8088

--Punto 4.5.--

--microDesordenado = Microprocesador {memoriaDeDatos = [2, 5, 1, 0, 6, 9], acumuladorA = 0, acumuladorB = 0, programCounter = 0, etiqueta = "", programas []}

--memoriaOrdenada at8086
--memoriaOrdenada microDesorden



