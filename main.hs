import System.IO
import Data.List.Split
import System.Directory (doesFileExist)

-- ***** Para compilar *****
-- cabal update
-- cabal install --lib split

 
-- tipo definido para manejar los datos de las coordenadas
type Coordinate = (String, String)

-- Entradas: La lista con la informacion de los parqueos
-- Salidas: No posee
-- Función para imprimir una fila de datos en el formato especificado
printFormattedRow :: [String] -> IO ()
printFormattedRow [id, nombre, direccion, provincia, ubicacionX, ubicacionY] = do
    putStrLn ("Información del Parqueo")
    putStrLn ("id: " ++ id)
    putStrLn ("nombre: " ++ nombre)
    putStrLn ("direccion: " ++ direccion)
    putStrLn ("provincia: " ++ provincia)
    putStrLn ("ubicacion x: " ++ ubicacionX)
    putStrLn ("ubicacion y: " ++ ubicacionY)
    putStrLn("")

printFormattedRow _ = putStrLn "Error: Datos incompletos o incorrectos en alguna fila del archivo de texto\n."

-- Entradas: La lista con la información de los parqueos
-- Salidas: No posee
-- Función para imprimir los datos en el formato especificado
printFormattedData :: [[String]] -> IO ()
printFormattedData dataRows = do
    putStrLn "\n-> Parqueos disponibles en el sistema:\n"
    mapM_ printFormattedRow dataRows -- En esta linea se aplica la función printFormattedRow a cada fila de datos en dataRows, lo que resulta en la impresión de cada fila en el formato establecido.

-- Entradas: Un string que contiene la direccion del archivo que se desea leer.
-- Salidas: Una lista con la informacion de lectura.
-- Función para leer el archivo y obtener los datos como una lista de listas de strings
readFileData :: FilePath -> IO [[String]]
readFileData filePath = do
    -- Leer el contenido del archivo
    fileContents <- readFile filePath
    -- Dividir el contenido en líneas y luego dividir cada línea en campos
    let linesOfFile = lines fileContents
        dataRows = map (splitOn ", ") linesOfFile
    return dataRows

-- Entradas: Una lista con la informacion parcial de las bicicletas
-- Salidas: Una lista con el neuevo formato
-- Funcion que agregra los campos adicionales que se necitarán para después
formattedList :: [String] -> [String]
formattedList [id, typeI] = 
    [id, typeI, "null", "false"]
formattedList _ = ["null"]  -- Devolver una lista con un solo elemento "null" en caso de error

-- Entradas: No posee
-- Salidas: Una lista con la información de los usuarios leida.
-- Función que carga la información de los usuarios
chargeUserData :: IO [[String]]
chargeUserData = do
    let filePath = "userData.txt"
    -- Verificar si el archivo existe
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            dataRows <- readFileData filePath
            return dataRows
        else do
            putStrLn "Error: No se ha podido cargar los datos de los usuarios. Por favor verifica que exista el archivo userData.txt.\n"
            return []

-- Entradas: No posee.
-- Salidas: Una lista con la información parcial leida de las bicicletas.
-- función encargada de cargar los datos de los bicicletas en el archivo "bikeData.txt"
preChargeBikes :: IO [[String]]
preChargeBikes = do
    let filePath = "bikeData.txt"
    -- Verificar si el archivo existe
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            dataRows <- readFileData filePath
            let newFormattedList = map formattedList dataRows
            return newFormattedList
        else do
            putStrLn "Error: No se ha podido cargar las bicicletas. Por favor verifica que exista el archivo bikeData.txt.\n"
            return []

-- Entradas: No posee.
-- Salidas: Una lista con la asosiación entre bicicleta y parqueo.
-- Esta función se encarga de leer los datos relacionados a la asosiación
chargeBikeLocation :: IO [[String]]
chargeBikeLocation = do
    let filePath = "bikeLocation.txt"
    -- Verificar si el archivo existe
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            dataRows <- readFileData filePath
            return dataRows
        else do
            putStrLn "Error: No se ha podido cargar la ubicación bicicletas. Por favor verifica que exista el archivo bikeLocation.txt.\n"
            return []

-- función encargada de cargar los datos de los parqueos según la dirección ingresada
loadShowParking :: IO [[String]]
loadShowParking = do
    putStr "Por favor ingrese la dirección del archivo: "
    hFlush stdout
    filePath <- getLine  -- Obtener la dirección del archivo desde la entrada del usuario
    -- Verificar si el archivo existe
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            dataRows <- readFileData filePath
            printFormattedData dataRows
            return dataRows  -- Devolver dataRows
        else do
            putStrLn "Error: No se ha podido encontrar el archivo. Por favor verifique la dirección e intente nuevamente.\n"
            return []  -- Devolver una lista vacía en caso de error

-- Entradas: La lista con la información de los parqueos.
-- Salidas: No posee.
-- función que solicita las coordenadas para localizar el parqueo de bicicletas más cercano 
checkForBikes ::  [[String]] -> IO ()
checkForBikes parkingDataList  = do 
    putStrLn "-> Consulta de bicicletas"
    putStrLn "-> Ingrese las coordenadas para localizar el parqueo más cercano"
    putStr "-> Posición X: "
    axisX <- getLine
    putStr "-> Posición Y: "
    axisY <- getLine

    -- tipo definido para manejar los datos de las coordenadas
    let userCoordinate = (axisX, axisY) :: Coordinate
    let nearestParking = findNearestCoordinate userCoordinate parkingDataList
    putStrLn $ "Parqueo más cercano: " ++ show nearestParking
    
-- Función que recorre el arreglo que contiene los datos de los parqueos de bicicletas   
findNearestCoordinate :: Coordinate -> [[String]] -> Coordinate    
findNearestCoordinate _ [] = ("No hay datos", "No hay datos")
findNearestCoordinate coord dataList =
    let nearest = foldl (compareDistance coord) (head dataList) (tail dataList)
    in (nearest !! 4, nearest !! 5)

compareDistance :: Coordinate -> [String] -> [String] -> [String]
compareDistance coord p1 p2 =
    if calculateDistance coord (p1 !! 4, p1 !! 5) < calculateDistance coord (p2 !! 4, p2 !! 5)
        then p1
        else p2
-- Función que calcula la distancia entre dos puntos, la cual es vista en matemática general
-- y adaptada con las funciones del lenguaje haskell
-- https://es.khanacademy.org/math/geometry/hs-geo-analytic-geometry/hs-geo-distance-and-midpoints/v/distance-formula#:~:text=Podemos%20volver%20a%20escribir%20el,distancia%20entre%20cualesquiera%20dos%20puntos.
calculateDistance :: Coordinate -> Coordinate -> Double
calculateDistance (x1, y1) (x2, y2) =
    sqrt $ (read x2 - read x1) ** 2 + (read y2 - read y1) ** 2

-- Función encargada de imprimir el menú que menú de estadísticas del programa 
showStadisticsMenu :: IO ()
showStadisticsMenu = do      
    putStrLn "-> Menú de estadísticas"
    putStrLn "   1) TOP - 5 Bicicletas con más viajes realizados"
    putStrLn "   2) TOP - 5 parqueos con más viajes (salida-destino) "
    putStrLn "   3) TOP - 3 Usuarios con más kilómetros recorridos"
    putStrLn "   4) Resumen total de viajes"
    putStrLn "   5) Retroceder al menú principal"
    putStr "Seleccione una opción: "
    hFlush stdout

-- Función encargada de gestionar las opciones del menú de estadísticas del programa 
stadisticsMenu :: [[String]] -> [[String]] -> [[String]] -> IO ()
stadisticsMenu parkingDataList bikeDataList userDataList = do 
    showStadisticsMenu
    option <- getLine
    putStrLn ""
    case option of
        "1" -> do
            putStrLn "Has seleccionado la Opción 1."
            stadisticsMenu parkingDataList bikeDataList userDataList
        "2" -> do
            putStrLn "Has seleccionado la Opción 2."
            stadisticsMenu parkingDataList bikeDataList userDataList
        "3" -> do
            putStrLn "Has seleccionado la Opción 3."
            stadisticsMenu parkingDataList bikeDataList userDataList
        "4" -> do
            putStrLn "Has seleccionado la Opción 4."
            stadisticsMenu parkingDataList bikeDataList userDataList
        "5" -> do
            putStrLn "De vuelta al menú principal."
            mainMenu parkingDataList bikeDataList userDataList
        _   -> do
            putStrLn "\nError: Opción inválida. Por favor, selecciona una opción válida.\n"
            stadisticsMenu parkingDataList bikeDataList userDataList


-- Función para mostrar el menú de opciones generales
showGeneralOptions :: IO ()
showGeneralOptions = do
    putStrLn "-> Opciones generales"
    putStrLn "   1) Consultar bicicletas"
    putStrLn "   2) Alquilar"
    putStrLn "   3) Facturar"
    putStrLn "   4) Volver"
    putStr "Seleccione una opción: "
    hFlush stdout

-- Función encargada de gestiones las opciones generales del programa    
generalOptions :: [[String]] -> [[String]] -> [[String]] -> IO ()
generalOptions parkingDataList bikeDataList userDataList= do 
    showGeneralOptions
    option <- getLine
    putStrLn ""
    case option of
        "1" -> do
            checkForBikes parkingDataList 

            
        "2" -> do
            putStrLn "Has seleccionado la Opción 2."
            
        "3" -> do
            putStrLn "Has seleccionado la Opción 3."
                  
        "4" -> do
            putStrLn "De vuelta al menú principal."
            mainMenu parkingDataList bikeDataList  userDataList
        _   -> do
            putStrLn "\nError: Opción inválida. Por favor, selecciona una opción válida.\n"
            generalOptions parkingDataList bikeDataList  userDataList


-- Función para mostrar el menú y obtener la selección del usuario
showMainMenu :: IO ()
showMainMenu = do
    putStrLn "-> Menú Principal"
    putStrLn "   1) Cargar y mostrar parqueos"
    putStrLn "   2) Mostrar y asignar bicicletas"
    putStrLn "   3) Cargar usuarios"
    putStrLn "   4) Estadisticas"
    putStrLn "   5) Opciones generales"
    putStrLn "   6) Salir"
    putStr "Seleccione una opción: "
    hFlush stdout

-- Función principal para manejar las opciones del menú
mainMenu :: [[String]] -> [[String]] -> [[String]] -> IO ()
mainMenu parkingDataList bikeDataList userDataList= do
    showMainMenu
    option <- getLine
    putStrLn ""
    case option of
        "1" -> do
            newParkingDataList <- loadShowParking
            mainMenu newParkingDataList bikeDataList userDataList
        "2" -> do
            putStrLn "Biciletas registradas:"
            mapM_ print bikeDataList
            bikeLocationList <- chargeBikeLocation
            putStrLn "Ubicación de las biciletas:"
            mapM_ print bikeLocationList
            mainMenu parkingDataList bikeDataList userDataList
        "3" -> do
            newUserDataList <- chargeUserData
            putStrLn "Datos de usuarios:"
            mapM_ print newUserDataList
            mainMenu parkingDataList bikeDataList userDataList

        "4" -> do            
            stadisticsMenu parkingDataList bikeDataList userDataList
        "5" -> do            
            putStrLn "Has seleccionado la Opción 5."
            generalOptions parkingDataList bikeDataList userDataList
        "6" -> putStrLn "Programa finalizado."
        _   -> do
            putStrLn "\nError: Opción inválida. Por favor, selecciona una opción válida.\n"
            mainMenu parkingDataList bikeDataList userDataList

main :: IO ()
main = do
    putStrLn "\n¡Bienvenido al sistema de alquiler de bicicletas!\n"
    let parkingDataList = []  -- Lista vacía para datos de estacionamientos
        userDataList = []
    bikeDataList <- preChargeBikes -- Obtener la lista de datos de bicicletas desde preChargeBikes
    mainMenu parkingDataList bikeDataList userDataList