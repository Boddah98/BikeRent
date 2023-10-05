import System.IO
import Data.List.Split
import System.Directory (doesFileExist)
-- Para compilar 
-- cabal update
-- cabal install --lib split

-- Función para asignar los ids de parqueos a las bicicletas
-- Función para asignar los ids de parqueos a las bicicletas
assignParqueoIds :: [[String]] -> [[String]] -> [[String]]
assignParqueoIds bikeData assignmentList = map (\[id_bicicleta, tipo, _, _] ->
    case lookup id_bicicleta assignmentList of
        Just id_parqueo -> [id_bicicleta, tipo, id_parqueo, "Disponible"]
        Nothing -> [id_bicicleta, tipo, "Sin Asignar", "No disponible"]
    ) bikeData

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

-- Función para imprimir los datos en el formato especificado
printFormattedData :: [[String]] -> IO ()
printFormattedData dataRows = do
    putStrLn "\n-> Parqueos disponibles en el sistema:\n"
    mapM_ printFormattedRow dataRows -- En esta linea se aplica la función printFormattedRow a cada fila de datos en dataRows, lo que resulta en la impresión de cada fila en el formato establecido.

-- Función para leer el archivo y obtener los datos como una lista de listas de strings
readFileData :: FilePath -> IO [[String]]
readFileData filePath = do
    -- Leer el contenido del archivo
    fileContents <- readFile filePath
    -- Dividir el contenido en líneas y luego dividir cada línea en campos
    let linesOfFile = lines fileContents
        dataRows = map (splitOn ", ") linesOfFile
    return dataRows

formattedList :: [String] -> [String]
formattedList [id, typeI] = 
    [id, typeI, "null", "false"]
formattedList _ = ["null"]  -- Devolver una lista con un solo elemento "null" en caso de error

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
stadisticsMenu :: [[String]] -> [[String]] -> IO ()
stadisticsMenu parkingDataList bikeDataList = do 
    showStadisticsMenu
    option <- getLine
    putStrLn ""
    case option of
        "1" -> do
            putStrLn "Has seleccionado la Opción 1."
            stadisticsMenu parkingDataList bikeDataList
        "2" -> do
            putStrLn "Has seleccionado la Opción 2."
            stadisticsMenu parkingDataList bikeDataList
        "3" -> do
            putStrLn "Has seleccionado la Opción 3."
            stadisticsMenu parkingDataList bikeDataList
        "4" -> do
            putStrLn "Has seleccionado la Opción 4."
            stadisticsMenu parkingDataList bikeDataList
        "5" -> do
            putStrLn "De vuelta al menú principal."
            mainMenu parkingDataList bikeDataList
        _   -> do
            putStrLn "\nError: Opción inválida. Por favor, selecciona una opción válida.\n"
            stadisticsMenu parkingDataList bikeDataList

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
generalOptions :: [[String]] -> [[String]] -> IO ()
generalOptions parkingDataList bikeDataList = do 
    showGeneralOptions
    option <- getLine
    putStrLn ""
    case option of
        "1" -> do
            putStrLn "Has seleccionado la Opción 1."
            
        "2" -> do
            putStrLn "Has seleccionado la Opción 2."
            
        "3" -> do
            putStrLn "Has seleccionado la Opción 3."
                  
        "4" -> do
            putStrLn "De vuelta al menú principal."
            mainMenu parkingDataList bikeDataList
        _   -> do
            putStrLn "\nError: Opción inválida. Por favor, selecciona una opción válida.\n"
            generalOptions parkingDataList bikeDataList


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
mainMenu :: [[String]] -> [[String]] -> IO ()
mainMenu parkingDataList bikeDataList = do
    showMainMenu
    option <- getLine
    putStrLn ""
    case option of
        "1" -> do
            newParkingDataList <- loadShowParking
            mainMenu newParkingDataList bikeDataList
        "2" -> do
            bikeLocationList <- chargeBikeLocation
            let assignmentListPairs = map (\[id_bicicleta, id_parqueo] -> (id_bicicleta, id_parqueo)) bikeDataList
            let newBikeDataList = assignParqueoIds bikeDataList assignmentListPairs
            putStrLn "Biciletas con localizaciones:"
            mapM_ print newBikeDataList
            mainMenu parkingDataList bikeDataList
        "3" -> do
            putStrLn "Has seleccionado la Opción 3."
            mainMenu parkingDataList bikeDataList
        "4" -> do            
            stadisticsMenu parkingDataList bikeDataList
        "5" -> do            
            putStrLn "Has seleccionado la Opción 5."
            generalOptions parkingDataList bikeDataList
        "6" -> putStrLn "Programa finalizado."
        _   -> do
            putStrLn "\nError: Opción inválida. Por favor, selecciona una opción válida.\n"
            mainMenu parkingDataList bikeDataList

main :: IO ()
main = do
    putStrLn "\n¡Bienvenido al sistema de alquiler de bicicletas!\n"
    let parkingDataList = []  -- Lista vacía para datos de estacionamientos
    bikeDataList <- preChargeBikes -- Obtener la lista de datos de bicicletas desde preChargeBikes
    mainMenu parkingDataList bikeDataList