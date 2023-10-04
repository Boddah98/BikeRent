import System.IO
import Data.List.Split
import System.Directory (doesFileExist)
-- Para compilar 
-- cabal update
-- cabal install --lib split

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

loadShowParking :: IO ()
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
        else putStrLn "Error: No se ha podido encontrar el archivo. Por favor verifique la direccion e intente nuevamente.\n"

-- Función para mostrar el menú y obtener la selección del usuario
showMainMenu :: IO ()
showMainMenu = do
    putStrLn "-> Menú Principal"
    putStrLn "   1) Cargar y mostrar parqueos"
    putStrLn "   2) Mostrar y asignar bicicletas"
    putStrLn "   3) Cargar usuarios"
    putStrLn "   4) Estadisticas"
    putStrLn "   5) Salir"
    putStr "Seleccione una opción: "
    hFlush stdout

-- Función principal para manejar las opciones del menú
mainMenu :: IO ()
mainMenu = do
    showMainMenu
    option <- getLine
    putStrLn ""
    case option of
        "1" -> do
            loadShowParking
            mainMenu
        "2" -> do
            putStrLn "Has seleccionado la Opción 2."
            mainMenu
        "3" -> do
            putStrLn "Has seleccionado la Opción 3."
            mainMenu
        "4" -> do
            putStrLn "Has seleccionado la Opción 4."
            mainMenu
        "5" -> putStrLn "Programa finalizado."
        _   -> do
            putStrLn "\nError: Opción inválida. Por favor, selecciona una opción válida.\n"
            mainMenu

main :: IO ()
main = do
    putStrLn "\n¡Bienvenido al sistema de alquiler de bicletas!\n"
    mainMenu