import System.IO
import Data.List.Split
-- Para compilar 
-- cabal update
-- cabal install --lib split

userDataChrg :: () -> IO [[String]] 
userDataChrg _ = do
    handle <- openFile "userData.txt" ReadMode
    content <- hGetContents handle        
    let atributes = map (splitOn ",") (lines content)
    return atributes

parkingDataChrg :: () -> IO [[String]] 
parkingDataChrg _ = do
    handle <- openFile "parkingData.txt" ReadMode
    content <- hGetContents handle
    let atributes = map (splitOn ",") (lines content)    
    return atributes

bikeDataChrg :: () -> IO [[String]] 
bikeDataChrg _ = do
    handle <- openFile "bikeData.txt" ReadMode
    content <- hGetContents handle
    let atributes = map (splitOn ",") (lines content)    
    return atributes

-- Función para mostrar el menú y obtener la selección del usuario
showMainMenu :: IO ()
showMainMenu = do
    putStrLn "-> Menú Principal"
    putStrLn "   1) Cargar y mostrar parqueos"
    putStrLn "   2) Mostrar y asignar bicicletas"
    putStrLn "   3) Cargar usuarios"
    putStrLn "   4) Estadisticas"
    putStrLn "   5) Salir"
    putStrLn "Seleccione una opción:"

-- Función principal para manejar las opciones del menú
mainMenu :: IO ()
mainMenu = do
    showMainMenu
    option <- getLine
    putStrLn ""
    case option of
        "1" -> do
            putStrLn "Has seleccionado la Opción 1."
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
    --parkingData :: [[String]]
    parkingContent <- parkingDataChrg ()
    print parkingContent
    putStrLn "\n¡Bienvenido al sistema de alquiler de bicletas!\n"
    mainMenu


{-main :: IO ()
main = do 
    putStrLn "Prueba IO"
    userContent <- userDataChrg ()
    print userContent
    parkingContent <- parkingDataChrg ()
    print parkingContent
    bikeContent <- bikeDataChrg ()
    print bikeContent-}