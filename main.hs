import System.IO
import Data.List.Split
-- Para compilar 
-- cabal update
-- cabal install --lib split


userDataChrg :: () -> IO [[String]] 
userDataChrg _ = do
    handle <- openFile "C:/Users/andya/OneDrive/Escritorio/haskell/userData.txt" ReadMode
    content <- hGetContents handle        
    let atributes = map (splitOn ",") (lines content)
    return atributes

parkingDataChrg :: () -> IO [[String]] 
parkingDataChrg _ = do
    handle <- openFile "C:/Users/andya/OneDrive/Escritorio/haskell/parkingData.txt" ReadMode
    content <- hGetContents handle
    let atributes = map (splitOn ",") (lines content)    
    return atributes

bikeDataChrg :: () -> IO [[String]] 
bikeDataChrg _ = do
    handle <- openFile "C:/Users/andya/OneDrive/Escritorio/haskell/bikeData.txt" ReadMode
    content <- hGetContents handle
    let atributes = map (splitOn ",") (lines content)    
    return atributes

main :: IO ()
main = do 
    putStrLn "Prueba IO"
    userContent <- userDataChrg ()
    print userContent
    parkingContent <- parkingDataChrg ()
    print parkingContent
    bikeContent <- bikeDataChrg ()
    print bikeContent 