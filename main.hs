import Tictactoe
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import System.Random

{-
    Match controller
    Controlador de la partida
-}
type Agent = Game -> IO (Maybe Action)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (Agent, Agent) -> Game -> IO [(Player, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showBoard g)
   case (activePlayer g) of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust $ elemIndex p [X, O])
         move <- ag g
         runMatch ags (Tictactoe.next g (p, fromJust move))

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: Player -> Agent
consoleAgent player state = do
   let moves = fromJust $ lookup player (actions state)
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat [" "++ show m | m <- moves])
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do
         putStrLn "Invalid move!"
         consoleAgent player state

randomAgent :: Player -> Agent
randomAgent player state = do
    let moves = fromJust $ lookup player (actions state)
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola o aleatorios.
-}
main :: IO [(Player, Int)]
main = do
   runMatch (consoleAgent X, randomAgent O) beginning
   --runMatch (consoleAgent X, consoleAgent O) beginning