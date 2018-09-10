module Tictactoe where

import Data.Maybe (listToMaybe)
import Data.Char
import Data.List

{-
    Tictactoe.hs

    El clásico juego "ta-te-ti" hecho en Haskell

    Representación del tablero y sus posiciones:

        0 | 1 | 2
        ---------
        3 | 4 | 5
        ---------
        6 | 7 | 8

    Posiciones del tablero:
        Sin jugador es el entero 0
        Jugador "X" es el entero 1
        Jugador "O" es el entero 2
        
-}

-- Jugador
data Player = X | O deriving (Eq)

-- Juego. Contiene jugador actual y tablero actual
data Game = Game Player Board deriving (Eq, Show)

-- Una acción
data Action = Action Integer deriving (Eq)

-- Tablero. Lista de 8 enteros
type Board = [Integer]

-- Show para jugador
instance Show (Player) where
    show X = show "X"
    show O = show "O"

-- Show para acción
instance Show (Action) where
    show (Action action) = show action ++ " "

-- Inicio del juego. Devuelve tablero inicial.
-- 0 es vacío, 1 es X, 2 es O
beginning :: Game
beginning = Game X [0,0,0,0,0,0,0,0,0]

-- Representación del tablero actual
showBoard :: Game -> String
showBoard (Game player b) = "Current player: " ++ (show player) ++ "\n\n" ++ r1 ++ r2 ++ r3
    where
        r1 = " " ++ (intToPlayerStr (b!!0)) ++ " | " ++ (intToPlayerStr (b!!1)) ++ " | " ++ (intToPlayerStr (b!!2)) ++ "\n" ++ "-----------" ++ "\n"
        r2 = " " ++ (intToPlayerStr (b!!3)) ++ " | " ++ (intToPlayerStr (b!!4)) ++ " | " ++ (intToPlayerStr (b!!5)) ++ "\n" ++ "-----------" ++ "\n"
        r3 = " " ++ (intToPlayerStr (b!!6)) ++ " | " ++ (intToPlayerStr (b!!7)) ++ " | " ++ (intToPlayerStr (b!!8)) ++ "\n"

-- Devuelve el jugador opuesto
getOppositePlayer :: Player -> Player
getOppositePlayer X = O
getOppositePlayer O = X

-- Representación del entero posición
intToPlayerStr :: Integer -> String
intToPlayerStr 0 = " "
intToPlayerStr 1 = "X"
intToPlayerStr 2 = "O"

-- Devuelve 1 para jugador X y 2 para jugador O
playerToInt :: Player -> Integer
playerToInt X = 1
playerToInt O = 2

-- Acciones disponibles para el jugador actual
actions :: Game -> [(Player, [Action])]
actions (Game player board)
    | result (Game player board) /= [] = [(X, []), (O, [])]
    | player == X = [(X,(nextActions (Game X board))),(O,[])]
    | player == O = [(X,[]),(O,(nextActions (Game O board)))]

-- Devuelve una lista con las acciones disponibles
nextActions :: Game -> [Action]
nextActions (Game player board) = [(Action y) | (x,y) <- zip board [0..], x == 0]

-- Determina el resultado del juego
-- Si el juego sigue, se retorna lista vacía
result :: Game -> [(Player, Int)]
result (Game player b)
    | ((thereIsWinner player b) && (player == X)) = [(X,1), (O,(-1))]
    | ((thereIsWinner player b) && (player == O)) = [(X,(-1)), (O,1)]
    | otherwise = []

-- Determina si hay ganador en el tablero actual
thereIsWinner :: Player -> Board -> Bool
thereIsWinner player b = winner
    where
        playerInt = (playerToInt player)
        winner = (((b!!0) == (playerInt)) && ((b!!1) == (playerInt)) && ((b!!2) == (playerInt))) || (((b!!3) == (playerInt)) && ((b!!4) == (playerInt)) && ((b!!5) == (playerInt))) || (((b!!6) == (playerInt)) && ((b!!7) == (playerInt)) && ((b!!8) == (playerInt))) || (((b!!0) == (playerInt)) && ((b!!3) == (playerInt)) && ((b!!6) == (playerInt))) || (((b!!1) == (playerInt)) && ((b!!4) == (playerInt)) && ((b!!7) == (playerInt))) || (((b!!2) == (playerInt)) && ((b!!5) == (playerInt)) && ((b!!8) == (playerInt))) || (((b!!0) == (playerInt)) && ((b!!4) == (playerInt)) && ((b!!8) == (playerInt))) || (((b!!2) == (playerInt)) && ((b!!4) == (playerInt)) && ((b!!6) == (playerInt)))

-- Devuelve el jugador actual
activePlayer :: Game -> Maybe Player
activePlayer g = listToMaybe [p | (p, as) <- actions g, not (null as)]

-- Retorna acción según ingreso del usuario
readAction :: String -> Action
readAction s
    | (length s) == 1 = (Action (read s::Integer))
    | otherwise = error "Invalid action!"

-- Aplica una acción de jugador al juego actual
next :: Game -> (Player, Action) -> Game
next (Game player board) (playerAct, (Action action)) = (Game (getOppositePlayer playerAct) newBoard)
    where
        newBoard = [if y == action then (playerToInt player) else x | (x,y) <- (zip board [0..])]