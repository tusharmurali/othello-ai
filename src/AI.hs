{-|
Module      : AI
Description : AIs for Othello
Copyright   : (c) 2021 Tushar Muralidharan
License     : AllRightsReserved
-}
module AI where

import Othello
import Data.List (maximumBy, group, sort)
import Data.Maybe (fromJust)

-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int -> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up.

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- his or her attention to marking.
ais :: [(String, AIFunc)]
ais = [ ("firstLegalMove", NoLookahead firstLegalMove),
        ("default", WithLookahead defaultAI)
      ]

-- | A very simple AI, which picks the first move returned by the
-- 'legalMoves' function. AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.
firstLegalMove :: GameState -> Move
firstLegalMove st = head (legalMoves st)

-- | A function that evaluates a game state based on mobility, frontier
-- discs, disc difference, placement, stability, and corners. 
evaluate :: GameState -> Double
evaluate st@(GameState _ _ board) = 
  sum $ zipWith (*) (weights board) 
  [mobility st, frontier st, pieces st, placement st, stability st, corners st]

-- | Accounts for number of moves available to the player.
mobility :: GameState -> Double
mobility st@(GameState (bx, by) (Turn player) board) = 100 
  * (myMoves - opponentMoves) 
  / (myMoves + opponentMoves + 1)
  where
    myMoves = fromIntegral $ length $ legalMoves st
    opponentMoves = fromIntegral $ length 
      $ legalMoves (GameState (bx, by) (Turn (otherPlayer player)) board)

-- | Accounts for number of open spaces next to enemy discs.
frontier :: GameState -> Double
frontier st@(GameState (bx, by) (Turn player) _) = 100 
  * (myFrontier - opponentFrontier) 
  / (myFrontier + opponentFrontier + 1)
  where
    myFrontier = fromIntegral $ length 
      $ filter (isFrontier (otherPlayer player)) (allPositions st)
    opponentFrontier = fromIntegral $ length 
      $ filter (isFrontier player) (allPositions st)
    isFrontier p (x, y) = case pieceAt st (x, y) of
      Nothing
        | x > 0 && pieceAt st (x - 1, y) == Just p -> True
        | x < bx - 1 && pieceAt st (x + 1, y) == Just p -> True
        | y < by - 1 && pieceAt st (x, y + 1) == Just p -> True
        | y > 0 && pieceAt st (x, y - 1) == Just p -> True
        | x > 0 && y > 0 && pieceAt st (x - 1, y - 1) == Just p -> True
        | x > 0 && y < by - 1 && pieceAt st (x - 1, y + 1) == Just p -> True
        | x < bx - 1 && y > 0 && pieceAt st (x + 1, y - 1) == Just p -> True
        | x < bx - 1 && y < by - 1 && pieceAt st (x + 1, y + 1) 
          == Just p -> True
        | otherwise -> False
      _ -> False

-- | Accounts for difference between the number of discs of the players.
pieces :: GameState -> Double
pieces (GameState _ (Turn player) board) = 100 
  * (myPieces - opponentPieces) 
  / (myPieces + opponentPieces + 1)
  where
    (myPieces, opponentPieces) = case player of
      Player1 -> (fromIntegral $ p1Score, fromIntegral $ p2Score)
      _ -> (fromIntegral $ p2Score, fromIntegral $ p1Score)
    (p1Score, p2Score) = countPieces board

-- | Accounts for ideal placements of pieces based on theoretical knowledge
-- of the game of Othello.
placement :: GameState -> Double
placement (GameState _ (Turn player) board) = fromIntegral 
  $ sum . map sum $ zipWith (zipWith $ weigh) squareScores board
  where
    squareScores = [ [100, -10,   8,   6,   6,   8,  -10,  100],
                     [-10, -25,  -4,  -4,  -4,  -4,  -25,  -10],
                     [  8,  -4,   6,   4,   4,   6,   -4,    8],
                     [  6,  -4,   4,   0,   0,   4,   -4,    6],
                     [  6,  -4,   4,   0,   0,   4,   -4,    6],
                     [  8,  -4,   6,   4,   4,   6,   -4,    8],
                     [-10, -25,  -4,  -4,  -4,  -4,  -25,  -10],
                     [100, -10,   8,   6,   6,   8,  -10,  100]
                    ]
    weigh :: Integer -> Maybe Player -> Integer
    weigh score piece 
      | piece == Just player = score
      | piece == Just (otherPlayer player) = -score
      | otherwise = 0

-- | Accounts for pieces that cannot be flipped over by the opponent on any
-- subsequent turn.
stability :: GameState -> Double
stability st@(GameState (bx, by) (Turn player) _) = 100 
  * (myStability - opponentStability) 
  / (myStability + opponentStability + 1)
  where
    myStability = fromIntegral $ length $ group $ sort 
      $ concatMap (countStable player) 
      [(0, 0), (0, by - 1), (bx - 1, 0), (bx - 1, by - 1)]
    opponentStability = fromIntegral $ length $ group $ sort 
      $ concatMap (countStable (otherPlayer player)) 
      [(0, 0), (0, by - 1), (bx - 1, 0), (bx - 1, by - 1)]
    countStable p (x, y)
      | x == 0 && y == 0 && pieceAt st (x, y) == Just p = 
        takeWhile (isPlayer p) [ (a, 0) | a <- [1..bx-1] ] 
        ++ takeWhile (isPlayer p) [ (0, b) | b <- [1..by-1] ] 
        ++ takeWhile (isPlayer p) [ (a, a) | a <- [1..min (bx-1) (by-1)] ]
      | x == 0 && y == by - 1 && pieceAt st (x, y) == Just p = 
        takeWhile (isPlayer p) [ (a, by-1) | a <- [1..bx-1] ] 
        ++ takeWhile (isPlayer p) [ (0, b) | b <- [by-2..0] ] 
        ++ takeWhile (isPlayer p) [ (a, by-a) | a <- [1..min (bx-1) (by-1)] ]
      | x == bx - 1 && y == 0 && pieceAt st (x, y) == Just p = 
        takeWhile (isPlayer p) [ (a, 0) | a <- [bx-2..0] ] 
        ++ takeWhile (isPlayer p) [ (bx-1, b) | b <- [1..by-1] ] 
        ++ takeWhile (isPlayer p) [ (bx-b, b) | b <- [1..min (bx-1) (by-1)] ]
      | x == bx - 1 && y == by - 1 && pieceAt st (x, y) == Just p = 
        takeWhile (isPlayer p) [ (a, by-1) | a <- [bx-2..0] ] 
        ++ takeWhile (isPlayer p) [ (bx-1, b) | b <- [by-2..0] ] 
        ++ takeWhile (isPlayer p) [ (a, a) | a <- [min (bx-1) (by-1)..1]]
      | otherwise = []
    isPlayer p position = (pieceAt st position == Just p)

-- | Accounts for whether a corner piece can be acquired in the next turn.
corners :: GameState -> Double
corners st@(GameState (bx, by) _ _)
  | Move (0, 0) `elem` myLegalMoves = 100
  | Move (0, by - 1) `elem` myLegalMoves = 100
  | Move (bx - 1, 0) `elem` myLegalMoves = 100
  | Move (bx - 1, by - 1) `elem` myLegalMoves = 100
  | otherwise = 0
    where myLegalMoves = legalMoves st

-- | Provides weights for the different heuristic functions used to evaluate
-- a game state. Weights differ in the endgame. I got these weights from a
-- Machine-Learning Tuned implementation of Othello: 
-- https://github.com/arminkz/Reversi
weights :: Board -> [Double]
weights board 
  | count == 56 = [33, -50, -15, 4, 416, 2153]
  | count == 57 = [46, -50, -1, 3, 612, 4141]
  | count == 58 = [51, -50, 62, 3, 595, 3184]
  | count == 59 = [33, -5, 66, 2, 384, 2777]
  | count == 60 = [44, 50, 163, 0, 443, 2568]
  | count == 61 = [13, 50, 66, 0, 121, 986]
  | count == 62 = [4, 50, 31, 0, 27, 192]
  | count == 63 = [8, 500, 77, 0, 36, 299]
  | otherwise = [8, 85, -40, 10, 210, 520]
  where
    count = p1Score + p2Score
    (p1Score, p2Score) = countPieces board

-- | An AI, which uses a depth-limited minimax search with alpha-beta pruning
-- and a custom heuristic function to make the best move for the player.
defaultAI :: GameState -> Int -> Move
defaultAI st@(GameState _ (Turn player) _) depth = maximumBy (\x y -> compare 
    (alphabeta (fromJust $ applyMove st x) player (depth-1) (-1/0) (1/0) False)
    (alphabeta (fromJust $ applyMove st y) player (depth-1) (-1/0) (1/0) False)
  ) (legalMoves st)

-- | A minimax algorithm with alpha-beta pruning to calculate the game tree 
-- used to determine the best move for the player. This algorithm evaluates 
-- the game states after reaching a specified depth.
alphabeta :: GameState -> Player -> Int -> Double -> Double -> Bool -> Double
alphabeta (GameState bounds (GameOver _) board) player _ _ _ _ = 
  evaluate (GameState bounds (Turn player) board)
alphabeta st@(GameState bounds (Turn currentPlayer) board) 
  player depth a b maximizingPlayer
  | depth == 0 = evaluate st
  | (maximizingPlayer && legalMoves st == []) 
    || (not maximizingPlayer && legalMoves 
    (GameState bounds (Turn (otherPlayer currentPlayer)) board) == []) = 
    alphabeta st player (depth - 1) a b (not maximizingPlayer)
  | maximizingPlayer = compareMax 
    (map (fromJust . applyMove st) (legalMoves st)) (-1/0) a
  | otherwise = compareMin 
    (map (fromJust . applyMove st) (legalMoves st)) (1/0) b
    where
      compareMax [] value _ = value
      compareMax (x:xs) value alpha 
        | newAlpha >= b = value
        | otherwise = compareMax xs newValue newAlpha
          where
            newAlpha = max alpha newValue
            newValue = max value (alphabeta x player (depth - 1) alpha b False)
      compareMin [] value _ = value
      compareMin (x:xs) value beta
        | newBeta <= a = value
        | otherwise = compareMin xs newValue newBeta
          where
            newBeta = min beta newValue
            newValue = min value (alphabeta x player (depth - 1) a beta True)
