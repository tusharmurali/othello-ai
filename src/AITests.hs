{-|
Module      : AITests
Description : Tests for your AI functions
Copyright   : (c) 2021 Tushar Muralidharan
License     : AllRightsReserved
-}
module AITests where

import           AI
import           Othello
import           Testing

aiTests :: Test
aiTests = TestGroup "AI"
  [ TestGroup "evaluate" [
      Test "evaluate initial state, 0" (assertEqual (evaluate (initialState (8, 8))) (0)),
      Test "evaluate random state, advantage for Player2" (assertApproxEqual (evaluate (GameState (8, 8) (Turn Player1) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player2, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player2, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (-566.206896)),
      Test "evaluate random state, advantage for Player1" (assertApproxEqual (evaluate (GameState (8, 8) (Turn Player2) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player1, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Just Player1, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (5136.887796)),
      Test "evaluate mobility test, 4 legal moves each, 0 value" (assertEqual (mobility (initialState (8, 8))) (0)),
      Test "evaluate mobility test, random pos, 5 legal moves each, 0 value" (assertEqual (mobility (GameState (8, 8) (Turn Player1) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player2, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player2, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (0)),
      Test "evaluate mobility test, random pos, Player1 has more moves" (assertApproxEqual (mobility (GameState (8, 8) (Turn Player2) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player1, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Just Player1, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (-18.181818)),
      Test "evaluate frontier test, 10 frontier spaces, 5 each" (assertEqual (frontier (initialState (8, 8))) (0)),
      Test "evaluate frontier test, random pos, Player2 has more frontier spaces" (assertApproxEqual (frontier (GameState (8, 8) (Turn Player1) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player2, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player2, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (-6.896551)),
      Test "evaluate frontier test, random pos, Player1 has more frontier squares" (assertApproxEqual (frontier (GameState (8, 8) (Turn Player2) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player1, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Just Player1, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (43.243243)),
      Test "evaluate pieces test, 4 pieces each" (assertEqual (pieces (initialState (8, 8))) (0)),
      Test "evaluate pieces test, random pos, equal pieces" (assertEqual (pieces (GameState (8, 8) (Turn Player1) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player2, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player2, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (0)),
      Test "evaluate pieces test, random pos, Player1 has more pieces" (assertApproxEqual (pieces (GameState (8, 8) (Turn Player2) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player1, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Just Player1, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (-41.666666)),
      Test "evaluate placement test, 4 middle squares weighted at 0" (assertEqual (placement (initialState (8, 8))) (0)),
      Test "evaluate placement test, Player1 has better disc placement" (assertEqual (placement (GameState (8, 8) (Turn Player1) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player2, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player2, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (2)),
      Test "evaluate placement test, Player1 has better disc placement on Player2's turn" (assertEqual (placement (GameState (8, 8) (Turn Player2) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player1, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Just Player1, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (-6)),
      Test "evaluate stability test, no unflippable discs, all in center" (assertEqual (stability (initialState (8, 8))) (0)),
      Test "evaluate stability test, random pos, no unflippable discs" (assertEqual (stability (GameState (8, 8) (Turn Player1) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player2, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player2, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (0)),
      Test "evaluate stability test, Player1 has unflippable discs" (assertApproxEqual (stability (GameState (8, 8) (Turn Player2) [
          [Just Player1, Just Player1, Just Player1, Just Player1, Nothing, Nothing, Nothing, Nothing], 
          [Just Player1, Just Player1, Just Player1, Just Player2, Nothing, Just Player1, Nothing, Nothing], 
          [Just Player1, Just Player1, Nothing, Just Player2, Just Player2, Just Player2, Nothing, Nothing], 
          [Just Player1, Just Player1, Nothing, Just Player1, Just Player2, Nothing, Nothing, Nothing], 
          [Just Player1, Nothing, Just Player1, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Just Player1, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ])
        ) (-88.888888))
    ],
    TestGroup "defaultAI" [
      Test "defaultAI depth 1: same behavior as greedy and firstLegalMove since all moves are equaivalent" (assertEqual (defaultAI (initialState (8, 8)) 1) (Move (4, 5))),
      Test "defaultAI depth 2: initial position" (assertEqual (defaultAI (initialState (8, 8)) 1) (Move (4, 5))),
      Test "defaultAI depth 2: random position" (assertEqual (defaultAI (GameState (8, 8) (Turn Player1) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player2, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player2, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ]) 1) (Move (1, 4))),
      Test "defaultAI depth 2: random position 2" (assertEqual (defaultAI (GameState (8, 8) (Turn Player2) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player1, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Just Player1, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ]) 1) (Move (5, 4)))
    ],
    TestGroup "alphabeta" [
      Test "alphaBeta depth 1: same behavior as greedy and firstLegalMove since all moves are equaivalent, highest eval" (assertApproxEqual (alphabeta (initialState (8, 8)) Player1 1 (-1/0) (1/0) False) (5538.947368421053)),
      Test "alphaBeta depth 2: equal two layers of minimax and alpha-beta pruning" (assertApproxEqual (alphabeta (initialState (8, 8)) Player1 2 (-1/0) (1/0) False) (0.0)),
      Test "alphaBeta depth 2: random position, two layers of minimax and alpha-beta pruning" (assertApproxEqual (alphabeta (GameState (8, 8) (Turn Player2) [
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Just Player1, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Just Player1, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Just Player1, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ]) Player1 2 (-1/0) (1/0) False) (5501.203007)),
      Test "alphaBeta depth 2: random position 2, two layers of minimax and alpha-beta pruning" (assertApproxEqual (alphabeta (GameState (8, 8) (Turn Player2) [
          [Just Player1, Just Player1, Just Player1, Just Player1, Nothing, Nothing, Nothing, Nothing], 
          [Just Player1, Just Player1, Just Player1, Just Player2, Nothing, Just Player1, Nothing, Nothing], 
          [Just Player1, Just Player1, Nothing, Just Player2, Just Player2, Just Player2, Nothing, Nothing], 
          [Just Player1, Just Player1, Nothing, Just Player1, Just Player2, Nothing, Nothing, Nothing], 
          [Just Player1, Nothing, Just Player1, Just Player2, Just Player1, Nothing, Nothing, Nothing], 
          [Nothing, Just Player1, Nothing, Nothing, Nothing, Just Player1, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ]) Player1 2 (-1/0) (1/0) False) (-15696.296296))
    ]
  ]
