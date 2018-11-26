  module LSystems where

import IC.Graphics
import Data.Fixed

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.
angle :: System -> Float
angle (degrees, _baseString, _ruleList)
  = degrees

base :: System -> String
base (_degrees, baseString, _ruleList)
  = baseString

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (_degrees, baseString, ruleList)
  = ruleList


-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar element ruleList
  = concat([ y | (x, y) <- ruleList, element == x ]) -- could use higher order functions?

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne ruleList string
  = concat([ lookupChar x ruleList | x <- string])

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand ruleList string num
    = iterate (expandOne ruleList) string !! num
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.

toRadians :: Float -> Float
toRadians angle
  = angle * (pi / 180)

move :: Char -> TurtleState -> Float -> TurtleState
move cmd state@( start@(x1, y1), sAngle) deltAngle
  | cmd == 'F' =  (( x1 + unit * cos radians, y1 + unit * sin radians), sAngle)
  | cmd == 'L' = changeAngle deltAngle
  | cmd == 'R' = changeAngle (-deltAngle)
  | otherwise  = state
    where
      radians = toRadians sAngle
      unit = 1

      changeAngle dAngle
        = (start, mod' (sAngle + dAngle) 360)

trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 [] _turnAngle _colour = []
trace1 cmdList turnAngle colour
  = cList

  where
    (remCmds, cList) = trace' cmdList startState
    startState       = ((0, 0), 90)

    trace' :: String -> TurtleState -> (String, [ColouredLine])
    trace' [] _ = ([], [])
    trace' (cmd : cmds) curState@(vertex, bearing)
      | cmd == ']' = (cmds, [])
      | cmd == '[' = (remCmds''', cList'' ++ cList''')
      | cmd == 'F' = (remCmds', (vertex, vertex', colour) : cList')
      | otherwise  = trace' cmds finalState

      where
        finalState@(vertex', _newAngle) = move cmd curState turnAngle
        (remCmds', cList')              = trace' cmds finalState
        (remCmds'', cList'')            = trace' cmds curState
        (remCmds''', cList''')          = trace' remCmds'' curState


-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 2
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 [] _sBearing _colour = []
trace2 cmdList turnAngle colour
  = trace' cmdList startState

  where
    startState = [((0, 0), 90)]

    trace' :: String -> Stack -> [ColouredLine]
    trace' [] _stack = []
    trace' (cmd : cmds) stack@(top : remStack)
      | cmd == '[' =  trace' cmds (top : stack)
      | cmd == ']' = trace' cmds remStack
      | cmd == 'F' = (vertex, vertex', colour) : (trace' cmds (finalState : remStack))
      | otherwise = trace' cmds (finalState : remStack)
        where
          finalState@(vertex', _newAngle) = move cmd curState turnAngle
          curState@(vertex, bearing) = top


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (lSystem system n) (angle system) colour)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (lSystem system n) (angle system) colour)
