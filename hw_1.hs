{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise2


-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0   0   (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: Integer -> Picture
trafficLight 0  = botCircle green  & midCircle black   & topCircle black  & frame
trafficLight 1  = botCircle black  & midCircle yellow  & topCircle black  & frame
trafficLight 2  = botCircle black  & midCircle black   & topCircle red    & frame
trafficLight 3  = botCircle black  & midCircle yellow  & topCircle red    & frame

trafficController :: Double -> Picture
trafficController t
  | round (t) `mod` 8 < 3 = trafficLight 0
  | round (t) `mod` 8 < 4 = trafficLight 1
  | round (t) `mod` 8 < 7 = trafficLight 2
  | otherwise                = trafficLight 3

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2
path :: [Point] -> Picture
path [(0,0), (0,1)] = colored black (rectangle 0.5 1)

tree :: Integer -> Picture
tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))

bloom :: Color -> Double -> Picture
bloom c 0 = colored c (solidCircle 0.25) & translated 0 1 (
            colored c (rotated (- pi/6) (solidCircle 0.1)) 
            & colored c (rotated (pi/6) (solidCircle 0.1)))

bloom c n = colored c (solidCircle 0.25) & translated 0 1 (
  rotated (pi/10) (bloom c (n-1)) & rotated (- pi/10) (bloom c (n-1)))

bloomController :: Double -> Picture
bloomController t
  | round (t)  <= 1 = (bloom brown 3) & (bloom (dark green) 3) & (tree 10)
  | round (t)  == 2 = (bloom brown 3) & (bloom (dark green) 4) & (tree 10)
  | round (t)  == 3 = (bloom brown 3) & (bloom (dark green) 5) & (tree 10)
  | round (t)  == 4 = (bloom brown 3) & (bloom (dark green) 6) & (tree 10)
  | round (t)  == 5 = (bloom brown 3) & (bloom (dark green) 7) & (tree 10)
  | round (t)  == 6 = (bloom brown 3) & (bloom (dark green) 8) & (tree 10)
  | otherwise       = (bloom brown 3) & (bloom (dark green) 9) & (tree 10)

exercise2 :: IO ()
-- exercise2 = drawingOf ((bloom brown 3) & (bloom (dark green) 9) & (tree 10) )
exercise2 = animationOf bloomController

-- Exercise 3

wall, ground, storage, box :: Picture
wall =    undefined
ground =  undefined
storage = undefined
box =     undefined

drawTile :: Integer -> Picture
drawTile = undefined

         
pictureOfMaze :: Picture
pictureOfMaze = undefined

exercise3 :: IO ()
exercise3 = undefined
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 
