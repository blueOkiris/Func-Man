module Engine(render, update) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

--import Debug.Trace

import Globals
import Physics

-- Draw an image from a list (use with map) --
translateListElement :: ((Float, Float), Picture) -> Picture
translateListElement ((x, y), pic) =
    translate x y pic

-- Create list of the same images --
repeatPictureList :: Picture -> Int -> [Picture]
repeatPictureList pic n =
    if n == 1 then
        [pic]
    else
        pic : repeatPictureList pic (n - 1)

-- Draw every block in blockPos --
drawGameBlocks :: Game -> Picture
drawGameBlocks game =
    Pictures picList
    where
        imageList = repeatPictureList (sprBlock game) (length (blockPos game))
        totalTupleList = zip (blockPos game) imageList
        picList = map translateListElement totalTupleList

-- Draw the game --
render :: Game -> Picture
render game =
    Pictures    [ translate x y
                    (sprPlayer game)
                ,  drawGameBlocks game ]
                --, translate (x + dir) y (color red (rectangleSolid 64 32)) ]
                --, translate (fst (playerPos game)) ((snd (playerPos game)) - 64)
                --    (color red
                --        (rectangleSolid 64 64)) ]
                --, translate (fst (playerPos game)) (snd (playerPos game))
                --    (color red
                --        (circleSolid 10)) ]
    where
        (x, y) = playerPos game
        --(vx, vy) = playerVel game
        --dir = 64 * (vx / (abs vx))
        --collisions = collisionListHor (x + dir, y) (blockPos game)

-- Check if a player has left the screen. If so then reset --
checkFellOut :: Game -> Game
checkFellOut game =
    if snd (playerPos game) < (-(fromIntegral windowHeight) / 2) - 64 then
        initialState
    else
        game

-- Update position etc. --
update :: Float -> Game -> Game
update seconds game =
    checkFellOut
        (movePlayer seconds    
            (verticalCollision (horizontalCollision (fallWithGravity seconds 
                game))))
