module Globals(Game(..), initialState, fps, gameWindow, windowBackground, windowWidth, windowHeight) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import System.IO.Unsafe

data Game =
    Game    { playerPos     :: (Float, Float)
            , playerVel     :: (Float, Float)
            , playerMaxSpd  :: Float
            , playerMvSpd   :: Float
            , playerJmpSpd  :: Float
            , gravity       :: Float
            , grounded      :: Bool
            , blockPos      :: [(Float, Float)]
            , sprPlayer     :: Picture
            , sprBlock      :: Picture
            } deriving (Show)

fps :: Int
fps = 60

windowWidth :: Int
windowWidth = 1280

windowHeight :: Int
windowHeight = 720

gameWindow :: Display
gameWindow = (InWindow "Game" (windowWidth, windowHeight) (500, 500))

windowBackground :: Color
windowBackground = black

playerTexSrc    :: String
blockTexSrc     :: String
playerTexSrc =  "images/temp_plyr.png"
blockTexSrc =   "images/block.png"

png :: FilePath -> Picture
png fname = maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)

initialState :: Game
initialState =
    Game    { playerPos =       (0, -100)
            , playerVel =       (0, 0)
            , playerMaxSpd =    1024
            , playerMvSpd =     512
            , playerJmpSpd =    768
            , gravity =         32
            , grounded =        False
            , blockPos =        (64, lowerBlockHeight + 256) :
                                (-256, lowerBlockHeight + 64) : (-192, lowerBlockHeight + 64) :
                                (-192, lowerBlockHeight + 128) :
                                    zip lowerBlockXList lowerBlockYList
            , sprPlayer =       scale 0.5 0.5 (png playerTexSrc)
            , sprBlock =        scale 0.5 0.5 (png blockTexSrc) }
    where
        lowerBlockGroundCount = ((fromIntegral windowWidth) / 2) / 64
        lowerBlockHeight = -(720 / 2) + 32
        lowerBlockXList = map (* 64) [-lowerBlockGroundCount .. lowerBlockGroundCount]
        lowerBlockYList = map (+ lowerBlockHeight) (map (* 0) [-lowerBlockGroundCount .. lowerBlockGroundCount])
