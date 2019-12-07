module Main where
    
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Globals
import Engine
import Input

main :: IO ()
main =
        play gameWindow windowBackground fps initialState render keyEvent update
