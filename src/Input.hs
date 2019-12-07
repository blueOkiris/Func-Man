module Input(keyEvent) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Globals

-- Keyboard events --
keyEvent :: Event -> Game -> Game
keyEvent (EventKey (Char 'w') Down _ _) game =
    if grounded game then
        game    { playerVel =   (vx, vyp)
                , grounded =    False}
    else
        game
    where
        (vx, vy) = playerVel game
        vyp = playerJmpSpd game
keyEvent (EventKey (SpecialKey KeyUp) Down _ _) game =
    if grounded game then
        game    { playerVel =   (vx, vyp)
                , grounded =    False }
    else
        game
    where
        (vx, vy) = playerVel game
        vyp = playerJmpSpd game
keyEvent (EventKey (Char 'a') Down _ _) game =
    game    { playerVel = (vxp, vy) }
    where
        (vx, vy) = playerVel game
        vxp = -(playerMvSpd game)
keyEvent (EventKey (SpecialKey KeyLeft) Down _ _) game =
    game    { playerVel = (vxp, vy) }
    where
        (vx, vy) = playerVel game
        vxp = -(playerMvSpd game)
keyEvent (EventKey (Char 'd') Down _ _) game =
    game    { playerVel = (vxp, vy) }
    where
        (vx, vy) = playerVel game
        vxp = playerMvSpd game
keyEvent (EventKey (SpecialKey KeyRight) Down _ _) game =
    game    { playerVel = (vxp, vy) }
    where
        (vx, vy) = playerVel game
        vxp = playerMvSpd game
keyEvent (EventKey (Char 'a') Up _ _) game =
    game    { playerVel = (vxp, vy) }
    where
        (vx, vy) = playerVel game
        vxp = 0
keyEvent (EventKey (SpecialKey KeyLeft) Up _ _) game =
    game    { playerVel = (vxp, vy) }
    where
        (vx, vy) = playerVel game
        vxp = 0
keyEvent (EventKey (Char 'd') Up _ _) game =
    game    { playerVel = (vxp, vy) }
    where
        (vx, vy) = playerVel game
        vxp = 0
keyEvent (EventKey (SpecialKey KeyRight) Up _ _) game =
    game    { playerVel = (vxp, vy) }
    where
        (vx, vy) = playerVel game
        vxp = 0
keyEvent _ game =
    game
    