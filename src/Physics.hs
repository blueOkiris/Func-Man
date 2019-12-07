module Physics(fallWithGravity, movePlayer, verticalCollision, horizontalCollision) where

import Data.List

import Globals

-- Cause the player to accelerate down --
fallWithGravity :: Float -> Game -> Game
fallWithGravity seconds game =
    if snd (playerVel game) > -(playerMaxSpd game) then
        game { playerVel = (vx, vyp) }
    else
        game
    where
        (vx, vy) = playerVel game
        vyp = vy - (gravity game)

-- Move the player based on velocity --
movePlayer :: Float -> Game -> Game
movePlayer seconds game =
    game { playerPos = (xp, yp) }
    where
        (x, y) = playerPos game
        (vx, vy) = playerVel game
        xp = x + vx * seconds
        yp = y + vy * seconds
        --yp = y + (trace (show vy) vy) * seconds

-- Check for collisions --
comparePointVert :: (Float, Float) -> (Float, Float) -> Bool
comparePointVert p1 p2 =
    if (l1x > r2x) || (l2x > r1x) then
        False
    else if (l1y < r2y) || (l2y < r1y) then
        False
    else
        True
    where
        -- Center of collsion rects --
        (c1x, c1y) = p1
        (c2x, c2y) = p2

        -- Top left of collision rects
        l1x = c1x - 16
        l1y = c1y + 32

        l2x = c2x - 16
        l2y = c2y + 32

        -- Bottom right of collision rects --
        r1x = c1x + 16
        r1y = c1y - 32
        
        r2x = c2x + 16
        r2y = c2y - 32

comparePointHor :: (Float, Float) -> (Float, Float) -> Bool
comparePointHor p1 p2 =
    if (l1x > r2x) || (l2x > r1x) then
        False
    else if (l1y < r2y) || (l2y < r1y) then
        False
    else
        True
    where
        -- Center of collsion rects --
        (c1x, c1y) = p1
        (c2x, c2y) = p2

        -- Top left of collision rects
        l1x = c1x - 32
        l1y = c1y + 16

        l2x = c2x - 32
        l2y = c2y + 16

        -- Bottom right of collision rects --
        r1x = c1x + 32
        r1y = c1y - 16
        
        r2x = c2x + 32
        r2y = c2y - 16

collisionListVert :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
collisionListVert (x, y) pointList =
    --trace ((show (x, y)) ++ " " ++ (show filteredList)) filteredList
    filteredList
    where
        filteredList = filter (comparePointVert (x, y)) pointList
collisionListHor :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
collisionListHor (x, y) pointList =
    --trace ((show (x, y)) ++ " " ++ (show filteredList)) filteredList
    filteredList
    where
        filteredList = filter (comparePointHor (x, y)) pointList

-- Vertical collisions --
verticalCollision :: Game -> Game
verticalCollision game =
    if length collisions > 0 && -- Colliding with something --
        (True `elem` (map (>= ((snd (playerPos game)) + dir)) (map snd collisions)) 
        && True `elem` (map (<= (snd (playerPos game))) (map snd collisions))) then -- Collsion is y below --
            game    { playerVel =   (vx, 0)
                    , grounded =    True }
    else if length collisions > 0 && -- Colliding with something --
        (True `elem` (map (<= ((snd (playerPos game)) + dir)) (map snd collisions)) 
        && True `elem` (map (>= (snd (playerPos game))) (map snd collisions))) then -- Collsion is y below --
            game    { playerVel =   (vx, 0)
                    , grounded =    True }
    else
        game
    where
        (x, y) = playerPos game
        (vx, vy) = playerVel game
        dir = 64 * (vy / (abs vy))
        collisions = collisionListVert (x, y + dir) (blockPos game)

-- Horizontal collisions --
horizontalCollision :: Game -> Game
horizontalCollision game =
    if length collisions > 0 && -- Colliding with something --
        (True `elem` (map (>= ((fst (playerPos game)) + dir)) (map fst collisions))
        && True `elem` (map (<= (fst (playerPos game))) (map fst collisions))) then -- Collsion is y below --
            game    { playerVel =   (0, vy) }
    else if length collisions > 0 && -- Colliding with something --
        (True `elem` (map (<= ((fst (playerPos game)) + dir)) (map fst collisions))
        && True `elem` (map (>= (fst (playerPos game))) (map fst collisions))) then -- Collsion is y below --
            game    { playerVel =   (0, vy) }
    else
        game
    where
        (x, y) = playerPos game
        (vx, vy) = playerVel game
        dir = 64 * (vx / (abs vx))
        collisions = collisionListHor (x + dir, y) (blockPos game)
        --collisions = trace (show coll) coll