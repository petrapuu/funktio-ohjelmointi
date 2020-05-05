{-# LANGUAGE NamedFieldPuns #-}
-- Asteroids game exercise
module Main where
    import qualified Graphics.Gloss as Gloss
    import qualified Graphics.Gloss.Interface.Pure.Game as G
    import qualified Graphics.Gloss.Interface.Pure.Simulate as S
    import qualified Graphics.Gloss.Interface.Pure.Display as D
    
    data AsteroidWorld = Play [Rock] Ship [Bullet] Ufo
                       | GameOver 
                       deriving (Eq,Show)
    
    type Velocity     = (Float, Float)
    type Size         = Float
    type Age          = Float
    
data Ship   = Ship   PointInSpace Velocity      
    deriving (Eq,Show)
data Bullet = Bullet PointInSpace Velocity Age  
    deriving (Eq,Show)
data Rock   = Rock   PointInSpace Size Velocity 
    deriving (Eq,Show)
-- created datatype for ufo
    data Ufo
        = Hunting PointInSpace UfoData
        | Fleeing PointInSpace UfoData Time
        -- "one additional feature of your choice" - the ufo spins for a moment when hit
        -- I know the 'spd' field of UfoData is not actually necessary here,
        -- pls don't disqualify for a bit of laziness
        -- (maxSpd and hp are necessary because we need to remember them in the next state)
        | Spinning PointInSpace UfoData Angle
        -- max speed increases after each explosion
        | Exploding PointInSpace PrevMaxSpd Time
        deriving (Eq,Show)

	type Angle = Float
    type Time = Float
    type PrevMaxSpd = Float

    data UfoData = UfoData
        { spd :: Float -- UFO always moves towards or away from the player so we don't need to store direction
        , maxSpd :: Float -- increases every time the ufo is killed
        , hp :: Int
        }
        deriving (Eq,Show)
    

    -- E2: various UFO constants
    ufoMaxHealth :: Int
    ufoMaxHealth =
        3
    
    ufoAcceleration :: Float
    ufoAcceleration =
        0.05
    
    ufoSize :: Float
    ufoSize =
        15.0
    
    ufoFleeTime :: Float
    ufoFleeTime =
        2.0
    
    ufoFleeSpeed :: Float
    ufoFleeSpeed =
        2.0
    
    initialWorld :: AsteroidWorld
    initialWorld = 
        let
             -- E2: the initial ufo
            initialUfoData =
                UfoData
                    { spd = 0
                    , maxSpd = 1
                    , hp = ufoMaxHealth
                    }
        in
        Play
            [Rock (150,150)  45 (2,6)    
            ,Rock (-45,201)  45 (13,-8) 
            ,Rock (45,22)    25 (-2,8)  
            ,Rock (-210,-15) 30 (-2,-8) 
            ,Rock (-45,-201) 25 (8,2)   
            ] -- The default rocks
            (Ship (0,0) (0,5)) -- The initial ship
            [] -- The initial bullets (none)
            (Hunting (100, 300) initialUfoData)
    
    
    simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)
    
    simulateWorld _ GameOver = GameOver
    
    -- E3: remove all the specialized collision functions and use checkCollision for everything
simulateWorld timeStep (Play rocks (Ship shipPos shipV) bullets (Ufo ufoPos@(ufoPosX,ufoPosY) ufoV ufoTime ufoSize))
  | any (collidesWith shipPos) rocks = GameOver
  | collidesWith shipPos (Rock ufoPos ufoSize ufoV) = GameOver
  | otherwise = Play (concatMap updateRock rocks) 
                              (Ship newShipPos shipV)
                              (concat (map updateBullet bullets))
                              newUfo
        where
      collidesWith :: PointInSpace -> Rock -> Bool
      collidesWith p (Rock rp s _) 
       = magV (rp .- p) < s 

      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r 
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets 
     
      updateRock :: Rock -> [Rock]
      updateRock r@(Rock p s v) 
       | collidesWithBullet r && s < 7 
            = []
       | collidesWithBullet r && s > 7 
            = splitRock r
       | otherwise                     
            = [Rock (restoreToScreen (p .+ timeStep .* v)) s v]
 
      updateBullet :: Bullet -> [Bullet] 
      updateBullet (Bullet p v a) 
        | a > 5                      
             = []
        | any (collidesWith p) rocks 
             = [] 
        | otherwise                  
             = [Bullet (restoreToScreen (p .+ timeStep .* v)) v 
                       (a + timeStep)] 
        
            -- EDIT: generalized velocity integration function
            -- so we can also move the ufo with it
            move :: PointInSpace -> Velocity -> PointInSpace
            move pos vel =
                restoreToScreen (pos .+ timeStep .* vel)
        
            newShipPos :: PointInSpace
            newShipPos = move shipPos shipV

            -- E2: bigger ufo update
            newUfo :: Ufo
            newUfo =
                let
                    accelerate spd maxSpd
                        | spd < maxSpd =
                            -- E3: colliding with rocks slows the ufo down
                            if any (checkCollision ufo) rocks then
                                spd / 2
                            else
                                spd + (ufoAcceleration * maxSpd)
                        | otherwise = spd
                    
                    atPlayer ufoPos =
                        norm $ shipPos .- ufoPos

                    onHit :: PointInSpace -> UfoData -> Ufo
                    -- data is a reserved keyword so we call this uData instead.
                    -- this puzzled me for a little while :D
                    onHit pos uData@UfoData{spd, maxSpd, hp} =
                        if hp <= 1 then
                            Exploding pos maxSpd 0
                        else
                            Spinning pos (uData { spd = ufoFleeSpeed, hp = hp-1 }) 0
                in
                case ufo of
                    Hunting pos uData@UfoData{spd, maxSpd, hp} ->
                        if collidesWithBullet ufo then
                            onHit pos uData
                        else
                            Hunting (pos .+ (spd .* atPlayer pos)) (uData { spd = accelerate spd maxSpd })
                    
                    Fleeing pos uData@UfoData{spd, maxSpd, hp} time ->
                        if collidesWithBullet ufo then
                            onHit pos uData
                        else if time >= ufoFleeTime then
                            Hunting pos $ uData { spd = 0 }
                        else
                            Fleeing (pos .- (spd .* atPlayer pos)) uData (time + timeStep)
                    
                    Spinning pos uData angle ->
                        if angle >= 720 then
                            Fleeing pos uData 0
                        else
                            Spinning pos uData $ angle + 30
                    
                    Exploding pos prevMaxSpd time ->
                        if time >= 3 then
                            Hunting pos $ UfoData { spd = 0, maxSpd = prevMaxSpd + 2, hp = ufoMaxHealth }
                        else
                            Exploding pos prevMaxSpd (time + timeStep)
            
    
    splitRock :: Rock -> [Rock]
    splitRock (Rock p s v) = [ Rock p (s/2) (3 .* rotateV (pi/3)  v)
                             , Rock p (s/2) (3 .* rotateV (-pi/3) v) ]
    
    restoreToScreen :: PointInSpace -> PointInSpace
    restoreToScreen (x,y) = (cycleCoordinates x, cycleCoordinates y)
    
    cycleCoordinates :: (Ord a, Num a) => a -> a
    cycleCoordinates x 
        | x < (-400) = 800+x
        | x > 400    = x-800
        | otherwise  = x
    
    drawWorld :: AsteroidWorld -> D.Picture
    
    drawWorld GameOver 
       = D.scale 0.2 0.2
         . D.translate (-950) 0 
         . D.color D.red 
         . D.text 
         $ "Game Over! LMB to try again" -- EDIT: cosmetic
    
    drawWorld (Play rocks (Ship (x,y) (vx,vy)) bullets ufo) -- E2: ufo
      = D.pictures [ship, asteroids, shots, ufoGraphic]
       where 
        ship      = D.color D.red (D.pictures [D.translate x y (D.circle 10)])
        asteroids = D.pictures [D.translate x y (D.color D.orange (D.polygon (rockGraphic s))) 
                             | Rock   (x,y) s _ <- rocks]
        shots     = D.pictures [D.translate x y (D.color D.red (D.circle 2)) 
                             | Bullet (x,y) _ _ <- bullets]
                             
        -- EDIT: single polygon shape for all asteroids,
        -- not even rotated because I'm lazy and that wasn't a requirement
        rockGraphic size =  
            [ (size .* v)
                | v <- zipWith (.*)
                    [1.0, 1.2, 1.1, 0.9, 1.0, 0.8, 1.1, 1.0] -- modifiers to move the points towards or away from the origin
                    [(cos a, sin a) | a <- map ((*) (pi / 4)) [0..7]] -- 8 points on a unit circle
            ]

        -- E2: ufo graphic
        ufoGraphic =
            D.color D.blue 
            $ case ufo of
                Hunting (x,y) _ -> D.translate x y ufoAlive
                Fleeing (x,y) _ _ -> D.translate x y ufoAlive
                Spinning (x,y) _ angle ->
                    D.translate x y $ D.rotate angle $ ufoAlive
                Exploding (x,y) _ time ->
                    D.translate x y
                    $ D.scale ((sin (20*time) + 1) * 15) ((cos (15*time) + 1) * 15)
                    $ D.rotate time
                    $ star

        ufoAlive =
            D.pictures 
                [ D.arc 20 160 ufoSize
                , D.polygon [(w,5),(w,0),(-w,0),(-w,5)]
                ]
            where
                w = ufoSize * 1.5
        
        star =
            D.pictures
                [ D.polygon [(0,1),(0.2,0),(0,-1),(-0.2,0)]
                , D.polygon [(1,0),(0,0.2),(-1,0),(0,-0.2)]
                ]
    
    handleEvents :: G.Event -> AsteroidWorld -> AsteroidWorld

    -- EDIT: left clicking in game over state starts a new game
    handleEvents (G.EventKey (G.MouseButton G.LeftButton) G.Down _ _) GameOver = initialWorld
    
    handleEvents (G.EventKey (G.MouseButton G.LeftButton) G.Down _ clickPos)
                 (Play rocks (Ship shipPos shipVel) bullets ufo) -- EDIT: ufo does nothing
                 = Play rocks (Ship shipPos newVel) 
                              (newBullet : bullets)
                              ufo
     where 
         newBullet = Bullet shipPos 
                            (negate 150 .* norm (shipPos .- clickPos)) 
                            0
         newVel    = shipVel .+ (50 .* norm (shipPos .- clickPos))
    
    handleEvents _ w = w
    
    type PointInSpace = (Float, Float)
    (.-) , (.+) :: PointInSpace -> PointInSpace -> PointInSpace
    (x,y) .- (u,v) = (x-u,y-v)
    (x,y) .+ (u,v) = (x+u,y+v)
    
    (.*) :: Float -> PointInSpace -> PointInSpace
    s .* (u,v) = (s*u,s*v)
    
    infixl 6 .- , .+
    infixl 7 .*
    
    norm :: PointInSpace -> PointInSpace
    norm (x,y) = let m = magV (x,y) in (x/m,y/m)
    
    magV :: PointInSpace -> Float
    magV (x,y) = sqrt (x**2 + y**2) 
    
    limitMag :: Float -> PointInSpace -> PointInSpace
    limitMag n pt = if (magV pt > n) 
                      then n .* (norm pt)
                      else pt
    
    rotateV :: Float -> PointInSpace -> PointInSpace
    rotateV r (x,y) = (x * cos r - y * sin r
                      ,x * sin r + y * cos r)
    
    
    main = G.play 
             (D.InWindow "Asteroids!" (550,550) (20,20)) 
             D.black 
             24 
             initialWorld 
             drawWorld 
             handleEvents
             simulateWorld