module ElmInSpace (..) where

import Graphics.Collage as C
import Graphics.Element as E
import Keyboard
import Signal as S
import Random as R
import Color as C
import Maybe as M
import List as L
import Text as Txt
import Time as T
import Time exposing (Time)

import List.Extra as LE        -- lift2, removeWhen
import Maybe.Extra as ME       -- isJust, mapDefault
import Collision2D as C2D      -- rectangle, axisAlignedBoundingBox
import AnimationFrame as AF    -- frame
import ElmFire as EF
import ElmFire.Dict as EFD
import ElmFire.Op as EFO


{-
- CONFIG
-}

-- Resolution: Quarter of Full HD
resX = 960
resY = 540
framesPerSecond = 30
msPerFrame = 1000 / framesPerSecond
clock = AF.frame -- T.fps 30
corpseTime = 15 -- specify how long corpses exist
shotCoefficient = 4 -- 0-100, 0 = no shots, 100 = hellfire
initLives = 3
maxShots = 5
chargeGrantCoefficient = 2000 -- 1-5000, 1=always full shots, 1000=every second, 2000=every two seconds, ...
bitchMode = True -- missed shots come back at you
worldBackground = False -- display the world in the background


{-
- ELM FIRE
-}

fireBaseURL = "https://elm-in-space.firebaseio.com/"


{-
- PORT
-}

port jsRNG : Int


{-
- DATA
-}

type alias World = -- game world
    { mode : GameMode
      -- player specific:
    , playerX : Int
    , lives : Int
    , charge : Int -- number of shots available
      -- battle specific:
    , enemies : List Enemy
    , foeDir : Direction -- direction the enemies are headed
    , shotsP : List Shot
    , shotsE : List Shot
      -- necessary evil (utility):
    , seed : R.Seed -- meh
    , delta : Time
      -- statistics:
    , gameTime : Time
    , shotsFired : Int
    }

isAlive : Enemy -> Bool
isAlive e = not <| ME.isJust e.dead

animcnt : World -> Int
animcnt w = floor (w.gameTime)


type GameMode
    = PreIngame
    | Ingame
    | Victory
    | Defeat


type Direction
    = DirL
    | DirR
    | DirD Int Direction -- heading down for Int steps and then turning to Direction


type alias Enemy =
    { x : Float -- x y cooridinates spcify the center of the sprite
    , y : Float
    , kind : Int -- 1,2,3 -- specifies sprite image
    , dead : Maybe Int -- Nothing -> alive, (>0) -> remaing corpse time, (<=0) -> gargabe collect
    }


type alias Shot = -- x y cooridinates spcify the center of the sprite
    { x : Float
    , y : Float
    }


type Action
    = LeftAction
    | RightAction
    | ShootAction
    | NothingAction


{-
- INIT
-}

initEnemies : List Enemy
initEnemies =
    let
        xlist = L.map ((*) 15) [0..9]
        ylist = L.map ((*) 15) [0..5]
        ppx x = roundF <| 40 + 5 * toFloat x -- position formulas
        ppy y = roundF <| 40 + 2.5 * toFloat y
        kk y = y // 30 + 1 -- kind formula
    in
        LE.lift2 (\x' y' -> { x = ppx x', y = ppy y', kind = kk y', dead = Nothing }) xlist ylist

initial : World
initial = { mode = PreIngame
          , playerX = 50
          , lives   = initLives
          , charge  = maxShots
          , enemies = initEnemies
          , foeDir = DirR
          , shotsP = []
          , shotsE = []
          , seed = R.initialSeed jsRNG -- we get an initialSeed from javascript at the start
          , delta = 0 -- delta to the last frame
          , gameTime = 0 -- mesaure how long the game already lasted
          , shotsFired = 0 -- count how many shots were fired by the player
          }


{-
- UPDATE
-}

update : (Maybe Time, Action) -> World -> World
update (time, act) world =
    case world.mode of
            PreIngame ->
                processInputPreIngame act world
            Ingame ->
                time2world time world
                |> processInputIngame act
                |> moveShots
                |> filterDeadShots
                |> moveEnemies
                |> letEnemiesShoot
                |> shotEnemyCollision
                |> shotPlayerCollision
                |> handleCorpses
                |> grantCharge
                |> changeMode
            Victory ->
                time2world time world
                |> moveShots
                |> filterDeadShots
            Defeat ->
                time2world time world
                |> moveShots
                |> filterDeadShots


time2world : Maybe Time -> World -> World
time2world timeM ({ mode, gameTime, delta } as world) =
    case timeM of
        Nothing ->
            world
        -- time2world is still called when the game is in Victory/Defeat state
        -- so that remaining shots can still move.
        -- So we have to ask if we are still ingame and only increase the
        -- elapsed gameTime if that is the case.
        Just time ->
            { world | gameTime = if mode == Ingame
                                 then gameTime + time
                                 else gameTime
                    , delta = time }


{-
- PRE INGAME
-}

processInputPreIngame : Action -> World -> World
processInputPreIngame act world =
    case act of
        ShootAction ->
            { world | mode = Ingame }
        _ ->
            world


{-
- INGAME
-}

processInputIngame : Action -> World -> World
processInputIngame act ({ playerX, charge, shotsP, shotsFired } as world) =
    case act of
        LeftAction ->
            { world | playerX = max 0 (playerX - 1) }
        RightAction ->
            { world | playerX = min 105 (playerX + 1) }
        ShootAction ->
            if world.charge > 0 then
                { world | charge = charge - 1
                        , shotsP = (newplayershot world :: shotsP)
                        , shotsFired = shotsFired+1 }
            else
                world
        NothingAction ->
            world

newplayershot : World -> Shot
newplayershot { playerX } =
    let
        ppos = playerpos playerX
    in
        { x = fst ppos, y = snd ppos - 15 }


moveShots : World -> World
moveShots ({ shotsP, shotsE } as world) =
  let
      normalize z = z * world.delta / msPerFrame
      yDiff = normalize 6
  in
      {world | shotsP = L.map (\s -> {s | y = s.y - yDiff}) shotsP
             , shotsE = L.map (\s -> {s | y = s.y + yDiff}) shotsE }


filterDeadShots : World -> World
filterDeadShots ({ shotsP, shotsE } as world) =
    {world | shotsP = LE.removeWhen (\s -> s.y < -20) shotsP
           , shotsE = (LE.removeWhen (\s -> s.y > resY) shotsE)
                      ++ if bitchMode then L.filter (\s -> s.y < -20) shotsP else []}


moveEnemies : World -> World
moveEnemies ({ enemies, foeDir } as world) =
    let
        moveCoefficient = world.delta / msPerFrame
        normalize z = z * moveCoefficient
        enemiesX = L.map .x enemies
        xmin = L.minimum enemiesX
        xmax = L.maximum enemiesX
        ymax = L.maximum (L.map .y enemies)
        moveSingleEnemy e x' y' = {e | x=e.x+(normalize x'),  y=e.y+(normalize y')}
        moveAllLeft es = L.map (\e -> moveSingleEnemy e -2 0) es
        moveAllRight es = L.map (\e -> moveSingleEnemy e 2 0) es
        moveAllDown es = L.map (\e -> moveSingleEnemy e 0 1) es
        atBottom y = y >= 380
        atLeftEdge x = x <= 30
        atRightEdge x = x >= resX-100
        onlyLR = ME.mapDefault False atBottom ymax
    in
        case foeDir of
            DirL     -> {world | enemies = moveAllLeft enemies
                               , foeDir = ME.mapDefault foeDir (\minX -> if atLeftEdge minX then (if onlyLR then DirR else DirD 12 DirR) else foeDir) xmin}
            DirR     -> {world | enemies = moveAllRight enemies
                               , foeDir = ME.mapDefault foeDir (\maxX -> if atRightEdge maxX then (if onlyLR then DirL else DirD 12 DirL) else foeDir) xmax}
            DirD i d -> if onlyLR || i<=0
                        then {world | foeDir = d}
                        else {world | enemies = moveAllDown enemies
                                    , foeDir = DirD (i-1) d}


letEnemiesShoot : World -> World
letEnemiesShoot ({ enemies, seed } as world) =
    let
        enemyGen = shootNow `R.andThen` (\sn -> if sn then randomEnemy enemies else randomEnemy [])
        (maybeRandFoe, s) = R.generate enemyGen seed
        seedWorld = {world | seed = s}
    in
        ME.mapDefault seedWorld (\randFoe -> spawnShot seedWorld randFoe) maybeRandFoe

shootNow : R.Generator Bool
shootNow = R.map (\i -> i <= shotCoefficient) (R.int 1 100)

randomEnemy : List Enemy -> R.Generator (Maybe Enemy)
randomEnemy enemies =
    let
        idx = R.int 0 (L.length enemies - 1)
    in
        R.map (\i -> (enemies `LE.getAt` i)) idx


spawnShot : World -> Enemy -> World
spawnShot world { x, y } = {world | shotsE = {x=x, y=y} :: world.shotsE}


{-
- COLLISION
-}

playerrect world = C2D.rectangle (fst (playerpos world.playerX)) (snd (playerpos world.playerX)) 52 32
enemyrect e = C2D.rectangle e.x e.y (toFloat (enemywidth e.kind)) 24
shotrect s = C2D.rectangle s.x s.y 6 12


shotEnemyCollision : World -> World -- collide player shots with enemies
shotEnemyCollision ({charge, enemies, shotsP} as world) =
    let
        aliveEnemies = L.filter isAlive enemies
        enemyRects = L.map enemyrect aliveEnemies
        playerShotsRects = L.map shotrect shotsP
        hitByShot e = L.any (\sr -> C2D.axisAlignedBoundingBox (enemyrect e) sr) playerShotsRects
        hitAnEnemy s = L.any (\er -> C2D.axisAlignedBoundingBox (shotrect s) er) enemyRects
        hitEnemies = L.filter hitByShot aliveEnemies
    in
        {world | charge = min maxShots (charge + L.length hitEnemies)
               , enemies = LE.removeWhen hitByShot enemies
                           ++ L.map (\e -> {e | dead = Just corpseTime}) hitEnemies
               , shotsP = LE.removeWhen hitAnEnemy shotsP }

shotPlayerCollision : World -> World -- collide enemy shots with the player
shotPlayerCollision ({lives, enemies, shotsE} as world) =
    let
        enemyShotsRects = L.map shotrect shotsE
        playerHit = L.any (\sr -> C2D.axisAlignedBoundingBox (playerrect world) sr) enemyShotsRects
        hitThePlayer s = C2D.axisAlignedBoundingBox (playerrect world) (shotrect s)
    in
        {world | lives = if playerHit then lives - 1 else lives
               , shotsE = LE.removeWhen hitThePlayer shotsE}


{-
- ORGANIZATION
-}

handleCorpses : World -> World
handleCorpses ({ enemies } as world) =
    let
        decremented = L.map (\e -> {e | dead = M.map (\x->x-1) e.dead}) enemies -- decrement .dead for corpses
        stillThere = L.filter (\e -> ME.mapDefault True (\x -> x > 0) e.dead) decremented
    in
        {world | enemies = stillThere}

grantCharge : World -> World
grantCharge ({ charge } as w) =
  if toFloat ((animcnt w) % chargeGrantCoefficient) <= w.delta
  then {w | charge = min maxShots (charge+1)}
  else w

changeMode : World -> World
changeMode world =
    if world.enemies |> L.isEmpty
    then
        { world | mode = Victory }
    else
        if world.lives <= 0
        then
            { world | mode = Defeat }
        else
            world

calculateScore : World -> Int
calculateScore ({ lives, gameTime, shotsFired } as world) =
    let
        -- grant 100 points per remaining live
        livesScore = 100 * toFloat lives
        -- Clearing the screen in 60 seconds is a pretty good time,
        -- ~90 sec can be considered average, over 120 sec is bad.
        -- Being faster than 60 seconds give more than 100 points.
        -- Score function which grants 100 points after 60 seconds,
        -- at 90 seconds it gives 50 points,
        -- and after 120 seconds it starts giving negative points.
        gameTimeScore = 100 - (5/3) * ((gameTime/1000)-60)
        -- There are 60 enemies on the screen, so winning with 60 fires shots is perfect,
        -- using around 80 shots can be considered normal, over 100 is bad.
        -- Score function which grants 100 points for 60 used shots,
        -- at 80 shots it gives 50 points,
        -- and over 100 shots it starts giving negative points.
        shotsFiredScore = 100 - (5/2) * (toFloat shotsFired - 60)
        -- If one would clear the screen in 60 seconds using only the minimum of 60 shots
        -- while preserving all lives, this would result in a score of 500.
        -- This is considered VERY hard to beat.
        totalScore = livesScore + gameTimeScore + shotsFiredScore
    in
        -- But we multiply everything by 2 for dramatic effect
        -- (and because if feels more awesome to reach 1000 points instead of 500)
        round <| 2 * totalScore

{-
- VIEW
-}

view : World -> E.Element
view world =
    let
        redCenterString pos str = zero pos <| C.toForm <| E.centered <| Txt.height 40 <| Txt.color C.red <| Txt.fromString <| str
    in
        C.collage resX resY <| [ C.filled C.black (C.rect resX resY)
                                --    , starsky...?
                               ]
                            ++ [C.toForm << E.size (resX-50) (resY-100) << E.color (C.greyscale 0.8) <| E.show world]
                               ++ [player world.playerX]
                               ++ (List.map (enemy world) world.enemies)
                               ++ (List.map shotP world.shotsP)
                               ++ (List.map shotE world.shotsE)
                               ++ header world
                               ++ case world.mode of
                                      PreIngame ->
                                           [redCenterString (450, 300) "Press SPACE to start!"]
                                      Ingame ->
                                           []
                                      Victory ->
                                           [redCenterString (450, 300) "You did it! Press F5 for another round."]
                                      Defeat ->
                                           [redCenterString (450, 80) "Try harder next time!"]


header : World -> List C.Form
header w = [ zero (900, 10) <| C.toForm <| E.image 32 32 "img/heart.png"
           , zero (875, 10) <| C.toForm <| E.centered <| Txt.height 18 <| Txt.color C.red <| Txt.fromString <| toString w.lives
           , zero (800, 10) <| C.toForm <| E.image 32 32 "img/lightning.png"
           , zero (775, 10) <| C.toForm <| E.centered <| Txt.height 18 <| Txt.color C.blue <| Txt.fromString <| toString w.charge
           ]

-- Sprites
player : Int -> C.Form
player pX = zero (playerpos pX) (C.toForm (E.image 52 32 "img/player.png"))

playerpos : Int -> (Float, Float)
playerpos pX = (32+toFloat pX*8, 460)

-- http://www.wolframalpha.com/input/?i=InterpolatingPolynomial%5B%7B%7B1%2C+24%7D%2C+%7B2%2C+32%7D%2C+%7B3%2C+36%7D%7D%2C+x%5D
enemywidth x = 24 + (8 - 2 * (x-2)) * (x-1) -- enemy_kind -> int

enemy : World -> Enemy -> C.Form
enemy w e =
    let
        ab w = if animcnt w % 1200 <= 600 then "a" else "b"
        livingenemy w e = E.image (enemywidth e.kind) 24 ("img/enemy"++toString e.kind++ab w++"3.png")
        deadenemy = E.image 36 24 "img/dead3.png"
    in
        zero (e.x,e.y) (C.toForm <| if isAlive e then livingenemy w e else deadenemy)

shotP : Shot -> C.Form
shotP s = zero (s.x,s.y) (C.toForm (E.image 6 12 "img/playershot.png"))

shotE : Shot -> C.Form
shotE s = zero (s.x,s.y) (C.toForm (E.image 6 12 "img/enemyshot.png"))

-- makes top left corner the (0,0) origin
zero : (Float, Float) -> C.Form -> C.Form
zero (x,y) f = C.move (x,-y) (C.move (-450,250) f)


{-
- INPUT
-}

input : Signal (Maybe Time, Action)
input = S.merge leftNright space

leftNright : Signal (Maybe Time, Action)
leftNright =
    Signal.sampleOn
        clock
        (Signal.map2 (,)
            (S.map Just clock)
            (S.map
              (\ v -> if v == {x=-1, y=0} then LeftAction
                      else if v == {x=1, y=0} then RightAction
                      else NothingAction)
              Keyboard.arrows)
        )

space : Signal (Maybe Time, Action)
space = S.map2 (,)
            (S.map (\_->Nothing) clock)
            (S.map (always ShootAction) (S.filter identity False Keyboard.space))


{-
- MAIN
-}

main : Signal E.Element
main = S.map view (S.foldp update initial input)


{-
- UTIL - I hope i can get rid of these once they are added to the Community Libraries
-}

-- Sent a pull request to dasch/elm-basics-extra, no response from maintainer yet
{-| Round a float
-}
roundF = toFloat << round
