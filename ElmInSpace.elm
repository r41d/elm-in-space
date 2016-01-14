module Main (..) where

import Graphics.Collage as C
import Graphics.Element as E
import Signal
import Keyboard
import Time
import Color
import List as L
import Random as R
import Text
import Maybe as M

import List.Extra as LE
import Maybe.Extra as ME
import Collision2D as Coll


{-
- CONFIG
-}

-- Resolution: Quarter of Full HD
resX = 960
resY = 540
clock = Time.fps 30
corpseTime = 15 -- specify how long corpses exist
shotCoefficient = 5 --  0-100, 0 = no shots, 100 = hellfire
maxShots = 5
chargeGrantCoefficient = 60 -- 1-1000, 1=always full shots, 30=every second, 60=every two seconds, ...
bitchMode = True -- missed shots come back at you
worldBackground = False -- display the world in the background


{-
- PORT
-}

--port rngJS : Signal Int

--rngMailbox = Signal.mailbox Int


{-
- DATA
-}

type alias World = -- game world
    { mode : GameMode
    , playerX : Int
    , lives : Int
    , charge : Int -- number of shots available
    , enemies : List Enemy
    , foeDir : Direction -- direction the enemies are headed
    , counter : Int -- auxiliary counter for charge granting and enemy animation
    , shotsP : List Shot
    , shotsE : List Shot
    , seed : R.Seed -- meh
    }

isAlive : Enemy -> Bool
isAlive e = not <| isDead e

isDead : Enemy -> Bool
isDead e = ME.isJust e.dead

aliveEnemies : World -> List Enemy
aliveEnemies w = L.filter isAlive w.enemies

deadEnemies : World -> List Enemy
deadEnemies w = L.filter isDead w.enemies

animcnt : World -> Int
animcnt w = w.counter % 40


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
          , lives   = 3
          , charge  = maxShots
          , enemies = initEnemies
          , foeDir = DirR
          , counter = 0
          , shotsP = []
          , shotsE = []
          , seed = R.initialSeed 5014718317
          }


{-
- UPDATE
-}

update : Action -> World -> World
update act world =
    case world.mode of
        PreIngame ->
            processInputPreIngame act world
            |> moveShots
            >> filterDeadShots
            >> incrementCounter
        Ingame ->
            processInputIngame act world
            |> moveShots
            >> filterDeadShots
            >> moveEnemies
            >> letEnemiesShoot
            >> shotEnemyCollision
            >> shotPlayerCollision
            >> handleCorpses
            >> grantCharge
            >> incrementCounter
            >> changeMode
        Victory ->
            moveShots world
            |> filterDeadShots
        Defeat ->
            moveShots world
            |> filterDeadShots


updateGameMode : World -> World
updateGameMode world =
    case world.mode of
        PreIngame ->
            { world | mode = world.mode }
        Ingame ->
            { world | mode = world.mode }
        Victory ->
            { world | mode = world.mode }
        Defeat ->
            { world | mode = world.mode }


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
processInputIngame act ({ playerX, charge, shotsP } as world) =
    case act of
        LeftAction ->
            { world | playerX = max 0 (playerX - 1) }
        RightAction ->
            { world | playerX = min 105 (playerX + 1) }
        ShootAction ->
            if world.charge > 0 then
                { world
                    | charge = charge - 1
                    , shotsP = (newplayershot world :: shotsP)
                }
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
    {world | shotsP = L.map (\s -> {s | y=s.y-5}) shotsP
           , shotsE = L.map (\s -> {s | y=s.y+5}) shotsE }


filterDeadShots : World -> World
filterDeadShots ({ shotsP, shotsE } as world) =
    {world | shotsP = unfilter (\s -> s.y < -20) shotsP
           , shotsE = (unfilter (\s -> s.y > resY) shotsE)
                      ++ if bitchMode then L.filter (\s -> s.y < -20) shotsP else []}


moveEnemies : World -> World
moveEnemies ({ enemies, foeDir } as world) =
    let
        xmin = L.minimum (L.map .x enemies)
        xmax = L.maximum (L.map .x enemies)
        ymax = L.maximum (L.map .y enemies)
        moveSingleEnemy e x' y' = {e | x=e.x+x',  y=e.y+y'}
        moveAllLeft = L.map (\e -> moveSingleEnemy e -2 0) enemies
        moveAllRight = L.map (\e -> moveSingleEnemy e 2 0) enemies
        moveAllDown = L.map (\e -> moveSingleEnemy e 0 1) enemies
        invAtBottom y = y >= 380
        invAtLeftEdge x = x <= 30
        invAtRightEdge x = x >= resX-100
        -- somewhat of code duplication, TODO: figure out a readable(!) shorter alternative
        onlyLR = -- only move left to right
          case world.foeDir of
            DirL     -> {world | enemies = moveAllLeft
                               , foeDir = maybeCondition' xmin (\minX -> if invAtLeftEdge minX then DirR else foeDir) foeDir}
            DirR     -> {world | enemies = moveAllRight
                               , foeDir = maybeCondition' xmax (\maxX -> if invAtRightEdge maxX then DirL else foeDir) foeDir}
            DirD _ d -> {world | foeDir = d} -- stop moving down and just turn to the next direction
        nextdir = -- also move down when reaching the left/right border
          case foeDir of
            DirL     -> {world | enemies = moveAllLeft
                               , foeDir = maybeCondition' xmin (\minX -> if invAtLeftEdge minX then DirD 12 DirR else foeDir) foeDir}
            DirR     -> {world | enemies = moveAllRight
                               , foeDir = maybeCondition' xmax (\maxX -> if invAtRightEdge maxX then DirD 12 DirL else foeDir) foeDir}
            DirD 0 d -> {world | foeDir = d}
            DirD i d -> {world | enemies = moveAllDown
                               , foeDir = DirD (i-1) d}
    in
        if maybeCondition' ymax invAtBottom False
        then onlyLR
        else nextdir


letEnemiesShoot : World -> World
letEnemiesShoot ({ enemies, seed } as world) =
    let
        enemyGen = shootNow `R.andThen` (\sn -> if sn then randomEnemy enemies else randomEnemy [])
        (maybeRandFoe, s) = R.generate enemyGen seed
        seedWorld = {world | seed = s}
    in
        maybeCondition' maybeRandFoe (\randFoe -> spawnShot seedWorld randFoe) seedWorld

shootNow : R.Generator Bool
shootNow = R.map (\i -> i <= shotCoefficient) (R.int 1 100)

randomEnemy : List Enemy -> R.Generator (Maybe Enemy)
randomEnemy enemies =
    let
        idx = R.int 0 (L.length enemies - 1)
    in
        R.map (\i -> (enemies `getAt` i)) idx


spawnShot : World -> Enemy -> World
spawnShot world { x, y } = {world | shotsE = {x=x, y=y} :: world.shotsE}


{-
- COLLISION
-}

playerrect world = Coll.rectangle (fst (playerpos world.playerX)) (snd (playerpos world.playerX)) 52 32
enemyrect e = Coll.rectangle e.x e.y (toFloat (enemywidth e.kind)) 24
shotrect s = Coll.rectangle s.x s.y 6 12


shotEnemyCollision : World -> World -- collide player shots with enemies
shotEnemyCollision ({charge, enemies, shotsP} as world) =
    let
        enemyRects = L.map enemyrect (aliveEnemies world)
        playerShotsRects = L.map shotrect shotsP
        hitByShot e = L.any (\sr -> Coll.axisAlignedBoundingBox (enemyrect e) sr) playerShotsRects
        hitAnEnemy s = L.any (\er -> Coll.axisAlignedBoundingBox (shotrect s) er) enemyRects
        hitEnemies = L.filter hitByShot (aliveEnemies world)
    in
        {world | charge = min maxShots (charge + L.length hitEnemies)
               , enemies = unfilter hitByShot enemies
                           ++ L.map (\e -> {e | dead = Just corpseTime}) hitEnemies
               , shotsP = unfilter hitAnEnemy shotsP }

shotPlayerCollision : World -> World -- collide enemy shots with the player
shotPlayerCollision ({lives, enemies, shotsE} as world) =
    let
        enemyShotsRects = L.map shotrect shotsE
        playerHit = L.any (\sr -> Coll.axisAlignedBoundingBox (playerrect world) sr) enemyShotsRects
        hitThePlayer s = Coll.axisAlignedBoundingBox (playerrect world) (shotrect s)
    in
        {world | lives = if playerHit then lives - 1 else lives
               , shotsE = unfilter hitThePlayer shotsE}


{-
- ORGANIZATION
-}

handleCorpses : World -> World
handleCorpses ({ enemies } as world) =
    let
        decremented = L.map (\e -> {e | dead = M.map (\x->x-1) e.dead}) enemies -- decrement .dead for corpses
        stillThere = L.filter (\e -> maybeCondition' e.dead (\x -> x > 0) True) decremented
    in
        {world | enemies = stillThere}

grantCharge : World -> World
grantCharge ({ charge } as world) =
  if world.counter % chargeGrantCoefficient == 0
  then {world | charge = min maxShots (charge+1)}
  else world

incrementCounter : World -> World
incrementCounter world = { world | counter = (world.counter + 1) % 1200 }

changeMode : World -> World
changeMode world =
    if L.isEmpty world.enemies
    then
        { world | mode = Victory }
    else
        if world.lives <= 0
        then
            { world | mode = Defeat }
        else
            world


{-
- VIEW
-}

view : World -> E.Element
view world =
    let
        redCenterString pos str = zero pos << C.toForm << E.centered << Text.height 40 << Text.color Color.red << Text.fromString <| str
    in
        C.collage resX resY <| [ C.filled Color.black (C.rect resX resY)
                                --    , starsky... ;)
                               ]
                               -- this kille the whole application if worldBackground==True, WTF
                               --   ++ if worldBackground then [C.toForm << E.size (resX-50) (resY-100) << E.color (Color.greyscale 0.8) <| E.show world] else []
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
           , zero (875, 10) <| C.toForm <| E.centered <| Text.color Color.red <| Text.fromString <| toString w.lives
           , zero (800, 10) <| C.toForm <| E.image 32 32 "img/lightning.png"
           , zero (775, 10) <| C.toForm <| E.centered <| Text.color Color.blue <| Text.fromString <| toString w.charge
           ]

-- Sprites
-- TODO: clean this up a bit

player : Int -> C.Form
player pX = zero (playerpos pX) (C.toForm (E.image 52 32 "img/player.png"))

playerpos : Int -> (Float, Float)
playerpos pX = (32+toFloat pX*8, 460)

-- http://www.wolframalpha.com/input/?i=InterpolatingPolynomial%5B%7B%7B1%2C+24%7D%2C+%7B2%2C+32%7D%2C+%7B3%2C+36%7D%7D%2C+x%5D
enemywidth x = 24 + (8 - 2 * (x-2)) * (x-1) -- enemy_kind -> int

enemy : World -> Enemy -> C.Form
enemy w e =
    let
        ab w = if animcnt w <= 20 then "a" else "b"
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

input : Signal Action
input = Signal.merge leftNright space

-- sampleOn : Signal a -> Signal b -> Signal b
leftNright : Signal Action
leftNright =
    Signal.sampleOn clock
        (Signal.map
          (\ v -> if v == {x=-1, y=0} then LeftAction
                  else if v == {x=1, y=0} then RightAction
                  else NothingAction)
          Keyboard.arrows)
        --(Signal.filter (\ v -> v.y == 0) {x=0, y=0} Keyboard.arrows)
space : Signal Action
space = Signal.map (\v -> if v == True then ShootAction else NothingAction) Keyboard.space


{-
- MAIN
-}

main : Signal E.Element
main = Signal.map view (Signal.foldp update initial input)


{-
- UTIL - I hope i can get rid of these once they are added to the 3rd party Libraries
-}

-- circuithub/elm-list-extra contains this on master (as `removeWhen`) but not in the latest release
{-| Keep only elements that DON'T satisfy the predicate.
-}
unfilter : (a -> Bool) -> List a -> List a
unfilter p l = L.filter (\a -> not (p a)) l

-- Sent a pull request to circuithub/elm-maybe-extra
{-| Take a Maybe, a predicate and two values of Type b.
    Return the first b is the `Maybe` if `Just` and the predicate evaluates to true.
    Return the second b if the `Maybe` is `Nothing` or if the predicate evaluates to false.
-}
maybeCondition : Maybe a -> (a -> Bool) -> (a -> b) -> b -> b
maybeCondition m f b1 b2 =
  case m of
    Just a  -> if f a then b1 a else b2
    Nothing -> b2

maybeCondition' : Maybe a -> (a -> b) -> b -> b
maybeCondition' m f b2 =
  Maybe.withDefault b2 (Maybe.map f m)

-- Sent a pull request to circuithub/elm-list-extra
{-| Returns Just the element at the given index in the list,
or Nothing if the list is empty.
-}
getAt : List a -> Int -> Maybe a
getAt xs idx = List.head <| List.drop idx xs

-- TODO: Pull request to elm basics extra
{-| Round a float
-}
roundF = toFloat << round
