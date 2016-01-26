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

import List.Extra as LE -- lift2
import Maybe.Extra as ME -- isJust
import Collision2D as C2D -- rectangle, axisAlignedBoundingBox
import AnimationFrame as AF


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
shotCoefficient = 5 --  0-100, 0 = no shots, 100 = hellfire
maxShots = 5
chargeGrantCoefficient = 2000 -- 1-5000, 1=always full shots, 1000=every second, 2000=every two seconds, ...
bitchMode = True -- missed shots come back at you
worldBackground = False -- display the world in the background


{-
- PORT
-}

port jsRNG : Int

--rngMailbox = S.mailbox Int


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
  --  , counter : Int -- auxiliary counter for charge granting and enemy animation
    , shotsP : List Shot
    , shotsE : List Shot
    , seed : R.Seed -- meh
    , gameTime : Time
    , delta : Time
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
          , lives   = 3
          , charge  = maxShots
          , enemies = initEnemies
          , foeDir = DirR
    --      , counter = 0
          , shotsP = []
          , shotsE = []
          , seed = R.initialSeed jsRNG -- we get an initialSeed from javascript at the start
          , gameTime = 0 -- mesaure how long the game already lasted
          , delta = 0 -- delta to the last frame
          }


{-
- UPDATE
-}

update : (Time, Action) -> World -> World
update (time, act) world =
    case world.mode of
            PreIngame ->
                processInputPreIngame time act world
            Ingame ->
                time2world time world
                |> processInputIngame act
                >> moveShots
                >> filterDeadShots
                >> moveEnemies
                >> letEnemiesShoot
                >> shotEnemyCollision
                >> shotPlayerCollision
                >> handleCorpses
                >> grantCharge
                >> changeMode
            Victory ->
                time2world time world
                |> moveShots
                >> filterDeadShots
            Defeat ->
                time2world time world
                |> moveShots
                >> filterDeadShots


time2world : Time -> World -> World
time2world time ({ gameTime, delta } as world) =
    { world | gameTime = gameTime + time
            , delta = time
    }


{-
- PRE INGAME
-}

processInputPreIngame : Time -> Action -> World -> World
processInputPreIngame time act world =
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
  let
      normalize z = z * world.delta / msPerFrame
  in
      {world | shotsP = L.map (\s -> {s | y=s.y-(normalize 5)}) shotsP
             , shotsE = L.map (\s -> {s | y=s.y+(normalize 5)}) shotsE }


filterDeadShots : World -> World
filterDeadShots ({ shotsP, shotsE } as world) =
    {world | shotsP = unfilter (\s -> s.y < -20) shotsP
           , shotsE = (unfilter (\s -> s.y > resY) shotsE)
                      ++ if bitchMode then L.filter (\s -> s.y < -20) shotsP else []}


moveEnemies : World -> World
moveEnemies ({ enemies, foeDir } as world) =
    let
        normalize z = z * world.delta / msPerFrame
        xmin = L.minimum (L.map .x enemies)
        xmax = L.maximum (L.map .x enemies)
        ymax = L.maximum (L.map .y enemies)
        moveSingleEnemy e x' y' = {e | x=e.x+(normalize x'),  y=e.y+(normalize y')}
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

playerrect world = C2D.rectangle (fst (playerpos world.playerX)) (snd (playerpos world.playerX)) 52 32
enemyrect e = C2D.rectangle e.x e.y (toFloat (enemywidth e.kind)) 24
shotrect s = C2D.rectangle s.x s.y 6 12


shotEnemyCollision : World -> World -- collide player shots with enemies
shotEnemyCollision ({charge, enemies, shotsP} as world) =
    let
        enemyRects = L.map enemyrect (aliveEnemies world)
        playerShotsRects = L.map shotrect shotsP
        hitByShot e = L.any (\sr -> C2D.axisAlignedBoundingBox (enemyrect e) sr) playerShotsRects
        hitAnEnemy s = L.any (\er -> C2D.axisAlignedBoundingBox (shotrect s) er) enemyRects
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
        playerHit = L.any (\sr -> C2D.axisAlignedBoundingBox (playerrect world) sr) enemyShotsRects
        hitThePlayer s = C2D.axisAlignedBoundingBox (playerrect world) (shotrect s)
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
grantCharge ({ charge } as w) =
  if toFloat ((animcnt w) % chargeGrantCoefficient) <= w.delta
  then {w | charge = min maxShots (charge+1)}
  else w

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
        redCenterString pos str = zero pos << C.toForm << E.centered << Txt.height 40 << Txt.color C.red << Txt.fromString <| str
    in
        C.collage resX resY <| [ C.filled C.black (C.rect resX resY)
                                --    , starsky... ;)
                               ]
                               -- this kills the whole application if worldBackground==True, WTF
                               -- k++ [C.toForm << E.size (resX-50) (resY-100) << E.color (C.greyscale 0.8) <| E.show world]
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
header w = [ zero (900, 10) << C.toForm <| E.image 32 32 "img/heart.png"
           , zero (875, 10) << C.toForm <| E.centered << Txt.height 18 << Txt.color C.red << Txt.fromString <| toString w.lives
           , zero (800, 10) << C.toForm <| E.image 32 32 "img/lightning.png"
           , zero (775, 10) << C.toForm <| E.centered << Txt.height 18 << Txt.color C.blue << Txt.fromString <| toString w.charge
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

input : Signal (Time, Action)
input = S.merge leftNright space

leftNright : Signal (Time, Action)
leftNright =
    Signal.sampleOn
        clock
        (Signal.map2 (,)
            (clock)
            (S.map
              (\ v -> if v == {x=-1, y=0} then LeftAction
                      else if v == {x=1, y=0} then RightAction
                      else NothingAction)
              Keyboard.arrows)
        )

space : Signal (Time, Action)
space = S.map2 (,) clock (S.map (always ShootAction) (S.filter identity False Keyboard.space))


{-
- MAIN
-}

main : Signal E.Element
main = S.map view (S.foldp update initial input)


{-
- UTIL - I hope i can get rid of these once they are added to the Community Libraries
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

-- Sent a pull request to dasch/elm-basics-extra
{-| Round a float
-}
roundF = toFloat << round
