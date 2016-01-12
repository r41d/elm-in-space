import Graphics.Collage as C
import Graphics.Element as E
import Signal
import Keyboard
import Time
import Color
import List as L
import List.Extra as LE
import Collision2D as Coll


{-
 - CONFIG
 -}

-- Resolution: Quarter of Full HD
resX = 960
resY = 540
fps = 30


{-
 - DATA
 -}

type alias World = -- game world
  { playerX : Int
  , lives   : Int
  , enemies : List Enemy
  , nextDir : Direction
  , shotsP  : List Shot
  , shotsE  : List Shot
  }

type Direction = DirL | DirR | DirD

type alias Enemy = -- x y cooridinates spcify the center of the sprite
  { x     : Float
  , y     : Float
  , kind  : Int -- 1,2,3 -- specifies sprite image
  , alive : Bool
  }

type alias Shot = -- x y cooridinates spcify the center of the sprite
  { x : Float
  , y : Float
  }

type Action = Left | Right | Shoot | Nothing


{-
 - INIT
 -}

initEnemies : List Enemy
initEnemies =
  let xlist = L.map (\x->x*15) [0..9]
      ylist = L.map (\x->x*15) [0..5]
      ppx x = 60+5 * toFloat x -- position formulas
      ppy y = 80+2.5 * toFloat y
      kk y = y // 30 + 1 -- kind formula
  in LE.lift2 (\xx yy -> {x = ppx xx, y = ppy yy, kind = kk yy, alive = True}) xlist ylist

initial : World
initial = { playerX = 0
          , lives   = 3
          , enemies = initEnemies
          , nextDir = DirR
          , shotsP = []
          , shotsE = [{x=50,y=0}]
          }


{-
 - UPDATE
 -}

update : Action -> World -> World
update act world = processInput act world
                   |> moveShots
                   >> filterDeadShots
                   >> moveEnemies
                   >> letEnemiesShoot
                   >> shotEnemyCollision
                   >> shotPlayerCollision

processInput : Action -> World -> World
processInput act world = if act == Left
                          then
                           { world | playerX = max 0 (world.playerX-1) }
                         else if act == Right
                          then
                           { world | playerX = min 105 (world.playerX+1) }
                         else if act == Shoot
                          then
                           { world | shotsP = (newplayershot world :: world.shotsP) }
                         else
                           world

newplayershot : World -> Shot
newplayershot s = let ppos = playerpos s.playerX
                  in {x = fst ppos, y = snd ppos - 15}

moveShots : World -> World
moveShots world = {world | shotsP = L.map (\s -> {s | y=s.y-5}) world.shotsP
                         , shotsE = L.map (\s -> {s | y=s.y+5}) world.shotsE }

filterDeadShots : World -> World
filterDeadShots world = {world | shotsP = unfilter (\s -> s.y <  -50) world.shotsP
                               , shotsE = unfilter (\s -> s.y > resY) world.shotsE }

moveEnemies : World -> World
moveEnemies world = {world | enemies = world.enemies}

letEnemiesShoot : World -> World
letEnemiesShoot s = s

{-
 - COLLISION
 -}

playerrect world = Coll.rectangle (fst (playerpos world.playerX)) (snd (playerpos world.playerX)) 52 32
enemyrect e = Coll.rectangle e.x e.y (toFloat (enemywidth e.kind)) 24
shotrect s = Coll.rectangle s.x s.y 6 12


shotEnemyCollision : World -> World -- collide player shots with enemies
shotEnemyCollision world =
  let enemyRects = L.map enemyrect world.enemies
      playerShotsRects = L.map shotrect world.shotsP
      hitByShot e = L.any (\sr -> Coll.axisAlignedBoundingBox (enemyrect e) sr) playerShotsRects
      hitAnEnemy s = L.any (\er -> Coll.axisAlignedBoundingBox (shotrect s) er) enemyRects
  in {world | enemies = unfilter hitByShot world.enemies
            , shotsP = unfilter hitAnEnemy world.shotsP }

shotPlayerCollision : World -> World -- collide enemy shots with the player
shotPlayerCollision world =
  let enemyShotsRects = L.map shotrect world.shotsE
      playerHit = L.any (\sr -> Coll.axisAlignedBoundingBox (playerrect world) sr) enemyShotsRects
      hitThePlayer s = Coll.axisAlignedBoundingBox (playerrect world) (shotrect s)
  in {world | lives = if playerHit then world.lives - 1 else world.lives
            , shotsE = unfilter hitThePlayer world.shotsE}


{-
 - VIEW
 -}

view : World -> E.Element
view world = C.collage resX resY ( [ C.filled Color.black (C.rect resX resY)
                                 , player world.playerX
                                 , C.toForm << E.color Color.blue <| E.show world
                                 ]
                                 ++ (List.map enemy world.enemies)
                                 ++ (List.map shotP world.shotsP)
                                 ++ (List.map shotE world.shotsE)
                               )

-- Sprites
player : Int -> C.Form
player pX = zero (playerpos pX) (C.toForm (E.image 52 32 "img/player.png"))
playerpos : Int -> (Float, Float)
playerpos pX = (32+toFloat pX*8, 460)
-- http://www.wolframalpha.com/input/?i=InterpolatingPolynomial%5B%7B%7B1%2C+24%7D%2C+%7B2%2C+32%7D%2C+%7B3%2C+36%7D%7D%2C+x%5D
enemywidth x = 24 + (8 - 2 * (x-2)) * (x-1) -- enemy_kind -> int
enemy : Enemy -> C.Form
enemy e = zero (e.x,e.y) (C.toForm (E.image (enemywidth e.kind) 24 ("img/enemy"++toString e.kind++"a3.png")))
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
leftNright = Signal.sampleOn (Time.fps fps)
               (Signal.map
                 (\ v -> if v == {x=-1, y=0} then Left
                         else if v == {x=1, y=0} then Right
                         else Nothing)
                 Keyboard.arrows)
               --(Signal.filter (\ v -> v.y == 0) {x=0, y=0} Keyboard.arrows)
space : Signal Action
space = Signal.map (\v -> if v == True then Shoot else Nothing) Keyboard.space


{-
 - MAIN
 -}

main : Signal E.Element
main = Signal.map view (Signal.foldp update initial input)


{-
 - UTIL
 -}

-- Keep only elements that DON'T satisfy the predicate.
unfilter : (a -> Bool) -> List a -> List a
unfilter p l = L.filter (\a -> not (p a)) l
