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

type alias State = -- game state
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

initial : State
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

update : Action -> State -> State
update act state = processInput act state
                   |> moveShots
                   >> filterDeadShots
                   >> moveEnemies
                   >> letEnemiesShoot
                   >> shotEnemyCollision
                   >> shotPlayerCollision

processInput : Action -> State -> State
processInput act state = if act == Left
                          then
                           { state | playerX = state.playerX-1 }
                         else if act == Right
                          then
                           { state | playerX = state.playerX+1 }
                         else if act == Shoot
                          then
                           { state | shotsP = (newplayershot state :: state.shotsP) }
                         else
                           state

newplayershot : State -> Shot
newplayershot s = let ppos = playerpos s.playerX
                  in {x = fst ppos, y = snd ppos - 15}

moveShots : State -> State
moveShots state = {state | shotsP = L.map (\s -> {s | y=s.y-5}) state.shotsP
                         , shotsE = L.map (\s -> {s | y=s.y+5}) state.shotsE }

filterDeadShots : State -> State
filterDeadShots state = {state | shotsP = unfilter (\s -> s.y <  -50) state.shotsP
                               , shotsE = unfilter (\s -> s.y > resY) state.shotsE }

moveEnemies : State -> State
moveEnemies s = s

letEnemiesShoot : State -> State
letEnemiesShoot s = s

{-
 - COLLISION
 -}

playerrect state = Coll.rectangle (fst (playerpos state.playerX)) (snd (playerpos state.playerX)) 52 32
enemyrect e = Coll.rectangle e.x e.y (toFloat (enemywidth e.kind)) 24
shotrect s = Coll.rectangle s.x s.y 6 12


shotEnemyCollision : State -> State -- collide player shots with enemies
shotEnemyCollision state =
  let enemyRects = L.map enemyrect state.enemies
      playerShotsRects = L.map shotrect state.shotsP
      hitByShot e = L.any (\sr -> Coll.axisAlignedBoundingBox (enemyrect e) sr) playerShotsRects
      hitAnEnemy s = L.any (\er -> Coll.axisAlignedBoundingBox (shotrect s) er) enemyRects
  in {state | enemies = unfilter hitByShot state.enemies
            , shotsP = unfilter hitAnEnemy state.shotsP }

shotPlayerCollision : State -> State -- collide enemy shots with the player
shotPlayerCollision state =
  let enemyShotsRects = L.map shotrect state.shotsE
      playerHit = L.any (\sr -> Coll.axisAlignedBoundingBox (playerrect state) sr) enemyShotsRects
      hitThePlayer s = Coll.axisAlignedBoundingBox (playerrect state) (shotrect s)
  in {state | lives = if playerHit then state.lives - 1 else state.lives
            , shotsE = unfilter hitThePlayer state.shotsE}


{-
 - VIEW
 -}

view : State -> E.Element
view state = C.collage resX resY ( [ C.filled Color.black (C.rect resX resY)
                                 , player state.playerX
                                 , C.toForm << E.color Color.blue <| E.show state
                                 ]
                                 ++ (List.map enemy state.enemies)
                                 ++ (List.map shotP state.shotsP)
                                 ++ (List.map shotE state.shotsE)
                               )

-- Sprites
player : Int -> C.Form
player pX = zero (playerpos pX) (C.toForm (E.image 52 32 "img/player.png"))
playerpos : Int -> (Float, Float)
playerpos pX = (56+toFloat pX*8, 460)
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
