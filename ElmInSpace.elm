import Graphics.Collage as C
import Graphics.Element as E
import Signal
import Keyboard
import Time
import Color
import List as L
import List.Extra as LE

resX = 900
resY = 500

type alias State = -- game state
  { playerX : Int
  , enemies : List Enemy
  , shotsP  : List Shot
  , shotsE  : List Shot
  }

type alias Enemy = 
  { pos  : (Float,Float)
  , kind : Int -- 1,2,3 -- specifies sprite
  }

-- type EnemyType = Bot | Mid | Top | Ufo

type alias Shot = 
  { x : Float
  , y : Float
  }

type Action = Left | Right | Shoot | Nothing

initEnemies : List Enemy
initEnemies =
  let xlist = L.map (\x->x*15) [0..9]
      ylist = L.map (\x->x*15) [0..5]
      pp x y = (toFloat (70+5*x), toFloat (280+2*y)) -- position formula
      kk y = y // round (resY*0.06) + 1 -- kind formula
  in LE.lift2 (\x y -> {pos = pp x y, kind = kk y }) xlist ylist

initial : State
initial = { playerX = 0
          , enemies = initEnemies
          , shotsP = []
          , shotsE = []
          }

update : Action -> State -> State
update act state = let s2 = processInput act state
                   in moveShots s2

processInput : Action -> State -> State
processInput act state = if act == Left
                          then
                           { state | playerX = state.playerX-1 }
                         else if act == Right
                          then
                           { state | playerX = state.playerX+1 }
                         else if act == Shoot
                          then
                           { state | shotsP = (newshot state :: state.shotsP) }
                         else
                           state

newshot s = let z = playerpos s.playerX in {x = fst z, y = snd z}

moveShots : State -> State
moveShots state = {state | shotsP = L.map (\s -> {s | x=s.x, y=s.y+5}) state.shotsP
                         , shotsE = state.shotsE }

view : State -> E.Element
view state = C.collage 900 500 ( [ C.filled Color.black (C.rect 900 500)
                                 , player state.playerX
                                 , C.toForm << E.color Color.red <| E.show state
                                 ]
                                 ++ (List.map enemy state.enemies)
                                 ++ (List.map shotP state.shotsP)
                                 ++ (List.map shotE state.shotsE)
                               )

main : Signal E.Element
main = Signal.map view (Signal.foldp update initial input)


-- Sprites
player : Int -> C.Form
player pX = zero (playerpos pX) (C.toForm (E.image 52 32 "img/player.png"))
playerpos : Int -> (Float, Float)
playerpos pX = (56+toFloat pX*8, 80)
enemy : Enemy -> C.Form
-- http://www.wolframalpha.com/input/?i=InterpolatingPolynomial%5B%7B%7B1%2C+24%7D%2C+%7B2%2C+32%7D%2C+%7B3%2C+36%7D%7D%2C+x%5D
enemy e = let width x = 24 + (8 - 2 * (x-2)) * (x-1)
          in zero e.pos (C.toForm (E.image (width e.kind) 24 ("img/enemy"++toString e.kind++"a3.png")))
shotP : Shot -> C.Form
shotP s = zero (s.x,s.y) (C.toForm (E.image 6 12 "img/playershot.png"))
shotE : Shot -> C.Form
shotE s = zero (s.x,s.y) (C.toForm (E.image 6 12 "img/enemyshot.png"))
zero : (Float, Float) -> C.Form -> C.Form
zero (x,y) f = C.move (x,y) (C.move (-450,-250) f)

-- Input
input : Signal Action
input = Signal.merge leftNright space

-- sampleOn : Signal a -> Signal b -> Signal b
leftNright : Signal Action
leftNright = Signal.sampleOn clocker
               (Signal.map
                 (\ v -> if v == {x=-1, y=0} then Left
                         else if v == {x=1, y=0} then Right
                         else Nothing)
                 Keyboard.arrows)
               --(Signal.filter (\ v -> v.y == 0) {x=0, y=0} Keyboard.arrows)
space : Signal Action
space = Signal.map (\v -> if v == True then Shoot else Nothing) Keyboard.space


clocker = Time.fps 30

