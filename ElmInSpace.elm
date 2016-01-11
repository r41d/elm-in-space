import Graphics.Collage as C
import Graphics.Element as E
import Signal
import Keyboard
import Time
import Color

type alias State = -- game state
  { playerX : Int
  , enemies : List Enemy
  , shotsP  : List Shot
  , shotsE  : List Shot
  }

type alias Enemy = 
  { pos  : (Float,Float)
  , kind : Int
  }

type alias Shot = 
  { pos    : (Float,Float)
  }

type Action = Left | Right | Shoot | Nothing

initial : State
initial = { playerX = 0
          , enemies = []
          , shotsP = []
          , shotsE = []
          }

update : Action -> State -> State
update act state = if act == Left
                    then
                     { state | playerX = state.playerX-1 }
                   else if act == Right
                    then
                     { state | playerX = state.playerX+1 }
                   else if act == Shoot
                    then
                     { state | shotsP = ({pos = playerpos state.playerX} :: state.shotsP) }
                   else
                     state

view : State -> E.Element
view state = C.collage 900 500 ( [ C.filled Color.black (C.rect 900 500)
                                 , player state.playerX
                                 , C.toForm (E.color Color.red (E.show state))
                                 ]
                                 ++ (List.map (\e -> enemy e) state.enemies)
                                 ++ (List.map (\s -> shotP s) state.shotsP)
                                 ++ (List.map (\s -> shotE s) state.shotsE)
                               )

main : Signal E.Element
main = Signal.map view (Signal.foldp update initial input)


-- Sprites
player : Int -> C.Form
player pX = zero (playerpos pX) (C.toForm (E.image 52 32 "img/player.png"))
playerpos : Int -> (Float, Float)
playerpos pX = (56+toFloat pX*8, 80)
enemy : Enemy -> C.Form
enemy e = zero e.pos (C.toForm (E.image 24 24 ("img/emeny"++toString e.kind++"a3.png")))
shotP : Shot -> C.Form
shotP s = zero s.pos (C.toForm ( E.image 6 12 "img/playershot.png"))
shotE : Shot -> C.Form
shotE s = zero s.pos (C.toForm ( E.image 6 12 "img/enemyshot.png"))
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

