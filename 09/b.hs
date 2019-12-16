import Data.List as L
import qualified Data.List.Zipper as Z
import qualified Data.Map.Strict as M

type State = (Z.Zipper Int, M.Map Int Int)

players, marbles :: Int
players = 413
marbles = 7108200

main :: IO ()
main = do
    let (ring, scores) = foldl loop (Z.empty, M.empty) [0..marbles]
    print $ maximum $ M.elems $ scores

cw :: Z.Zipper a -> Z.Zipper a
cw z = if Z.endp z then Z.right $ Z.start z else Z.right z

ccw :: Z.Zipper a -> Z.Zipper a
ccw z = if Z.beginp z then Z.left $ Z.end z else Z.left z

loop :: State -> Int -> State
loop (z, dic) i = if i > 0 && mod i 23 == 0
    then 
        let
            player = mod i players
            a = i
            moved = foldl (\z _ -> ccw z) z [0..7]
            b = Z.cursor moved
        in
            (cw $ Z.delete moved, foldr (M.insertWith (+) player) dic [a, b])
    else (Z.push i $ cw z, dic)
