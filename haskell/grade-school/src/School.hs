module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import Data.List (sort)
import Data.Maybe (maybeToList)
import Control.Monad (join)

data Grade = K | G1 | G2 | G3 | G4 | G5 | G6 | G7 | G8 | G9 | G10 | G11 | G12
  deriving (Eq, Ord, Enum)

type Name = String
data School = School (M.Map Grade [Name])

add :: Int -> String -> School -> School
add gradeNum student (School school)
  | gradeNum < 0 || gradeNum > 12 = School school
  | otherwise = School $ M.adjust ((:) student) (toEnum gradeNum) school

empty :: School
empty = School $ M.fromList [ (K,  []), (G1, []), (G2, []), (G3, []), (G4, [])
                            , (G5, []), (G6, []), (G7, []), (G8, []), (G9, [])
                            , (G10,[]), (G11,[]), (G12,[])
                            ]

grade :: Int -> School -> [String]
grade gradeNum (School school)
  | gradeNum < 0 || gradeNum > 12 = []
  | otherwise = sort $ join $ maybeToList $ M.lookup (toEnum gradeNum) school

sorted :: School -> [(Int, [String])]
sorted (School school) = filter (\(g, ns) -> ns /= []) $
  (\(g, ns) -> (fromEnum g, sort ns)) <$> (M.toList school)
