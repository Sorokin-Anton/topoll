module WitnessSpec where
import qualified Data.Vector as V
import Topoll.DistanceMatrix.DistanceMatrix
import Topoll.Complexes.Witness
import Topoll.SimplicialSet
import Test.Hspec
import Data.Vector (Vector)
import Named

distance :: (Float, Float) -> (Float, Float) -> Float
distance (a, b) (c, d) = sqrt $ (a - c) * (a - c) + (b - d) * (b - d)

distanceMatrix :: [(Float, Float)] -> [(Float, Float)] -> Vector Point
distanceMatrix dataPoints landmarkPoints = V.fromList . map V.fromList $ distanceMatrixList dataPoints landmarkPoints where
  distanceMatrixList [] _ = []
  distanceMatrixList (p : ps) lPoints = map (distance p) lPoints : distanceMatrixList ps lPoints

testDataPoints :: [(Float, Float)]
testDataPoints = [(1, -1), (-1, 1), (2, 2)]

testLandmarkPoints :: [(Float, Float)]
testLandmarkPoints = [(0,0), (2,0), (0,2)]

w :: SimplicialSet
Right w = witnessSet (distanceMatrix testDataPoints testLandmarkPoints) ! #r 0 ! #nu 2

spec :: Spec
spec = describe "ripsSet" $ do
  it "behaves good at examples" $ do
    Just w `shouldBe` simplicialSetFullCheck [[],[0],[0,1],[0,1,2],[0,2],[1],[1,2],[2]]
