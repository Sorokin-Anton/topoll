module RipsSpec where
import qualified Data.Vector as V
import Topoll.DistanceMatrix.DistanceMatrix ( DistanceMatrix (DistanceMatrix) )
import Topoll.Complexes.Rips
import Topoll.SimplicialSet
import Test.Hspec
import Named

distance :: (Float, Float) -> (Float, Float) -> Float
distance (a, b) (c, d) = sqrt $ (a - c) * (a - c) + (b - d) * (b - d)

distanceMatrix :: [(Float, Float)] -> [(Float, Float)] -> DistanceMatrix
distanceMatrix dataPoints landmarkPoints = DistanceMatrix . V.fromList . map V.fromList $ distanceMatrixList dataPoints landmarkPoints where
  distanceMatrixList [] _ = []
  distanceMatrixList (p : ps) lPoints = map (distance p) lPoints : distanceMatrixList ps lPoints

testDataPoints :: [(Float, Float)]
testDataPoints = [(1, -1), (-1, 1), (2, 2)]


r :: SimplicialSet
r = ripsSet (distanceMatrix testDataPoints testDataPoints) ! #r 1.5

spec :: Spec
spec = describe "ripsSpec" $ do
  it "behaves good at examples" $ do
    Just r `shouldBe` simplicialSetFullCheck [[],[0],[0,1],[1],[2]]
