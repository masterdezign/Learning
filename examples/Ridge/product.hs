import qualified Data.Vector.Storable as V
import           Numeric.LinearAlgebra

main = do
  m <- loadMatrix "file.txt"
  let m' = (tr' m) <> m
  print m'
