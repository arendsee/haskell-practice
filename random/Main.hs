import System.Random

main :: IO ()
main = do
  let seed  = 42
  let range = (0,1) :: (Double, Double)
  let g     = mkStdGen seed
  let us    = randomRs range g
  print $ show $ take 10 us
