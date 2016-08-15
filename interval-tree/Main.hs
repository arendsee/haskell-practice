import Intervals

main = do
    let a = Interval 1 3
    let b = Interval 2 5
    let c = Interval 4 5
    print $ (overlap a b)
    print $ (overlap a c)
