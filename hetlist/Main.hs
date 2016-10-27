data Het a = Value a
data Compound a b = Compound (Het a) (Het b)


data Kind = KString String | KInt Int | KKinds Kind Kind deriving Show
