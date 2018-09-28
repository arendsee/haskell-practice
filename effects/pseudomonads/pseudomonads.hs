infixl 4 &
infixl 1 <<, #, >>

-- generalized return
--      return :: p -> f p
type UnitFn p q = p -> q

-- generalized bind and pseudobind
--              bind :: f a       -> (a -> f b) -> f b
type       BindFn p q = p         -> (p ->   q) ->   q
type PseudoBindFn p q = Monad q r -> (p ->   r) ->   r 

data Monad p q = Monad {
      unit :: UnitFn p q
    , bind :: BindFn p q
  }

data Pseudomonad p q = Pseudomonad {
      pseudounit :: UnitFn p q
    , pseudobind :: PseudoBindFn p q
  }

idmonad :: Monad p p
idmonad = Monad (\x -> x) (\x f -> f x)
