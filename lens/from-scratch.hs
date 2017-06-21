{-# LANGUAGE RankNTypes #-}

-- Here I am following the blog:
-- https://blog.jakuba.net/2014/07/14/lens-tutorial-introduction-part-1.html

data Character
  = Character {
      name   :: String
    , weapon :: Weapon
  } deriving(Show) 

data Weapon
  = Weapon {
      dmg :: Int
  } deriving(Show)

you = Character { name = "sparkletoes", weapon = Weapon { dmg = 1 } }

-- Now we want to set a Character weapon's damage
-- In "normal" language we could do:
-- Bob.Weapon.dmg = 10
-- But this gets ugly in naive Haskell

setWeaponDmg :: Int -> Character -> Character
setWeaponDmg d c = c { weapon = (weapon c) { dmg = d } } 

-- now let's try making standard setters and getters

-- functions inside records a little different
-- they take an implicit argument
-- so the type of `set` is
--   set :: NaiveLens s a -> a -> s -> s
data NaiveLens s a
  = NaiveLens {
      view :: s -> a  -- get
    , set  :: a -> s -> s -- set
  }
-- where `a` is the value inside the object
--       `s` is the object
-- Or, for whatever reason, in lens nomenclature, `s` stands for object and
-- `a` stands for focus.

nameLens :: NaiveLens Character String
nameLens = NaiveLens name (\a s -> s { name = a})

-- set your name
newyou = set nameLens "sparklefinger" you
-- get your name
yourname = view nameLens you


-- this is all fine an dandy, but what if you want to be able to map a function
-- over the record?

-- and here is a mapping function
modName :: (String -> String) -> Character -> Character
modName f c = set nameLens (f (view nameLens c)) c

-- I could add this in:
data NaiveLens' s a
  = NaiveLens' {
      view' :: s -> a  -- get
    , set'  :: a -> s -> s -- set
    , over' :: (a -> a) -> s -> s
  }
-- I call this over to forshadow the actual lens name

nameLens' :: NaiveLens' Character String
nameLens'
  = NaiveLens' {
      view' = name
    , set'  = \a s -> s { name = a }
    , over' = \f c -> set nameLens (f (view nameLens c)) c
  }

evilyou = over' nameLens' reverse you

-- However, we can factor out set as a special case of over', with cons
data NaiveLens'' s a
  = NaiveLens'' {
      view'' :: s -> a  -- get
    , over'' :: (a -> a) -> s -> s
  }
nameLens'' :: NaiveLens'' Character String
nameLens''
  = NaiveLens'' {
      view'' = name
    , over'' = \f c -> set nameLens (f (view nameLens c)) c
  }
set'' :: NaiveLens'' s a -> a -> s -> s
set'' ln a s = over'' ln (const a) s 

notyou = set'' nameLens'' "fred" you


-- -- Now we want to be able to do fancy things like computing in a context (e.g.
-- -- IO) So we could, again, add another function
-- data NaiveLens''' s a
--   = NaiveLens''' {
--       view''' :: s -> a  -- get
--     , over''' :: (a -> a) -> s -> s
--     , with''' :: (a -> f a) -> s -> f s
--   }

-- but you can implement everything else in terms of this it has a neat name, a
-- Laarhoven lens, probably named after the clever chap that invented it.

-- General form: Functor f => (a -> f a) -> s -> f s

lens :: Functor f => (a -> f a) -> s -> f s

-- how to manipulate this into view and over?

data Identity
  = Identity {
      toIdentity :: a -> Identity a
    , fromIdentity :: Identity a -> a
  }
