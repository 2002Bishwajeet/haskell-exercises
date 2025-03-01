-- Records
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- This is how you create a record
data Person = Person
  { name ::
      String,
    age ::
      Int
  }

--  TO use this

greet :: Person -> [Char]
greet person = "HI" ++ name person
greet (Person name _) = "HI" ++ name