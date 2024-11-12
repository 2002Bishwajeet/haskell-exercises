data Color = Red | Black
    deriving (Eq, Show, Enum)

colors = [Red .. Black]

data Suit = Spades | Heart | Diamond Bool | Clubs 

f x = read(show x)

-- This won't work unless you determine the output type
{- Also it won't work if you call f 32 as show returns "32" and when read gets this, it remvoves the quotes 
    so its a number thus it fails to parse it as a string -}