data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show)

abbrFirstName :: Person -> Person
abbrFirstName p = p { firstName = if length (firstName p) < 2 then firstName p else head (firstName p) : ['.']}