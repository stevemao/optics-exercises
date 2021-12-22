{-# LANGUAGE TemplateHaskell #-}
module VirtualFields where
import Control.Lens

data User =
  User { _firstName :: String
       , _lastName :: String
       , _email :: String
       } deriving (Show)

makeLenses ''User

username :: Lens' User String 
username = email

fullName :: Lens' User String
fullName = lens getter setter
    where getter user = view firstName user <> " " <> view lastName user
          setter user newValue = set lastName lname (set firstName fname user)
                        where separated = words newValue
                              fname = head separated
                              lname = unwords (tail separated)

-- >>> let user = User "John" "Cena" "invisible@example.com"
-- >>> view fullName user
-- "John Cena"
-- >>> set fullName "Doctor of Thuganomics" user
-- User {_firstName = "Doctor", _lastName = "of Thuganomics", _email = "invisible@example.com"}
