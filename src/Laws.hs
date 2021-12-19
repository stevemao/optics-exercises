module Laws where
import Control.Lens

data Err = 
    ReallyBadError { _msg :: String } 
  | ExitCode       { _code :: Int }
    deriving Eq

msg :: Lens' Err String
msg = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    -- Hrmm, I guess we just return ""?
    getMsg (ExitCode _) = ""
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    -- Nowhere to set it, I guess we do nothing?
    setMsg (ExitCode n) newMessage = ExitCode n

-- >>> let newMessage = "False alarm!"
-- >>> view msg (set msg newMessage (ReallyBadError "BAD BAD BAD")) == newMessage
-- True

-- >>> let newMessage = "False alarm!"
-- >>> view msg (set msg newMessage (ExitCode 1)) == newMessage
-- False

-- >>> set msg (view msg (ExitCode 1)) (ExitCode 1) == (ExitCode 1)
-- True

-- >>> set msg (view msg (ReallyBadError "BAD BAD BAD")) (ReallyBadError "BAD BAD BAD") == (ReallyBadError "BAD BAD BAD")
-- True

-- >>> let newMessage = "False alarm!"
-- >>> let newMessage2 = "Alarm!"
-- >>> set msg newMessage2 (set msg newMessage (ExitCode 2)) == set msg newMessage2 (ExitCode 2)
-- True

-- >>> let newMessage = "False alarm!"
-- >>> let newMessage2 = "Alarm!"
-- >>> set msg newMessage2 (set msg newMessage (ReallyBadError "BAD BAD BAD")) == set msg newMessage2 (ReallyBadError "BAD BAD BAD")
-- True


msg' :: Lens' Err String
msg' = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    -- Hrmm, I guess we just return ""?
    getMsg (ExitCode _) = ""
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    -- Nowhere to set it, I guess we do nothing?
    setMsg (ExitCode n) newMessage = ReallyBadError newMessage

-- >>> let newMessage = "False alarm!"
-- >>> view msg' (set msg' newMessage (ReallyBadError "BAD BAD BAD")) == newMessage
-- True

-- >>> let newMessage = "False alarm!"
-- >>> view msg' (set msg' newMessage (ExitCode 1)) == newMessage
-- True

-- >>> set msg' (view msg' (ExitCode 1)) (ExitCode 1) == (ExitCode 1)
-- False

-- >>> set msg' (view msg' (ReallyBadError "BAD BAD BAD")) (ReallyBadError "BAD BAD BAD") == (ReallyBadError "BAD BAD BAD")
-- True

-- >>> let newMessage = "False alarm!"
-- >>> let newMessage2 = "Alarm!"
-- >>> set msg' newMessage2 (set msg' newMessage (ExitCode 2)) == set msg' newMessage2 (ExitCode 2)
-- True

-- >>> let newMessage = "False alarm!"
-- >>> let newMessage2 = "Alarm!"
-- >>> set msg' newMessage2 (set msg' newMessage (ReallyBadError "BAD BAD BAD")) == set msg' newMessage2 (ReallyBadError "BAD BAD BAD")
-- True


newtype Ex1 = Ex1 Int
    deriving (Eq, Show)

ex1 :: Lens' Ex1 Int 
ex1 = lens getEx1 setEx1
    where getEx1 = const 1
          setEx1 e i = if i == 100 then e else Ex1 20 

-- >>> view ex1 (set ex1 10 (Ex1 1)) == 10
-- False

-- >>> set ex1 (view ex1 (Ex1 1)) (Ex1 1) == Ex1 1
-- False

-- >>> set ex1 100 (set ex1 30 (Ex1 40)) == set ex1 100 (Ex1 40)
-- False


data Builder =
    Builder { _context :: [String]
            , _build   :: [String] -> String
            }

instance Eq Builder where
    Builder a f == Builder b g = a == b && f a == g b

build :: Lens' Builder String
build = lens getBuilder setBuilder
    where getBuilder (Builder c b) = b c
          setBuilder (Builder c b) s = Builder c (const s)

-- >>> f _ = "ret"
-- >>> view build (set build "test" (Builder [] f)) == "test"
-- True

-- >>> f _ = "ret"
-- >>> set build (view build (Builder [] f)) (Builder [] f) == Builder [] f
-- True

-- >>> f _ = "ret"
-- >>> set build "new value" (set build "old value" (Builder [] f)) == set build "new value" (Builder [] f)
-- True
