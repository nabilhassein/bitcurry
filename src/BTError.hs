module BTError where

data BTError = NoParse | NoKey String | FailureReason String

instance Show BTError where
  show NoParse   = "no parse"
  show (NoKey s) = "no key: " ++ s
  show (FailureReason s) = "failure reason: " ++ s
