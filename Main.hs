{-# LANGUAGE MultiWayIf #-}
module Main where

import Text.Printf
import Data.List
import qualified Pipes.Prelude as P
import Pipes
import Pipes.Lift
import qualified Control.Foldl as F
import qualified Pipes.Extras as PE
import Control.Applicative
import Control.Monad
import Options.Applicative
import System.IO
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict hiding (State)


type State = ([String], Int)
type Foldee = (State,String)

fld :: Foldee -> String -> Foldee
fld (state@(stack,prevN),res) str
    | ticks>0   = ((stack',n), printf "%s %d" (intercalate ";" stack') ticks)
    | otherwise = (state, res)
    where 
      ws = words str
      n = length $ takeWhile (==' ') str
      fun = printf "%s:%s" (ws!!1) (ws!!0)
      ticks = read (ws!!8) :: Int
      stack' = take (length stack - (prevN-n+1)) stack ++ [fun]


data Status = OK | PTooSmall | NotProf deriving Show


work :: Effect (EitherT Status (StateT Status IO)) ()
work = 
       P.stdinLn
           >-> P.dropWhile (\x -> not (isPrefixOf "COST CENTRE" x && isInfixOf "no." x))

           >-> (do
                 line <- await
                 when ("%alloc" `isSuffixOf` line) $ lift $ left PTooSmall
                 lift.lift $ put OK
                 cat
                 )
            >-> P.stdoutLn



main :: IO ()
main = do 
  inp <- openFile "../pb.prof" ReadMode

  r <- flip runStateT NotProf $ 
       runEitherT $ 
       runEffect $ work

  case r of
    (Left PTooSmall, _) -> putStrLn "-P, not -p"
    (_, NotProf) -> putStrLn "doesn't look like a .prof"
    (Right (), OK) -> return ()

