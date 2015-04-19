module Main where

import Text.Printf
import Data.List
import Pipes
import qualified Pipes.Prelude as P
import Control.Monad
import Options.Applicative
import System.IO
import System.Exit (exitWith, ExitCode(..))
import Control.Monad.Trans.Either



type State = ([String], Int)
type Foldee = (State, Maybe String)

-- COST CENTRE    ticks
-- A       …      n₁                     A n₁
--  B      …      n₂            ===>     A;B n₂
--  C      …      n₃                     A;C n₃
-- 
conv :: Foldee -> String -> Foldee
conv (state@(stack,prevLevel),_) str
    | length ws == 10 && ticks>0 = ((stack',level), Just $ printf "%s %d" (intercalate ";" stack') ticks)
    | otherwise                  = (state,          Nothing)
    where 
      ws = words str
      level = length . takeWhile (==' ') $ str
      fun = printf "%s:%s" (ws!!1) (ws!!0)
      ticks = read (ws!!8) :: Int
      stack' = take (length stack - (prevLevel-level+1)) stack ++ [fun]


data Status = OK | PTooSmall deriving Show


work :: Producer String (EitherT Status IO) ()
     -> Producer String (EitherT Status IO) ()
work input =
     input >-> P.dropWhile (not . ((&&) <$> isPrefixOf "COST CENTRE"
                                        <*> isInfixOf "no."))
           >-> do line <- await
                  when ("%alloc" `isSuffixOf` line) . lift $ left PTooSmall
                  cat
           >-> P.dropWhile null
           >-> P.scan conv (([],0),Nothing) snd
           >-> P.concat
           >-> P.tee P.stdoutLn



data Options = Options {
      inputFile :: Maybe String
}

options = Options <$> optional (argument str (metavar "PROF-FILE"))


main :: IO ()
main = execParser opts >>= main'
    where opts = info (helper <*> options) fullDesc


main' :: Options -> IO ()
main' opts = do
  inp <- case inputFile opts of
           Just file -> P.fromHandle <$> openFile file ReadMode
           Nothing   -> return P.stdinLn

  r <- runEitherT . P.length $ work inp

  case r of
    Left PTooSmall -> die "Error: looks like it’s -p output, I need a -P one"
    Right 0        -> die "Error: the input doesn't look like a .prof"
    Right _        -> return ()


die :: String -> IO ()
die str = hPutStrLn stderr str >> exitWith (ExitFailure 1)
