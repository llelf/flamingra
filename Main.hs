{-# LANGUAGE RecordWildCards #-}
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

data CostCentre = CostCentre {
      ccLevel :: Int,
      ccName :: String,
      ccModule :: String,
      ccTicks :: Integer
    }

costCentreFormatFun :: CostCentre -> String
costCentreFormatFun cc = printf "%s:%s" (ccModule cc) (ccName cc)

-- COST CENTRE    ticks
-- A       …      n₁                     A n₁
--  B      …      n₂            ===>     A;B n₂
--  C      …      n₃                     A;C n₃
-- 
conv :: Foldee -> String -> Foldee
conv (state@(stack,prevLevel),_) str
    | Just cc@CostCentre{..} <- parseLine str,
       ccTicks > 0
           = let stack' = take (length stack - (prevLevel - ccLevel + 1)) stack <> [fun]
                 fun    = costCentreFormatFun cc
             in ((stack', ccLevel), Just $ printf "%s %d" (intercalate ";" stack') ccTicks)
    | otherwise
           = (state, Nothing)



parseLine :: String -> Maybe CostCentre
parseLine str = costCentre <$> params
    where
      params | length ws == 10 = Just fields10
             | length ws == 11 = Just fields11
             | otherwise       = Nothing

      costCentre (fun,mod,ticks) = CostCentre level fun mod (read ticks)

      ws = words str
      level = length . takeWhile (==' ') $ str

      fields11 = (fun,mod,ticks)
          where [fun,mod,_src,_no,_entries,_indTime,_indAlloc,_inhTime,_inhAlloc,ticks,_bytes] = ws
      fields10 = (fun,mod,ticks)
          where [fun,mod,_no,_entries,_indTime,_indAlloc,_inhTime,_inhAlloc,ticks,_bytes] = ws



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
