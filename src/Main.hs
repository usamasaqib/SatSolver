module Main where

import qualified Solver as S
import qualified ThreadManagement as TM
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Control.Monad.Fix (fix)
 
type Proposition = String

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4444 iNADDR_ANY)
  listen sock 2
  map <- TM.newManager
  mainLoop sock map 0
  
mainLoop :: Socket -> TM.ThreadManager -> Int -> IO ()
mainLoop sock map clientNum = do
  conn <- accept sock
  putStrLn ("Connection from " ++ show conn ++ " accepted.")
  forkIO (worker conn map clientNum)
  mainLoop sock map $! clientNum + 1

worker :: (Socket, SockAddr) -> TM.ThreadManager -> Int -> IO ()
worker (sock, _) map clientNum = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl ("Hi, What action would you like to perform." ++
                   "\nTo query results, enter: res\nTo submit formula, enter: prop")
    action <- fmap init (hGetLine hdl)
    hPutStr hdl action

    case action of
        "res" -> do
                hPutStr hdl "Enter your ticket>"
                ticket <- fmap init (hGetLine hdl)
                solution <- TM.findResult map ticket
                case solution of
                    Nothing -> hPutStrLn hdl "Invalid ticket"
                    (Just TM.Computing) -> hPutStrLn hdl "Computation is still running. Please return later."
                    (Just (TM.Completed [])) -> hPutStrLn hdl "No possible solution"
                    (Just (TM.Completed x)) -> hPutStrLn hdl (show x) 
        
        "prop" -> do
                hPutStrLn hdl ("Please Enter the boolean proposition.\nGrammer: " ++
                               "b_prop ::= var | (b_prop | b_prop) | (b_prop & b_prop) | (~b_prop {NO SPACE AFTER THE TILDA ( ~ ) } )")
                prop <- fmap init (hGetLine hdl)
                --parsedProp <- return $ S.parse prop
                let parsedProp = S.parse prop
                case parsedProp of
                  "Invalid Input" -> hPutStrLn hdl "Invalid Input"
                  _ -> do
                    --ticket <- return $ makeTicket clientNum
                    let ticket = makeTicket clientNum
                    _ <- TM.forkManaged map ticket parsedProp solveSat
                    hPutStrLn hdl ("Thank you. Return at a later time for the solution." ++
                     " Use your ticket to lookup the solution.\nTicket: " ++ ticket)

        _ -> hPutStrLn hdl "\nInvalid Option Selected"

    hClose hdl


-- The reason I made a seperate function was so that appropriate uniquie and unpredictable
-- identifiers could be generated, as is needed for any secure system of this nature. However for
-- now I went with this entirely insufficient solution.
makeTicket :: Int -> String
makeTicket clientNum = show (clientNum + 1)  


solveSat :: Proposition -> [(Char, Bool)]
solveSat prop = if sol == Nothing then [] else fromJust sol
 where sol = S.evaluate prop
       fromJust (Just x) = x
