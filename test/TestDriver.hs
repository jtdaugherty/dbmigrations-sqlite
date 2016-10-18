module Main where
import Test.HUnit
import System.Exit
import System.IO ( stderr )

import qualified BackendTest

import Control.Exception ( finally )

import Database.HDBC ( IConnection(disconnect) )
import Database.HDBC.Sqlite3 ( connectSqlite3 )

loadTests :: IO [Test]
loadTests = do

  sqliteConn <- connectSqlite3 ":memory:"

  let testAct = (BackendTest.tests (BackendTest.HDBCConnection sqliteConn))
                `finally`
                (disconnect sqliteConn)
  return [ ("SQLite backend tests") ~: test testAct ]

main :: IO ()
main = do
  tests <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
