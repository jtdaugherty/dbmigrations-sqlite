{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import Database.Schema.Migrations.Test.BackendTest as BackendTest

import Control.Exception (Handler(..), catches, finally )
import Database.HDBC ( IConnection(disconnect) )
import qualified Database.HDBC as HDBC
import Database.HDBC.Sqlite3 ( connectSqlite3 )
import System.Exit
import System.IO ( stderr )
import Test.HUnit

data SQLiteBackendConnection =
    forall a. HDBC.IConnection a => HDBCConnection a

instance BackendConnection SQLiteBackendConnection where
    supportsTransactionalDDL = const True
    makeBackend (HDBCConnection c) = hdbcBackend c
    commit (HDBCConnection c) = HDBC.commit c
    withTransaction (HDBCConnection c) transaction =
        HDBC.withTransaction c (transaction . HDBCConnection)
    getTables (HDBCConnection c) = HDBC.getTables c
    catchAll (HDBCConnection _) act handler =
        act `catches` [ Handler (\(_ :: HDBC.SqlError) -> handler) ]

loadTests :: IO [Test]
loadTests = do

  sqliteConn <- connectSqlite3 ":memory:"
  let backendConnection :: SQLiteBackendConnection = HDBCConnection sqliteConn
      testAct = (BackendTest.tests backendConnection)
                `finally`
                (disconnect sqliteConn)
  return [ ("SQLite backend tests") ~: test testAct ]

main :: IO ()
main = do
  tests_ <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests_
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
