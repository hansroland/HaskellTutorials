-- --------------------------------------------------------
-- A first tutorial on opaleye
-- -------------------------------------------------------
--
-- See:
--     https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialBasic.lhs
--     (with some extensions)
-- -------------------------------------------------------

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE OverloadedStrings #-}

module Main where


import Opaleye (Column, Nullable, matchNullable, isNull,
                Table(Table), required, queryTable,
                Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<), (.===),
                showSqlForPostgres, Unpackspec,
                pgString, (.++), ifThenElse,
                PGInt4, PGText, PGDate,
                runQuery)

import Data.Profunctor.Product (p2, p3)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import Control.Arrow (returnA, (<<<))
import Data.Time.Calendar (Day)

import qualified Database.PostgreSQL.Simple as PGS

-- | Little utility function to print generated SQL statements
printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres

-- --------------------------------------------------------
-- Table Definitions
-- --------------------------------------------------------

-- Note: All table names must be in lowercase.
--       We cannot access tables in mixed mode

-- | personTable - a tuple with standard rows
personTable :: Table (Column PGText, Column PGInt4, Column PGText)  -- read
                     (Column PGText, Column PGInt4, Column PGText)  -- write
personTable = Table "persontable" (p3 ( required "name"
                                      , required "age"
                                      , required "address"))

-- ---------------------------------------------------------
-- How to query multiple columns with a Haskell data type
-- --------------------------------------------------------

-- | define haskell type and corresponding columns
--   The polymorphic approach is highly recommended
data Birthday' a b = Birthday { bdName :: a , bdDay :: b }
type Birthday = Birthday' String Day
type BirthdayColumn = Birthday' (Column PGText) (Column PGDate)

-- create intances with Template Haskell
$(makeAdaptorAndInstance "pBirthday" ''Birthday')

-- | bithdayTable - a table corresponding to a Haskell type
birthdayTable :: Table BirthdayColumn BirthdayColumn
birthdayTable = Table "birthdaytable"
                      (pBirthday Birthday { bdName = required "name"
                                          , bdDay = required "birthday"})

-- | employeeTable - A table with nullable fields
employeeTable :: Table (Column PGText, Column (Nullable PGText))
                       (Column PGText, Column (Nullable PGText))
employeeTable = Table "employeetable" (p2 ( required "name"
                                          , required "boss" ))

-- ---------------------------------------------------------------------
-- Queries
-- --------------------------------------------------------------------

-- | a first simple query on the person table
personQuery :: Query (Column PGText, Column PGInt4, Column PGText)
personQuery = queryTable personTable

-- | query the birthday table
birthdayQuery :: Query BirthdayColumn
birthdayQuery = queryTable birthdayTable

-- ---------------------------------------------------------------------------
-- Projections (read only a subset of the available columns)
-- ---------------------------------------------------------------------------
nameAge :: Query (Column PGText, Column PGInt4)
nameAge = proc () -> do
  (name, age, _) <- personQuery -< ()
  returnA -< (name, age)

-- ----------------------------------------------------------------------------
-- Where clauses (Restrictions)
-- ----------------------------------------------------------------------------

-- | where clause with a single condition
--   SELECT name, age, address
--     FROM personTable
--     WHERE age <= 18
youngPeople :: Query (Column PGText, Column PGInt4, Column PGText)
youngPeople = proc () -> do
  row@(_, age, _) <- personQuery -< ()
  restrict -< age .<= 18
  returnA -< row

-- | Use a parameter for the age of the person
--   ghci> printSql $ relativeYoungPeople 20
relativeYoungPeople :: Int ->  Query (Column PGText, Column PGInt4, Column PGText)
relativeYoungPeople limit = proc () -> do
  row@(_, age, _) <- personQuery -< ()
  restrict -< age .<= fromIntegral limit
  returnA -< row

-- | where clause with multiple conditions
twentiesAtAddress :: Query (Column PGText, Column PGInt4, Column PGText)
twentiesAtAddress = proc () -> do
  row@(_, age, address) <- personQuery -< ()

  restrict -< (20 .<= age) .&& (age .< 30)
  restrict -< address .== pgString "1 My Street, My Town"

  returnA -< row

-- -------------------------------------------------------------
-- Inner Join (=Product with Restriction)
-- -------------------------------------------------------------
-- | A simple cartesian product,without any restrictions
personBirthdayProduct :: Query ((Column PGText, Column PGInt4, Column PGText), BirthdayColumn)
personBirthdayProduct = proc () -> do
  personRow <- personQuery -< ()
  birthdayRow <- birthdayQuery -< ()
  returnA -< (personRow, birthdayRow)

-- | A standard Inner Join
personAndBirthday ::
  Query (Column PGText, Column PGInt4, Column PGText, Column PGDate)
personAndBirthday = proc () -> do
  (name, age, address) <- personQuery -< ()
  birthday             <- birthdayQuery -< ()
  restrict -< name .== bdName birthday
  returnA -< (name, age, address, bdDay birthday)

-- -------------------------------------------------
-- Queries with nullable fields
-- --------------------------------------------------
-- | The most common case, list all where a field is null
--   Select name
--   from employeeTable
--   where boss is null
noBossQuery :: Query (Column PGText)
noBossQuery = proc () -> do
  (name, nullableBoss) <- queryTable employeeTable -< ()
  restrict -< isNull nullableBoss
  returnA -< name

-- | A query with sql conactenation of a text depending a null field
--  SELECT name || ' has '
--              || CASE WHEN boss IS NULL THEN 'no' ELSE 'a' END || ' boss'
--     FROM employeeTable
hasBoss :: Query (Column PGText)
hasBoss = proc () -> do
  (name, nullableBoss) <- queryTable employeeTable -< ()
  let aOrNo = ifThenElse (isNull nullableBoss) (pgString "no") (pgString "a")
  returnA -< name .++ pgString " has " .++ aOrNo .++ pgString " boss"
  -- Note that `matchNullable` corresponds to Haskell's
  --      maybe :: b -> (a -> b) -> Maybe a -> b

-- ----------------------------------------------------------------------------
-- Composability - Factor out common parts of a where clause
-- ----------------------------------------------------------------------------
restrictIsTwenties :: QueryArr (Column PGInt4) ()
restrictIsTwenties = proc age -> do
  restrict -< (20 .<= age) .&& (age .< 30)

restrictAddressIs1MyStreet :: QueryArr (Column PGText) ()
restrictAddressIs1MyStreet = proc address -> do
  restrict -< address .== pgString "1 My Street, My Town"

--
twentiesAtAddress' :: Query (Column PGText, Column PGInt4, Column PGText)
twentiesAtAddress' = proc () -> do
  row@(_, age, address) <- personQuery -< ()

  restrictIsTwenties -< age
  restrictAddressIs1MyStreet -< address

  returnA -< row

-- Still features missi


-- --------------------------------------------------------------------------
-- Running Queries on Postgres
-- --------------------------------------------------------------------------
-- runQuery :: Database.PostgreSQL.Simple.Connection
--          -> Query columns -> IO [haskells]


runPersonQuery :: PGS.Connection
                  -- -> Query (Column PGText, Column PGInt4, Column PGText)
                  -> IO [(String, Int, String)]
runPersonQuery conn = runQuery conn personQuery


main :: IO()
main = do
  conn <- PGS.connectPostgreSQL  "user=roland dbname=roland"
  results <- runPersonQuery conn
  mapM_  print results
  return ()
