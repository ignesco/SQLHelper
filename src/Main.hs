module Main where

import System.IO

data SQLPattern = AddColumn {
        addColTableName :: String
        ,addColName :: String
        ,addColType :: String
        ,addColTypeSub :: Maybe String
        ,addColIsNonNul :: Bool
    }

    | RenameColumn {
        renColTableName :: String
        , renColOldColName :: String
        , renColNewColName :: String
    }
        
    | DropCons {
        dpkTableName :: String
        , dpkName :: String
    }
            
    | CreateUniqueIndex {
        cixName :: String
        , cixTableName :: String
        , cixCommaName :: String
    }

    | CreatePK {
        pkTabName :: String
        , pkName :: String
        , pkCommaName :: String
    }

instance Show SQLPattern where
    show (AddColumn tabName colName typeName sub' nonNull') =
        let
            sub = maybe "" (\s -> "("++s++")") sub'
            nonNull = if nonNull' then " NON NULL" else ""
        in concat ["ALTER TABLE [", tabName, "] ADD [", colName, "] ", typeName, sub, nonNull, ";"]

    show (RenameColumn tabName oldColName newColName) =
        concat ["EXECUTE sp_rename N'dbo.[", tabName, "].[", oldColName, "]', N'" ++ newColName  ++ "', 'COLUMN';" ]

    show (DropCons tabName conName) =
        concat ["ALTER TABLE [", tabName, "] DROP CONSTRAINT [", conName,"];"]

    show (CreateUniqueIndex ixName tabName colList) =
        concat ["CREATE UNIQUE INDEX [", ixName, "] ON [", tabName, "] (", colList, ");"]

    show (CreatePK tabName constraintName colList) =
        concat ["ALTER TABLE [", tabName, "] ADD CONSTRAINT [", constraintName, "] PRIMARY KEY (", colList ,");"]

data Option a = Option String (IO SQLPattern)

putStrx :: String -> IO ()
putStrx s = do
    putStr s
    hFlush stdout

printOptions :: [Option a] -> IO ()
printOptions opts' = do
    let opts = zip [1..] opts'
    putStrLn "0) exit"
    mapM_ (\(n, (Option s _)) -> putStrLn ( show n ++ ") " ++ s )) opts

processOption :: [Option a] -> String -> IO () -> IO ()
processOption opts opt cc =
    case opt of
        "0" -> return ()
        s -> do
            let opt'@(Option s act) = opts !! ((read opt) - 1 )
            sql <- act
            putStrLn ""
            putStrLn "vvvvvvvvvvvvvvvvvvvvvvvvvv"
            putStrLn $ show sql
            putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^"
            putStrLn ""
            cc

strToMaybeStr :: String -> Maybe String
strToMaybeStr "" = Nothing
strToMaybeStr s = Just s

ynStrToBool :: String -> Bool
ynStrToBool "y" = True
ynStrToBool "Y" = True
ynStrToBool _ = False

renCol :: IO SQLPattern
renCol = do
    putStrx $ "table name: "
    p1 <- getLine :: IO String
        
    putStrx $ "old col name: "
    p2 <- getLine :: IO String
        
    putStrx $ "new col name: "
    p3 <- getLine :: IO String

    return $ RenameColumn p1 p2 p3

addCol :: IO SQLPattern
addCol = do
    putStrx $ "table name: "
    p1 <- getLine :: IO String
        
    putStrx $ "new col name: "
    p2 <- getLine :: IO String
        
    putStrx $ "new col type: "
    p3 <- getLine :: IO String
        
    putStrx $ "new col sub type: "
    p4' <- getLine :: IO String
    let p4 = strToMaybeStr p4'

    putStrx $ "new col NonNull? (yY/nN): "
    p5' <- getLine :: IO String
    let p5 = ynStrToBool p5'
        
    return $ AddColumn p1 p2 p3 p4 p5

dropCons = do
    putStrx $ "table name: "
    p1 <- getLine :: IO String
        
    putStrx $ "constraint name: "
    p2 <- getLine :: IO String
        
    return $ DropCons p1 p2

createUIX = do
    putStrx $ "IX name: "
    p1 <- getLine :: IO String
        
    putStrx $ "table name: "
    p2 <- getLine :: IO String
        
    putStrx $ "column list: "
    p3 <- getLine :: IO String

    return $ CreateUniqueIndex p1 p2 p3


addPK = do        
    putStrx $ "Table name: "
    p1 <- getLine :: IO String
        
    putStrx $ "PK name: "
    p2 <- getLine :: IO String
        
    putStrx $ "column list: "
    p3 <- getLine :: IO String
        
    return $ CreatePK p1 p2 p3

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let
        options = [
                Option "Add Column" addCol
                , Option "Rename Column" renCol
                , Option "Drop PK" dropCons
                , Option "Drop IX" dropCons
                , Option "Create UNIQUE IX" createUIX
                , Option "Add PK" addPK

        
            ]
    printOptions options
    l <- getLine :: IO String
    processOption options l main

