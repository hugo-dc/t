import System.IO
import System.Environment
import System.Directory
import Data.Digest.Pure.SHA
import Data.ByteString.Lazy.Char8 (pack)
import Data.List
import Data.List.Split -- splitOn "|" "abc|def"
import Control.Exception

-----------------------------------------------------------------
-- Available Commands
-- commands
-----------------------------------------------------------------
commands :: [(String, [String])]
commands = [("r", ["-t", "--task-dir", "task_dir"]),
            ("r", ["-l", "--list"    , "list"]),
            (" ", ["-f", "--finish"  , "finish"]), 
            (" ", ["-e", "--edit"    , "edit"]),
            (" ", ["-r", "--remove"  , "remove"])]


-----------------------------------------------------------------
-- Main
-----------------------------------------------------------------
main :: IO ()
main = do 
    args <- getArgs
    checkCommands args


-----------------------------------------------------------------
-- Check Commands
-----------------------------------------------------------------
checkCommands :: [String] -> IO ()
checkCommands args 
    | argsOK args = checkOthers args
    | otherwise = putStrLn "Error, provide task dir and list name!"



-----------------------------------------------------------------
-- Check Aarguments
-----------------------------------------------------------------
argsOK :: [String] -> Bool
argsOK args = and [e | (r,[mc, lc, _] ) <- commands, r == "r" , let e = ( mc `elem` args ) || ( lc `elem` args) ]


-----------------------------------------------------------------
-- Check Others
-----------------------------------------------------------------
checkOthers :: [String] -> IO ()
checkOthers args = do 
    let c = countNR args
    executeCommand args c


-----------------------------------------------------------------
-- countNR
-----------------------------------------------------------------
countNR :: [String] -> Int
countNR args = sum [1 | (r, [mc, lc, _] ) <- commands, r /= "r", mc `elem` args || lc `elem` args ]

-----------------------------------------------------------------
-- executeCommand
-----------------------------------------------------------------
executeCommand :: [String] -> Int -> IO ()
executeCommand args c  
    | c == 1 = do
        let cmd   = head ( fst $ getCommand args ) 
            ps = getNum $ snd (getCommand args)
            value = getValue ((fst $ getCommand args) !! ps) args
            file  = getFile args

        execute file cmd value
    | c == 0 = do 
        let file  = getFile args
        createTask  file ( intercalate " " $ drop 4 args  ) 
    | otherwise   = putStrLn "Not OK!!!"

-----------------------------------------------------------------
-- getCommand
-----------------------------------------------------------------
getCommand :: [String] -> ([String], Bool)
getCommand args =  head [ ([c, mc, lc], mc `elem` args)  | (r, [mc, lc, c]) <- commands, r /= "r", mc `elem` args || lc `elem` args ]  

-----------------------------------------------------------------
-- getNum
-----------------------------------------------------------------
getNum :: Bool -> Int 
getNum True  = 1 
getNum False = 2

-----------------------------------------------------------------
-- getValue
-----------------------------------------------------------------
getValue :: String -> [String] -> [String]
getValue cmd args = drop (no $ elemIndex cmd args) args

-----------------------------------------------------------------
-- getFile
-----------------------------------------------------------------
getFile :: [String] -> String
getFile args = dir ++ "\\" ++ file 
    where dir = args !! post 
          file = args !! posf 
          post = ix ("--task-dir", "-t")
          posf = ix ("--list", "-l")
          ix (a,b) | a `elem` args = no ( elemIndex a args ) + 1
                   | b `elem` args = no ( elemIndex b args) + 1
                   | otherwise = -1

-----------------------------------------------------------------
-- execute
-----------------------------------------------------------------
execute :: String -> String -> [String] -> IO ()
execute file "finish" value = finishTask file (value !! 1)  
execute file "edit"   value = editTask file ( value !! 1 ) ( intercalate " " $ drop 2 value ) 
execute file "remove" value = removeTask file (value !! 1)
execute _ _ _ = putStrLn "ERROR!"


-----------------------------------------------------------------
-- no
-----------------------------------------------------------------
no :: ( Maybe Int ) -> Int
no ( Just a ) = a
no Nothing = -1

-----------------------------------------------------------------
-- Get Id
-----------------------------------------------------------------
getId :: String -> String
getId task = show . sha1 $ pack task

-----------------------------------------------------------------
-- Create Task
-----------------------------------------------------------------
createTask :: String -> String -> IO () 
createTask file    ""   = getAllTasks file 
createTask file task = appendFile file ( task ++ " | " ++  (getId task)  ++ "\n")
            

-----------------------------------------------------------------
--getIDs 
-----------------------------------------------------------------
getIDs :: [String] -> [(String, String)]
getIDs tasks = [ (last sp, unwords $ init sp)   | x <- tasks, let sp = splitOn " | " x ]

-----------------------------------------------------------------
--getJustIds 
-----------------------------------------------------------------
getJustIds :: [(String, String)] -> [String]
getJustIds tasks = [x | (x,_) <- tasks]

-----------------------------------------------------------------
--getJustTasks 
-----------------------------------------------------------------
getJustTasks :: [(String, String)] -> [String]
getJustTasks tasks = [y | (_,y) <- tasks]

-----------------------------------------------------------------
--minimize 
-----------------------------------------------------------------
minimize :: String -> [String] -> [String]
minimize  _ []      = []
minimize "" (x:xs)  = [x !! 0 ] : minimize [ x !! 0 ] xs 
minimize acc (x:xs) = ( cmp acc x 0 ) : minimize (cmp acc x 0) xs 
    where cmp a b c
            | length a < ( c + 1 ) = [b !! c] 
            | otherwise = if a !! c == b !! c 
                            then 
                               ( a !! c ):(cmp a b (c+1)) 
                            else 
                                [b !! c]

-----------------------------------------------------------------
--minimizeIds 
-----------------------------------------------------------------
minimizeIds :: [String] -> [String]
minimizeIds = minimize ""  

-----------------------------------------------------------------
--sortTasksById
-----------------------------------------------------------------
sortTasksById :: [(String, String)] -> [(String, String)]
sortTasksById []     = []
sortTasksById (x:xs) = sortTasksById [(a, b) | (a,b) <- xs , a <= ( fst x ) ] 
                    ++ [x] 
                    ++ sortTasksById [(a, b) | (a,b) <- xs, a > (fst x)]

getMaxLength :: [String] -> Int
getMaxLength ids = maximum ls
    where ls = map length ids

-----------------------------------------------------------------
--getAllTasks
-----------------------------------------------------------------
getAllTasks :: String -> IO ()
getAllTasks file = do
    fileExist <- doesFileExist file
    if not fileExist 
    then writeFile file ""
    else return ()
    contents <- readFile file 
    let todoTasks = lines contents
        tdIds     = getIDs todoTasks
        tdOrd     = sortTasksById tdIds
        tdJustIds = getJustIds tdOrd
        tdMinIds  = minimizeIds tdJustIds
        mx        = getMaxLength tdMinIds
        tdJustTsk = getJustTasks tdOrd
        tasks = zipWith (\tid value ->  tid ++ ( take ( mx - (length tid) ) $ repeat ' ') ++ " | " ++ value)
                        tdMinIds tdJustTsk
    mapM_ putStrLn tasks

-----------------------------------------------------------------
--getTask 
-----------------------------------------------------------------
getTask :: String -> [(String, String)] -> (String, String)
getTask tid tasks = head [(x, y) | (x, y) <- tasks, x == tid]

-----------------------------------------------------------------
-- Get Task Position
-----------------------------------------------------------------
getPos :: String -> [(String, String)] -> Int
getPos tid list 
    | s >= ( length list ) = 0
    | ( exist tid list )  = s
    | otherwise = (-1) 
    where s = sum [ 1  | (x, _) <- list, x /= tid, x < tid ]
          exist i l= or [ True | (x, _) <- l , x == i ]

-----------------------------------------------------------------
-- Remove task  
-----------------------------------------------------------------
removeTask :: String -> String -> IO ()
removeTask file tid = do 
    contents <- readFile file 
    let todoTasks = lines contents 
        tdIds = getIDs todoTasks
        tdOrd = sortTasksById tdIds
        tdJustIds = getJustIds tdOrd
        tdMinIds = minimizeIds tdJustIds
        tdJustTsk = getJustTasks tdOrd
        fid_ts = zipWith (\i value -> (i, value))
                        tdJustIds tdJustTsk
        mid_ts = zipWith (\i value -> (i, value))
                        tdMinIds tdJustTsk
    
        pos = getPos (fst (getTask tid mid_ts)) mid_ts
        newTasks = delete (fid_ts !! pos ) fid_ts
        tdJI = getJustIds newTasks
        tdJT = getJustTasks newTasks
        nt   = zipWith (\i value -> value ++ " | " ++ i)
                        tdJI tdJT
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do 
            hClose tempHandle 
            removeFile tempName)
        (\(tempName, tempHandle) -> do 
            hPutStr tempHandle $ unlines nt 
            hClose tempHandle 
            removeFile file 
            renameFile tempName file)



-----------------------------------------------------------------
-- editTask
-----------------------------------------------------------------
editTask :: String -> String -> String -> IO ()
editTask file tid task = do 
    contents <- readFile file 
    let todoTasks = lines contents
        tdIds = getIDs todoTasks 
        tdOrd = sortTasksById tdIds 
        tdJustIds = getJustIds tdOrd 
        tdMinIds = minimizeIds tdJustIds
        tdJustTsk = getJustTasks tdOrd
        fid_ts = zipWith (\i value -> (i, value))
                    tdJustIds tdJustTsk
        mid_ts = zipWith (\i value -> (i, value))
                    tdMinIds tdJustTsk

        pos = getPos (fst (getTask tid mid_ts) ) mid_ts
        new = ( (fst ( fid_ts !! pos ) ), task)  
        newTasks = new : ( delete (fid_ts !! pos) fid_ts )
        tdJI = getJustIds newTasks 
        tdJT = getJustTasks newTasks 
        nt = zipWith (\i value -> value ++ " | " ++ i)
                     tdJI tdJT 

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do 
            hClose tempHandle 
            removeFile tempName)
        (\(tempName, tempHandle) -> do 
            hPutStr tempHandle $ unlines nt 
            hClose tempHandle 
            removeFile file 
            renameFile tempName file)

-----------------------------------------------------------------
-- Finish task [NW]
-----------------------------------------------------------------
finishTask :: String -> String -> IO ()
finishTask file tid = do 
    contents <- readFile file
    let todoTasks = lines contents 
        tdIds = getIDs todoTasks
        tdOrd = sortTasksById tdIds
        tdJustIds = getJustIds tdOrd
        tdMinIds = minimizeIds tdJustIds
        tdJustTsk = getJustTasks tdOrd
        mid_ts = zipWith (\i value -> (i, value))
                    tdMinIds tdJustTsk
        fid_ts = zipWith (\i value -> (i, value))
                    tdJustIds tdJustTsk
        pos = getPos tid mid_ts 
        nid = fst (fid_ts !! pos )
        tsk = snd (fid_ts !! pos )
        dnFN = file ++ ".done" 
    appendFile dnFN (tsk ++ " | " ++ nid ++ "\n") 
    removeTask file tid



-----------------------------------------------------------------
-- Grep tasks 
-----------------------------------------------------------------
--grepTask :: String -> String -> IO ()

-----------------------------------------------------------------
-- List Done tasks
-----------------------------------------------------------------
--listDone :: String -> IO ()



