import System.IO
import System.Environment
import System.Directory
import Data.Digest.Pure.SHA
import Data.ByteString.Lazy.Char8 (pack)
import Data.List
import Data.List.Split -- splitOn "|" "abc|def"
import Data.String.Utils
import Control.Exception

commands :: [(String, [String])]
commands = [("r", ["-t", "--task-dir", "task_dir"]),
            ("r", ["-l", "--list"    , "list"]),
            (" ", ["-f", "--finish"  , "finish"]), 
            (" ", ["-e", "--edit"    , "edit"]),
            (" ", ["-r", "--remove"  , "remove"])]

main :: IO ()
main = do 
    args <- getArgs
    checkCommands args

argsOK :: [String] -> Bool
argsOK args = and [e | (r,[mc, lc, c] ) <- commands, r == "r" , let e = ( mc `elem` args ) || ( lc `elem` args) ]

getCommand :: [String] -> ([String], Bool)
getCommand args =  head [ ([c, mc, lc], mc `elem` args)  | (r, [mc, lc, c]) <- commands, r /= "r", mc `elem` args || lc `elem` args ]  

getValue :: String -> [String] -> [String]
getValue cmd args = drop (no $ elemIndex cmd args) args

getFile :: [String] -> String
getFile args = dir ++ "\\" ++ file 
    where dir = args !! post 
          file = args !! posf 
          post = ix ("--task-dir", "-t")
          posf = ix ("--list", "-l")
          ix (a,b) | a `elem` args = no ( elemIndex a args ) + 1
                   | b `elem` args = no ( elemIndex b args) + 1
                   | otherwise = -1

no :: ( Maybe Int ) -> Int
no ( Just a ) = a
no Nothing = -1

execute :: String -> String -> [String] -> IO ()
execute file "finish" value = finishTask file (value !! 1)  
execute file "edit"   value = editTask file ( value !! 1 ) ( value !! 2 ) 
execute file "remove" value = removeTask file (value !! 1)
execute _ _ _ = do
    putStrLn "ERROR!"
    
   
executeCommand :: [String] -> Int -> IO ()
executeCommand args c  
    | c == 1 = do
        let cmd   = ( fst $ getCommand args ) !! 0   
            ps = getNum $ snd (getCommand args)
            value = getValue ((fst $ getCommand args) !! ps) args
            file  = getFile args

        execute file cmd value
    | c == 0 = do 
        let cmd   = getCommand args 
            file  = getFile args
        createTask  file ( join " " $ drop 4 args  ) 
    | otherwise   = putStrLn "Not OK!!!"

getNum :: Bool -> Int 
getNum True  = 1 
getNum False = 2

checkOthers :: [String] -> IO ()
checkOthers args = do 
    let c = countNR args
    executeCommand args c

countNR :: [String] -> Int
countNR args = sum [1 | (r, [mc, lc, c] ) <- commands, r /= "r", mc `elem` args || lc `elem` args ]

checkCommands :: [String] -> IO ()
checkCommands args 
    | argsOK args = checkOthers args
    | otherwise = do putStrLn "Error, provide task dir and list name!"


-- Get Id
getId :: String -> String
getId task = show . sha1 $ pack task

-- Create Task
createTask :: String -> String -> IO () 
createTask file    ""   = getAllTasks file 
createTask file task = appendFile file ( task ++ " | " ++  (getId task)  ++ "\n")
            

--getIDs 
getIDs :: [String] -> [(String, String)]
getIDs tasks = [ (last sp, unwords $ init sp)   | x <- tasks, let sp = splitOn " | " x ]

--getJustIds 
getJustIds :: [(String, String)] -> [String]
getJustIds tasks = [x | (x,y) <- tasks]

--getJustTasks 
getJustTasks :: [(String, String)] -> [String]
getJustTasks tasks = [y | (x,y) <- tasks]

--minimize 
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
                                [b !! 0]

--minimizeIds 
minimizeIds :: [String] -> [String]
minimizeIds x = minimize "" x 

--sortTasksById
sortTasksById :: [(String, String)] -> [(String, String)]
sortTasksById []     = []
sortTasksById (x:xs) = sortTasksById [(a, b) | (a,b) <- xs , a <= ( fst x ) ] 
                    ++ [x] 
                    ++ sortTasksById [(a, b) | (a,b) <- xs, a > (fst x)]
--getAllTasks
getAllTasks file = do
    contents <- readFile file 
    let todoTasks = lines contents
        tdIds     = getIDs todoTasks
        tdOrd     = sortTasksById tdIds
        tdJustIds = getJustIds tdOrd
        tdMinIds  = minimizeIds tdJustIds
        tdJustTsk = getJustTasks tdOrd
        tasks = zipWith (\id value ->  id ++ " | " ++ value)
                        tdMinIds tdJustTsk
    mapM_ putStrLn tasks

--getTask 
getTask :: String -> [(String, String)] -> (String, String)
getTask id tasks = head [(x, y) | (x, y) <- tasks, x == id]

-- Get Task Position
getPos :: String -> [(String, String)] -> Int
getPos id list 
    | s >= ( length list ) = 0
    | ( exist id list )  = s
    | otherwise = (-1) 
    where s = sum [ 1  | (x, y) <- list, x /= id, x < id ]
          exist i l= or [ True | (x, y) <- l , x == i ]

-- Remove task  
removeTask :: String -> String -> IO ()
removeTask file id = do 
    contents <- readFile file 
    let todoTasks = lines contents 
        tdIds = getIDs todoTasks
        tdOrd = sortTasksById tdIds
        tdJustIds = getJustIds tdOrd
        tdMinIds = minimizeIds tdJustIds
        tdJustTsk = getJustTasks tdOrd
        id_ts = zipWith (\id value -> (id, value))
                        tdMinIds tdJustTsk
        tasks = zipWith (\id value -> id  ++ " - " ++ value)
                        tdMinIds tdJustTsk
    
    let newTasks = delete (getTask id id_ts) id_ts
        tdJI = getJustIds newTasks
        tdJT = getJustTasks newTasks
        nt   = zipWith (\id value -> value ++ " | " ++ id)
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



editTask :: String -> String -> String -> IO ()
editTask file id task = do 
    contents <- readFile file 
    let todoTasks = lines contents
        tdIds = getIDs todoTasks 
        tdOrd = sortTasksById tdIds 
        tdJustIds = getJustIds tdOrd 
        tdMinIds = minimizeIds tdJustIds
        tdJustTsk = getJustTasks tdOrd
        fid_ts = zipWith (\id value -> (id, value))
                    tdJustIds tdJustTsk
        mid_ts = zipWith (\id value -> (id, value))
                    tdMinIds tdJustTsk

--        old = getTask id mid_ts
        pos = getPos (fst (getTask id mid_ts) ) mid_ts
        new = ( (fst ( fid_ts !! pos ) ), task)  
        newTasks = new : ( delete (fid_ts !! pos) fid_ts )
        tdJI = getJustIds newTasks 
        tdJT = getJustTasks newTasks 
        nt = zipWith (\id value -> value ++ " | " ++ id)
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

-- Finish task [NW]
finishTask :: String -> String -> IO ()
finishTask file id = do 
    contents <- readFile file
    let todoTasks = lines contents 
        tdIds = getIDs todoTasks
        tdOrd = sortTasksById tdIds
        tdJustIds = getJustIds tdOrd
        tdMinIds = minimizeIds tdJustIds
        tdJustTsk = getJustTasks tdOrd
        mid_ts = zipWith (\id value -> (id, value))
                    tdMinIds tdJustTsk
        fid_ts = zipWith (\id value -> (id, value))
                    tdJustIds tdJustTsk
        pos = getPos id mid_ts 
        nid = fst (fid_ts !! pos )
        tsk = snd (fid_ts !! pos )
        dnFN = file ++ ".done" 
    appendFile dnFN (tsk ++ " | " ++ nid ++ "\n") 
    removeTask file id



-- Grep tasks 
--grepTask :: String -> String -> IO ()

-- List Done tasks
--listDone :: String -> IO ()



