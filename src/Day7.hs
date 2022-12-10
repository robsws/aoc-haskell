module Day7 (part1, part2) where

import Data.Maybe (fromJust)

data FileSystemNode = 
    Dir{
        name :: String,
        children :: [FileSystemNode]
    } | 
    File{
        name :: String,
        size :: Int
    }
    deriving (Eq)

instance Ord FileSystemNode where
    -- Define ordering based on size
    compare a b = compare (fileSystemNodeSize a) (fileSystemNodeSize b)

type FileSystemPath = [String]

data CommandLineState =
    CommandLineState{
        path :: FileSystemPath,
        fileSystem :: FileSystemNode
    }

totalDirSizesFilteredBy :: (FileSystemNode -> Bool) -> FileSystemNode -> Int
-- Add up all of the directory sizes in the file system that meet a certain
-- given condition.
totalDirSizesFilteredBy _ File{name=_,size=_} = 0
totalDirSizesFilteredBy filterFunc dir =
    let total = sum $ map (totalDirSizesFilteredBy filterFunc) (children dir) in
        if filterFunc dir then (fileSystemNodeSize dir) + total else total

fileSystemNodeSize :: FileSystemNode -> Int
-- Get the total size of a particular node in the file system,
-- recursively including sizes of children.
fileSystemNodeSize File{name=_,size=s} = s
fileSystemNodeSize Dir{name=_,children=lChildren} =
    sum $ map fileSystemNodeSize lChildren

parseStdOut :: [String] -> CommandLineState -> CommandLineState
-- Parse the entire stdout listing given and return the file system it represents
parseStdOut [] clState = clState
parseStdOut (hLine:tLines) clState
    -- Process a command and update the state
    | argv!!0 == "$" =
        let clState' = doCommand (tail argv) tLines clState in
            parseStdOut tLines clState'
    -- Skip non-command lines
    | otherwise = parseStdOut tLines clState
    where argv = words hLine

doCommand :: [String] -> [String] -> CommandLineState -> CommandLineState
-- Execute a command given in the stdout listing and update the state.
doCommand command lLines clState
    | commandName == "cd" =
        let dir = command!!1 in
            doCd dir clState
    | commandName == "ls" =
        doLs lLines clState
    | otherwise = error ("Unsupported command " ++ commandName)
    where commandName = command!!0

doCd :: String -> CommandLineState -> CommandLineState
doCd dirName CommandLineState{path=currentPath,fileSystem=currentFileSystem}
    -- Go back to the top level directory
    | dirName == "/" =
        CommandLineState{path=[],fileSystem=currentFileSystem}
    -- Go up one directory
    | dirName == ".." =
        let parentPath = tail currentPath in
            CommandLineState{path=parentPath,fileSystem=currentFileSystem}
    -- Go into a named child directory (should have been added by previous ls command)
    | otherwise =
        let childPath = dirName : currentPath in
            CommandLineState{path=childPath,fileSystem=currentFileSystem}

applyToNamedNode :: String -> [FileSystemNode] -> (FileSystemNode -> FileSystemNode) -> [FileSystemNode]
-- Apply the given function to the node in the list that has the given name
-- and return the list of nodes including the transformed node.
applyToNamedNode nodeName [] _ = error ("Could not find node named" ++ nodeName)
applyToNamedNode nodeName (hNode:tNodes) transform
    | name hNode == nodeName =
        (transform hNode) : tNodes
    | otherwise =
        hNode : (applyToNamedNode nodeName tNodes transform)

addNode :: FileSystemPath -> FileSystemNode -> FileSystemNode -> FileSystemNode
-- Add the given node at the position given in the file system.
addNode _ _ File{name=_,size=_} = error ("Cannot add node to a file.")
addNode [] newNode inFs =
    -- We arrived at the directory where we want to add the node.
    Dir{name=(name inFs),children=newNode:(children inFs)}
addNode atPath newNode inFs =
    -- Still need to find the directory where the node should be added.
    let topDirName = last atPath
        remainingPath = init atPath 
        childNodes = children inFs in
            let childNodes' = applyToNamedNode topDirName childNodes (addNode remainingPath newNode) in
                Dir{name=(name inFs),children=childNodes'}

doLs :: [String] -> CommandLineState -> CommandLineState
doLs [] clState = clState
doLs (hLine:tLines) clState@CommandLineState{path=currentPath,fileSystem=fs}
    | typeOrSize == "dir" =
        -- Add new directory node and recurse on the rest of the output
        let fs' = addNode currentPath Dir{name=nodeName,children=[]} fs
            in doLs tLines CommandLineState{path=currentPath,fileSystem=fs'}
    | typeOrSize == "$" = clState -- Prompt means we hit the end of ls output
    | otherwise =
        -- Add new file node and recurse on the rest of the output
        let fs' = addNode currentPath File{name=nodeName,size=(read typeOrSize)} fs
            in doLs tLines CommandLineState{path=currentPath,fileSystem=fs'}
    where nodeInfo = words hLine
          typeOrSize = nodeInfo!!0
          nodeName = nodeInfo!!1

getDirToDelete :: Int -> FileSystemNode -> Maybe FileSystemNode
-- Find the smallest directory that, if deleted, would free up
-- enough space in the total file system to run an update
-- (30 million bytes). The file system has 70 million bytes available
-- in total.
getDirToDelete _ File{name=_,size=_} = Nothing
getDirToDelete targetSize fs 
    | dirSize > targetSize =
        let childCandidates = map fromJust . filter (/=Nothing) . map (getDirToDelete targetSize) $ children fs
            in if childCandidates == [] then Just fs else Just (minimum childCandidates)
    | otherwise = Nothing
    where dirSize = fileSystemNodeSize fs

part1 :: [String] -> String
part1 inputs =
    let emptyDir = Dir{name="/",children=[]}
        emptyFs = CommandLineState{path=[],fileSystem=emptyDir}
        fs = fileSystem $ parseStdOut inputs emptyFs
        filterCondition = (\f -> (fileSystemNodeSize f) < 100000)
            in show $ totalDirSizesFilteredBy filterCondition fs

part2 :: [String] -> String
part2 inputs =
    let emptyDir = Dir{name="/",children=[]}
        emptyFs = CommandLineState{path=[],fileSystem=emptyDir}
        fs = fileSystem $ parseStdOut inputs emptyFs
        targetSize = 30000000 - (70000000 - fileSystemNodeSize fs)
            in show . fileSystemNodeSize . fromJust . getDirToDelete targetSize $ fs