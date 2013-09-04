--Creates a new git repo in the current directory and
--adds an empty README.md, barebones .gitignore, and
--a copy of the MIT license, then commits them
import System.Directory (getCurrentDirectory)
import System.Environment(getArgs)
import System.FilePath.Posix(splitPath)
import System.Process(runCommand,waitForProcess)
import Data.List(last,break,length,replicate)
import Data.Time(getCurrentTime,utctDay,toGregorian)

main = do
    args       <- getArgs
    currentDir <- getCurrentDirectory
    date       <- getCurrentTime
    writeFiles currentDir date
    runCommand ("git init " ++ concat args)     >>= waitForProcess
    runCommand "git add ."                      >>= waitForProcess
    runCommand "git commit -m 'Initial commit'" >>= waitForProcess

  where
    writeFiles dir date = 
        do writeFile "LICENSE" (createLicense date dir)
           writeFile "README.md" "This is an empty readme"
           writeFile ".gitignore" gitIgnore


createLicense date cs = name ++ "\n" ++ dots ++ "\n" ++
    "Author: Leif Grele\n" ++ "Copyright (c) Leif Grele " ++
    show year ++ "\n\n" ++ mit
   where
        name       = last (splitPath cs)
        dots       = replicate (length cs) '-'
        (year,_,_) = toGregorian (utctDay date)


mit = "License Agreement (The MIT License)\n\n\
\Permission is hereby granted, free of charge, to any person obtaining a copy\n\
\of this software and associated documentation files (the \"Software\"), to deal\n\
\in the Software without restriction, including without limitation the rights\n\
\to use, copy, modify, merge, publish, distribute, sublicense, and/or sell\n\
\copies of the Software, and to permit persons to whom the Software is\n\
\furnished to do so, subject to the following conditions:\n\n\

\The above copyright notice and this permission notice shall be included in\n\
\all copies or substantial portions of the Software.\n\n\

\THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\n\
\IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\n\
\FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE\n\
\AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\n\
\LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,\n\
\OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN\n\
\THE SOFTWARE."

gitIgnore = "#ignore builds\n\
            \build/*\n\n\
            \#ignore sublime project files\n\
            \*.sublime-project\n\
            \*.sublime-workspace\n\n\
            \#ignore objects and archives\n\
            \*.[oa]"