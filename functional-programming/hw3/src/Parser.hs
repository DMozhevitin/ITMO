module Parser(Command(..), options) where

import Options.Applicative

data Command
  = Help
  | Cd String
  | Pwd
  | Dir
  | Mkdir String
  | Touch String
  | Cat String
  | Rm String
  | Rmdir String
  | Find String
  | WriteFile String [String]
  | Ls String
  | Stat String
  | Exit
  deriving (Show, Eq)


options :: ParserInfo Command
options = info (commandParser <**> helper) idm

commandParser :: Parser Command
commandParser = subparser
  (  command "help" (info (pure Help) (progDesc "Usage guide"))
  <> command
       "cd"
       (info (Cd <$> argument str (metavar "DIRECTORY"))
             (progDesc "Go to directory")
       )
  <> command "pwd" (info (pure Pwd) (progDesc "Current directory"))
  <> command "dir" (info (pure Dir) (progDesc "List files in current directory"))
  <> command
        "mkdir"
         (info (Mkdir <$> argument str (metavar "DIRECTORY"))
            (progDesc "Create directory")
       )
  <> command
        "touch"
         (info (Touch <$> argument str (metavar "FILENAME"))
            (progDesc "Create file")
       )
  <> command
        "cat"
         (info (Cat <$> argument str (metavar "FILENAME"))
            (progDesc "Print file content")
       )
  <> command
        "rm"
         (info (Rm <$> argument str (metavar "FILENAME"))
            (progDesc "Remove file")
       )
  <> command
        "rmdir"
         (info (Rmdir <$> argument str (metavar "DIRECTORY"))
            (progDesc "Remove directory")
       )
  <> command
        "find"
         (info (Find <$> argument str (metavar "FILENAME"))
            (progDesc "Find file")
       )
  <> command
        "writeFile"
         (info (WriteFile <$> argument str (metavar "FILENAME") <*> (many (argument str (metavar "CONTENT"))))
            (progDesc "Write content to a file")
       )
  <> command
        "ls"
         (info (Ls <$> argument str (metavar "DIRECTORY"))
            (progDesc "Get info about directory content")
       )
  <> command
        "stat"
         (info (Stat <$> argument str (metavar "FILENAME"))
            (progDesc "Get info about file")
       )
  <> command "exit" (info (pure Exit) (progDesc "Usage guide"))
  )







