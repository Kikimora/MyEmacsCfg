{-
tbl ZTOKEN 
  ZTOKENNAME -- contains documentation token name
  ZPAERNTNODE -- contain reference to ZNODE.Z_PK
  ZMETAINFORMATION -- reference to ZMETAINFORMATION.Z_PK

tbl ZNODE
  ZKPATH -- path to folder with html describing given class

tbl ZTOKENMETAINFORMATION
  ZANCHOR -- anchor to exact location where token described
 -}

import Database.HDBC.Sqlite3
import Database.HDBC
import System.IO
import Control.Monad
import Control.Exception
import Data.List
import Data.Char
import Text.Regex.Posix
import System.FilePath.Posix
import System( getArgs )

type TokenName = String
type MainHelpFile = IO FilePath
type TokenAnchor = String
type TokenInfo = (TokenName, MainHelpFile, TokenAnchor)

selectTokenDocInfo = "select t.ZTOKENNAME, fp.ZPATH, mi.ZANCHOR \
                     \from ZTOKEN t \
                     \inner join ZTOKENMETAINFORMATION mi on t.ZMETAINFORMATION = mi.Z_PK \
                     \inner join ZFILEPATH fp on mi.ZFILE = fp.Z_PK \
                     \where t.ZTOKENNAME like ? \
                     \order by t.ZTOKENNAME ASC \
                     \limit 100"

searchFirstLine :: (String -> Bool) -> FilePath ->  IO (Maybe String)
searchFirstLine pred path =
    bracket     (openFile path ReadMode)
                (hClose)
                (findFirstLineMatching pred)
    where findFirstLineMatching pred handle = do
            content <- (hGetContents handle)
            let contentLines = lines content
            case (dropWhile (not . pred . (map toLower)) contentLines) of
              (x:xs) -> return (Just x)
              _ -> return (Nothing)

_1 :: (String, String, String, [String]) -> String
_1 (_, _, _, d) = head d
                   
toTokenInfo :: FilePath -> [SqlValue] -> TokenInfo
toTokenInfo baseDir ((SqlString tokenName):(SqlString path):anchor:[]) =
    let anchor' = case anchor of
                    SqlString a -> a
                    SqlNull -> ""
    in (tokenName, resolveMetaRedirect path, anchor')         
    where resolveMetaRedirect path = do
            let docFile = baseDir </> path
            line <- searchFirstLine (isInfixOf "=\"refresh\"") docFile
            let match = case line of
                          Just string -> ("file://" ++ baseDir </> (takeDirectory path) </> filePath)
                                         where filePath = _1 (string =~ "URL=(.*\\.html)")
                          Nothing -> docFile
            return match
              
toTokenInfo _ _ = error "Incorrect row in results"

renderTokenInfo :: TokenInfo -> IO String
renderTokenInfo (token, docFile, anchor) = do
  docFilePath <- docFile
  let result = concat [token, "|", docFilePath, "#", anchor]
  return result

mainHelpFile :: TokenInfo -> IO String
mainHelpFile (_, docFile, anchor) = do
  docFilePath <- docFile
  return $ "file://" ++ docFilePath ++ ('#':anchor)

tokenName :: TokenInfo -> IO String
tokenName (name, _, _) = return name

main = do
  args <- getArgs
  let (op, token, db) = (head args, head (tail args), head ((tail . tail) args))
  conn <- connectSqlite3 db
  let baseDir = (takeDirectory db) </> "Documents"
  rows <- quickQuery' conn selectTokenDocInfo [SqlString (token ++ "%")]
  let result = case op of
                 "complete" -> (map (tokenName . toTokenInfo baseDir) rows)
                 "docurl" -> [mainHelpFile (toTokenInfo baseDir (head rows))]
  mapM (>>= putStrLn) result