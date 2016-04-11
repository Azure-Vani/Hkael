import Control.Monad.Reader
import Data.String.Utils (replace)
import Data.Sexp

import Text.Groom

import Language.Haskell.Exts
import Language.Sexp.Parser
import Language.Sexp.Printer
import qualified Data.ByteString.Lazy.Char8 as BS (concat, putStrLn, intercalate, append, pack, ByteString)

indentation = BS.pack "    "
space = BS.pack " "
newLine = BS.pack "\n"
empty = BS.pack ""

allAtom :: [Sexp] -> Bool
allAtom [] = True
allAtom (Atom _ : xs) = allAtom xs
allAtom (Data.Sexp.List _ : _) = False

pp :: Sexp -> Reader BS.ByteString BS.ByteString
pp (Data.Sexp.List sexps) =
    if allAtom sexps then do 
        currentIdent <- ask
        return $ BS.append (BS.append currentIdent $ BS.intercalate space $ map (\(Atom x) -> x) sexps) newLine
    else do
        xs <- mapM (local (BS.append indentation) . pp) sexps
        return $ BS.concat xs

pp (Atom str) = do
    currentIdent <- ask
    return $ BS.append (BS.append currentIdent str) newLine

toString :: Sexp -> BS.ByteString
toString x = runReader (pp x) empty

main = do
    a <- parseFile "test.hs"
    putStrLn $ groom $ fromParseResult a
