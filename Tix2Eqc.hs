{-# LANGUAGE StandaloneDeriving #-}
import System.Environment
import System.Process
import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Util
import Data.List
import Data.Function
import Data.Maybe
import Data.Ord
import Data.Tree
import Control.Monad

deriving instance Ord Mix
instance Ord Hash where
  compare = comparing show

usort :: Ord a => [a] -> [a]
usort = map head . group . sort

partitionBy :: Ord b => (a -> b) -> [a] -> [[a]]
partitionBy f = groupBy ((==) `on` f) . sortBy (comparing f)

partitionBy' :: Ord b => (a -> b) -> [a] -> [(b, [a])]
partitionBy' f =
  map (with (f . head)) . groupBy ((==) `on` f) . sortBy (comparing f)

with :: (a -> b) -> a -> (b, a)
with f x = (f x, x)

string :: String -> String
string x = "\"" ++ x ++ "\""

atom :: String -> String
atom x = "'" ++ x ++ "'"

tuple :: [String] -> String
tuple xs = "{" ++ intercalate "," xs ++ "}"

list :: [String] -> String
list xs = "[" ++ intercalate "," xs ++ "]"

call :: String -> [String] -> String
call f xs = f ++ "(" ++ intercalate "," xs ++ ")"

fun :: String -> [String] -> String -> String
fun f xs body = call f xs ++ " -> " ++ body

decl :: String -> String
decl x = x ++ "."

data Loc = Loc { locFile :: FilePath, locHash :: Hash, locPos :: HpcPos }
  deriving (Eq, Ord, Show)

showPos :: HpcPos -> String
showPos pos =
  tuple [tuple [show x1, show y1], tuple [show x2, show y2]]
  where
    (x1, y1, x2, y2) = fromHpcPos pos

showLoc :: Loc -> String
showLoc (Loc _ hash pos) =
  tuple [show hash, showPos pos]

makeLoc :: Mix -> HpcPos -> Loc
makeLoc (Mix file _ hash _ _) pos = Loc file hash pos

data Tick = Tick { tickLoc :: Loc, tickLabels :: [String], tickCount :: Integer }
  deriving (Eq, Ord, Show)

makeTick :: Mix -> MixEntry -> Integer -> Maybe Tick
makeTick mix (pos, ExpBox _) n =
  Just (Tick (makeLoc mix pos) [] n)
makeTick mix (pos, BinBox _ True) n =
  Just (Tick (makeLoc mix pos) ["true"] n)
makeTick mix (pos, BinBox _ False) n =
  Just (Tick (makeLoc mix pos) ["false"] n)
makeTick _ _ _ = Nothing

pruneTicks :: [Tick] -> [Tick]
pruneTicks = concatMap pruneFile . partitionBy (locFile . tickLoc)
  where
    pruneFile =
      concat . concatMap flatten .
      -- Remove children which are identical (excluding labels) to their parents
      map pruneChildren .
      -- If there is a labelled tick, don't include the unlabelled version
      map (fmap pruneNode) .
      -- Make the domination tree
      map (fmap snd) . createMixEntryDom . map (with (locPos . tickLoc))

    pruneNode ticks = filter p ticks
      where
        p tick = all (null . tickLabels) ticks || not (null (tickLabels tick))

    pruneChildren (Node ticks ts) =
      Node ticks (map pruneChildren (concatMap collapse ts))
      where
        n = sum (map tickCount ticks)
        collapse (Node [tick] ts)
          | tickCount tick == n && null (tickLabels tick) = ts
        collapse node = [node]
  
main = do
  tixfile:mixdirs <- getArgs
  Just (Tix mods) <- readTix tixfile
  mixes <- sequence [readMix mixdirs (Right mod) | mod <- mods]
  let
    allTicks =
      pruneTicks $ catMaybes [
        makeTick mix entry n
        | (mix@(Mix _ _ _ _ entries), mod) <- zip mixes mods,
          (entry, n) <- zip entries (tixModuleTixs mod) ]
    allLocs = usort (map tickLoc allTicks)
    allFiles = usort (map locFile allLocs)

  writeFile "tix2eqc_data.erl" $ unlines [
    decl (call "-module" ["tix2eqc_data"]),
    decl (call "-compile" ["export_all"]),
    decl (fun (atom "__eqc_cover_files") []
      (list (map string allFiles))),
    decl . fun (atom "__eqc_cover_ranges") [] $
      list [tuple [
        string file,
        list
          [ tuple [list [showPos (locPos loc)], showLoc loc]
          | loc <- locs ]]
        | (file, locs) <- partitionBy' locFile allLocs ],
    decl . fun "data" [] $
      list [tuple [
        "tix2eqc_data",
        list [
          tuple [
            showLoc loc,
            list [
              tuple [list (map string (tickLabels tick)), show (tickCount tick)]
              | tick <- ticks ]]
          | (loc, ticks) <- partitionBy' tickLoc allTicks ]]]]
          
  system "erl -eval 'compile:file(tix2eqc_data)' -eval 'eqc_cover:write_html(tix2eqc_data:data(), [])' -eval 'erlang:halt()'"
  where
