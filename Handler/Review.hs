{-# LANGUAGE OverloadedStrings, RebindableSyntax, NoImplicitPrelude #-}
module Handler.Review where

import Import hiding (mapM)

import Data.Text (pack, lines, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.List (transpose, groupBy, head)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Function (on)
import Control.Arrow ((&&&))

import qualified Github.Data.Definitions as GH
import Github.Data.Definitions (DetailedPullRequest(..), PullRequestCommit(..), Tree(..), GitTree(..), Blob(..))

import Control.Monad.Trans.Either (EitherT(..), left, right, bimapEitherT, hoistEither)

import Text.Diff.Parse (parseDiff)
import qualified Text.Diff.Parse.Types as D
import Haxl.Core
import Haxl.Prelude

import Github
import Github.DataSource

import Debug.Trace
traceShowId a = Debug.Trace.trace (show a) a

data Hunk = Hunk (Int, Int) (Int, Int) [Text]
    deriving (Eq, Show)

data Context = Context (Maybe Hunk) (Maybe Hunk)
data FileDiffPart = Change [DiffLine] [DiffLine]
                  | InitialContext [ContextLine]
                  | InternalContext [ContextLine]
                  | FinalContext [ContextLine]

data FileDiffTag = Within Hunk
                 | Before
                 | Between Hunk Hunk
                 | After Hunk
    deriving (Eq, Show)

data Error = GithubError GH.Error | ParseError String
    deriving Show

type NumberedLine = (Int, Text)
type DiffLine = (Int, Text)
type ContextLine = (Int, Int, Text)
type Source = [NumberedLine]

context = 3

last n = reverse . take n . reverse

sourceLines :: Source -> Hunk -> [NumberedLine]
sourceLines source (Hunk (lStart, lLength) _ _) = take lLength $ drop (lStart - 1) $ source

destLines :: Hunk -> [NumberedLine]
destLines (Hunk _ (rStart, _) text) = zip [rStart..] text

fillInContext :: Source -> [Hunk] -> [FileDiffPart]
fillInContext source hs = (map (uncurry mkDiffPart) $ groupAList taggedLines) ++ (map mkTrailingHunk remainingHunks)
    where
        (taggedLines, remainingHunks) = lineTags hs source
        mkTrailingHunk (Hunk _ (rStart, _) txt) = Change [] (zip [rStart..] txt)

groupAList :: (Eq a) => [(a, b)] -> [(a, [b])]
groupAList xs = map (fst . head &&& map snd) $ groupBy ((==) `on` fst) xs

mkDiffPart :: FileDiffTag -> [NumberedLine] -> FileDiffPart
mkDiffPart Before ls = InitialContext $ addContext ls [1..]
mkDiffPart (Within (Hunk _ (rStart, _) txt)) ls = Change ls (zip [rStart..] txt)
mkDiffPart (After (Hunk _ (start, length) _)) ls = FinalContext $ addContext ls [start + length..]
mkDiffPart (Between (Hunk _ (start, length) _) _) ls = InternalContext $ addContext ls [start + length..]

addContext :: [NumberedLine] -> [Int] -> [ContextLine]
addContext src destNums = zip3 srcNums destNums srcLines
    where (srcNums, srcLines) = unzip src

-- Must be called with a non-empty sorted list of hunks
lineTags :: [Hunk] -> [NumberedLine] -> ([(FileDiffTag, NumberedLine)], [Hunk])
lineTags hunks lines = go Nothing hunks lines
    where
        go _ hs [] = ([], hs)
        go Nothing [] ls = (zip (repeat Before) ls, [])
        go (Just h) [] ls = (zip (repeat $ After h) ls, [])
        go prev (h@(Hunk (start, len) (_, _) _):hs) (l@(n, t):ls)
            | n < start = let
                l' = (maybe Before (\h' -> Between h h') prev, l)
                (ls', hs') = go prev (h:hs) ls
                in (l':ls', hs')
            | n >= start && n <= start + len = let
                l' = (Within h, l)
                (ls', hs') = go (Just h) (h:hs) ls
                in (l':ls', hs')
            | n > start + len = let
                l' = (maybeHead (After h) (Between h) hs, l)
                (ls', hs') = go (Just h) hs ls
                in (l':ls', hs')

maybeHead def f = maybe def f . listToMaybe

trimContext :: D.Hunk -> [Hunk]
trimContext hunk = trim (D.rangeStartingLineNumber $ D.hunkSourceRange hunk) (D.rangeStartingLineNumber $ D.hunkDestRange hunk) groups
    where
        isContext = (D.Context ==) . D.lineAnnotation
        groups = groupBy ((==) `on` isContext) $ D.hunkLines hunk
        trim _ _ [] = []
        trim srcStart dstStart (g:gs)
            | isContext $ head g = trim (srcStart + length g) (dstStart + length g) gs
            | otherwise = hunk':rest
                where
                    hunk' = Hunk (srcStart, length removed) (dstStart, length added) (map D.lineContent added)
                    rest = trim (srcStart + length removed) (dstStart + length added) gs
                    added = filter ((D.Added ==) . D.lineAnnotation) g
                    removed = filter ((D.Removed ==) . D.lineAnnotation) g


diffToHunks :: D.FileDelta -> [Hunk]
diffToHunks delta = concatMap trimContext $ D.fileDeltaHunks delta

fileParts :: Tree -> D.FileDelta -> GenHaxl u [FileDiffPart]
fileParts tree delta = do
    let hunks = diffToHunks delta
    source <- getFileContents tree (unpack $ D.fileDeltaSourceFile delta)
    return $ fillInContext (zip [1..] $ Data.Text.lines source) hunks


filesForPR :: Owner -> Repo -> PullRequest -> GenHaxl u [(D.FileDelta, [FileDiffPart])]
filesForPR owner repo pr = do
    pr <- getPullRequest owner repo pr
    diffContents <- getPullRequestDiff pr
    case parseDiff diffContents of
        Left err -> error $ "diff parsing error"
        Right deltas -> do
            tree <- getTree owner repo $ pullRequestCommitSha $ detailedPullRequestBase $ pr
            parts <- mapM (fileParts tree) deltas
            return $ zip deltas parts

getReviewR :: Owner -> Repo -> PullRequest -> Handler Html
getReviewR owner repo pr = do
    env <- liftIO $ initEnv (stateSet GithubState stateEmpty) ()
    let debugEnv = env {flags = Flags 2}
    files <- liftIO $ runHaxl debugEnv $ filesForPR owner repo pr
    let diffLines isSrc lines = $(whamletFile "templates/diff-lines.hamlet")
        contextLines lines = $(whamletFile "templates/context-lines.hamlet")
    defaultLayout $ do
        setTitle $ toHtml $ "Reviewing " ++ show pr
        addScriptRemote "//code.jquery.com/jquery-2.1.1.min.js"
        $(widgetFile "review")
