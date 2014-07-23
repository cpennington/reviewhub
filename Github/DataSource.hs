{-# LANGUAGE OverloadedStrings, StandaloneDeriving, RecordWildCards,
    GADTs, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable,
    FlexibleInstances #-}

module Github.DataSource where

import Import

import Control.Exception (Exception)
import Control.Applicative
import Control.Monad
import Data.Hashable (Hashable(..))
import Data.Text (Text, pack)
import Data.Typeable (Typeable(..))
import Haxl.Core
import Github.Data.Definitions (Tree(..), DetailedPullRequest(..), Error, GitTree(..), Blob(..), PullRequestCommit(..))
import Github.GitData.Trees (nestedTree)
import Github.GitData.Blobs (blob)
import Github.PullRequests (pullRequest)
import Data.List (nub)
import Control.Concurrent.Async (Async, wait, async)
import Control.Arrow ((&&&), right)
import Codec.Binary.Base64.String (decode)
import Network.Wreq (getWith, defaults, header, responseBody)
import Control.Lens ((&), (.~), (^.))
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)


import Debug.Trace
traceShowId a = Debug.Trace.trace (show a) a

data GithubReq a where
    GetPullRequest :: Owner -> Repo -> PullRequest -> GithubReq DetailedPullRequest
    GetTree :: Owner -> Repo -> Sha -> GithubReq Tree
    GetFileContents :: Tree -> Path -> GithubReq Text
    GetPullRequestDiff :: DetailedPullRequest -> GithubReq Text
  deriving Typeable

data GithubException = GithubException Error
    deriving (Typeable, Show)

instance Exception GithubException

deriving instance Eq (GithubReq a)
deriving instance Show (GithubReq a)

instance Show1 GithubReq where show1 = show

instance StateKey GithubReq where
    data State GithubReq = GithubState {}

instance Hashable (GithubReq a) where
    hashWithSalt s (GetPullRequest owner repo pr) = hashWithSalt s (0::Int, owner, repo, pr)
    hashWithSalt s (GetTree owner repo sha) = hashWithSalt s (1::Int, owner, repo, sha)
    hashWithSalt s (GetFileContents tree path) = hashWithSalt s (2::Int, treeSha tree, path)
    hashWithSalt s (GetPullRequestDiff pr) = hashWithSalt s (3::Int, pullRequestCommitSha $ detailedPullRequestHead pr, pullRequestCommitSha $ detailedPullRequestBase pr)

instance DataSourceName GithubReq where
    dataSourceName _ = "Github"

instance DataSource u GithubReq where
    fetch = githubFetch

githubFetch _state _flags _user bfs =
    AsyncFetch $ \inner -> do
        asyncs <- mapM fetchAsync bfs
        inner
        mapM_ wait asyncs

fetchAsync :: BlockedFetch GithubReq -> IO (Async ())
fetchAsync (BlockedFetch req rvar) =
  async $ do
    res <- fetchReq req
    case res of
      Left err -> putFailure rvar $ GithubException err
      Right a -> putSuccess rvar a

fetchReq :: GithubReq a -> IO (Either Error a)
fetchReq (GetPullRequest owner repo pr) = pullRequest owner repo pr
fetchReq (GetTree owner repo sha) = nestedTree owner repo sha
fetchReq (GetFileContents tree path) = do
    let maybeBlobSha = lookup path (map (gitTreePath &&& gitTreeSha) $ treeGitTrees tree)
    case maybeBlobSha of
        Nothing -> return $ Right ""
        Just blobSha -> do
            blobRes <- blob "edx" "edx-platform" blobSha
            return $ right decodeBlob blobRes
fetchReq (GetPullRequestDiff pr) = do
    let diffOpts = defaults & header "Accept-Charset" .~ ["utf-8"]
    diffResponse <- getWith diffOpts $ detailedPullRequestDiffUrl pr
    return $ Right $ decodeUtf8 $ toStrict $ diffResponse ^. responseBody

decodeBlob :: Blob -> Text
decodeBlob blob = case blobEncoding blob of
    "utf-8" -> pack $ blobContent blob
    "base64" -> pack $ decode $ blobContent blob
