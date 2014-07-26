{-# LANGUAGE OverloadedStrings, StandaloneDeriving, RecordWildCards,
    GADTs, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable,
    FlexibleInstances #-}

module Github.DataSource where

import Import hiding (Content)

import Control.Exception (Exception)
import Control.Applicative
import Control.Monad
import Data.Hashable (Hashable(..))
import Data.Text (Text, pack)
import Data.Typeable (Typeable(..))
import Haxl.Core
import Github.Data.Definitions (
    Tree(..)
  , DetailedPullRequest(..)
  , Error
  , GitTree(..)
  , Blob(..)
  , PullRequestCommit(..)
  , ContentData(..)
  , Content(..)
  )
import Github.GitData.Trees (nestedTree)
import Github.GitData.Blobs (blob)
import Github.PullRequests (pullRequest)
import Github.Repos (contentsFor)
import Data.List (nub)
import Control.Concurrent.Async (Async, wait, async)
import Control.Arrow ((&&&), right)
import Network.Wreq (getWith, defaults, header, responseBody)
import Control.Lens ((&), (.~), (^.))
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)


import Debug.Trace
traceShowId a = Debug.Trace.trace (show a) a

data GithubReq a where
    GetPullRequest :: Owner -> Repo -> PullRequest -> GithubReq DetailedPullRequest
    GetTree :: Owner -> Repo -> Sha -> GithubReq Tree
    GetContents :: Owner -> Repo -> Sha -> Path -> GithubReq Content
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
    hashWithSalt s (GetContents owner repo sha path) = hashWithSalt s (2::Int, owner, repo, sha, path)
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
fetchReq (GetPullRequest owner repo pr) = pullRequest (unOwner owner) (unRepo repo) (unPR pr)
fetchReq (GetTree owner repo sha) = nestedTree (unOwner owner) (unRepo repo) (unSha sha)
fetchReq (GetContents owner repo sha path) = contentsFor (unOwner owner) (unRepo repo) (unPath path) $ Just $ unSha sha
fetchReq (GetPullRequestDiff pr) = do
    let diffOpts = defaults & header "Accept-Charset" .~ ["utf-8"]
    diffResponse <- getWith diffOpts $ detailedPullRequestDiffUrl pr
    return $ Right $ decodeUtf8 $ toStrict $ diffResponse ^. responseBody
