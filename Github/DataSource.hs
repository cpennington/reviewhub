{-# LANGUAGE OverloadedStrings, StandaloneDeriving, RecordWildCards,
    GADTs, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable,
    FlexibleInstances #-}

module Github.DataSource where

import Import hiding (Content)

import Control.Exception (Exception)
import Control.Applicative
import Control.Monad
import Data.Hashable (Hashable(..))
import Data.Text (Text, pack, unpack)
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
import Github.PullRequests (pullRequest')
import Github.Repos (contentsFor')
import Github.Auth (GithubAuth(..))
import Data.List (nub)
import Control.Concurrent.Async (Async, wait, async)
import Control.Arrow ((&&&), right)
import Network.Wreq (getWith, defaults, header, responseBody)
import Control.Lens ((&), (.~), (^.))
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as BLC
import qualified Data.ByteString as BS

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
    data State GithubReq = GithubState { gsAuth :: Maybe GithubAuth }

instance Hashable (GithubReq a) where
    hashWithSalt s (GetPullRequest owner repo pr) = hashWithSalt s (0::Int, owner, repo, pr)
    hashWithSalt s (GetTree owner repo sha) = hashWithSalt s (1::Int, owner, repo, sha)
    hashWithSalt s (GetContents owner repo sha path) = hashWithSalt s (2::Int, owner, repo, sha, path)
    hashWithSalt s (GetPullRequestDiff pr) = hashWithSalt s (3::Int, pullRequestCommitSha $ detailedPullRequestHead pr, pullRequestCommitSha $ detailedPullRequestBase pr)

instance DataSourceName GithubReq where
    dataSourceName _ = "Github"

instance DataSource u GithubReq where
    fetch = githubFetch

initGlobalState :: Maybe Text -> State GithubReq
initGlobalState Nothing = GithubState Nothing
initGlobalState (Just token) = GithubState $ Just $ GithubOAuth $ unpack token

githubFetch state _flags _user bfs =
    AsyncFetch $ \inner -> do
        asyncs <- mapM (fetchAsync $ gsAuth state) bfs
        inner
        mapM_ wait asyncs

fetchAsync :: Maybe GithubAuth -> BlockedFetch GithubReq -> IO (Async ())
fetchAsync auth (BlockedFetch req rvar) =
  async $ do
    res <- fetchReq auth req
    case res of
      Left err -> putFailure rvar $ GithubException err
      Right a -> putSuccess rvar a

fetchReq :: Maybe GithubAuth -> GithubReq a -> IO (Either Error a)
fetchReq auth (GetPullRequest owner repo pr) = pullRequest' auth (unOwner owner) (unRepo repo) (unPR pr)
fetchReq _ (GetTree owner repo sha) = nestedTree (unOwner owner) (unRepo repo) (unSha sha)
fetchReq auth (GetContents owner repo sha path) = contentsFor' auth (unOwner owner) (unRepo repo) (unPath path) $ Just $ unSha sha
fetchReq auth (GetPullRequestDiff pr) = do
    let diffOpts = defaults & header "Accept-Charset" .~ ["utf-8"]
        withAuth = case auth of
            Just (GithubOAuth token) -> diffOpts & header "Authorization" .~ ["token " `BS.append` BLC.pack token]
            _ -> diffOpts
    diffResponse <- getWith withAuth $ detailedPullRequestDiffUrl pr
    return $ Right $ decodeUtf8 $ toStrict $ diffResponse ^. responseBody
