module Github
  ( getPullRequest
  , getTree
  , getFileContents
  , getPullRequestDiff
  , Tree(..), DetailedPullRequest(..), Text
  ) where

import Model (Owner, Repo, Sha, PullRequest, Path)
import Prelude ((.), ($))
import Haxl.Core (dataFetch, GenHaxl)
import Github.DataSource
import Data.Text (Text)
import Github.Data.Definitions (Tree(..), DetailedPullRequest(..))

getPullRequest :: Owner -> Repo -> PullRequest -> GenHaxl u DetailedPullRequest
getPullRequest owner repo pr = dataFetch $ GetPullRequest owner repo pr

getTree :: Owner -> Repo -> Sha -> GenHaxl u Tree
getTree owner repo sha = dataFetch $ GetTree owner repo sha

getFileContents :: Tree -> Path -> GenHaxl u Text
getFileContents tree path = dataFetch $ GetFileContents tree path

getPullRequestDiff :: DetailedPullRequest -> GenHaxl u Text
getPullRequestDiff pr = dataFetch $ GetPullRequestDiff pr