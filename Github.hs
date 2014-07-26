module Github
  ( getPullRequest
  , getTree
  , getContents
  , getPullRequestDiff
  , Tree(..), DetailedPullRequest(..), Content
  ) where

import Model (Owner, Repo, Sha, PullRequest, Path)
import Prelude ((.), ($))
import Haxl.Core (dataFetch, GenHaxl)
import Github.DataSource
import Data.Text (Text)
import Github.Data.Definitions (Tree(..), DetailedPullRequest(..), Content)

getPullRequest :: Owner -> Repo -> PullRequest -> GenHaxl u DetailedPullRequest
getPullRequest owner repo pr = dataFetch $ GetPullRequest owner repo pr

getTree :: Owner -> Repo -> Sha -> GenHaxl u Tree
getTree owner repo sha = dataFetch $ GetTree owner repo sha

getContents :: Owner -> Repo -> Sha -> Path -> GenHaxl u Content
getContents owner repo sha path = dataFetch $ GetContents owner repo sha path

getPullRequestDiff :: DetailedPullRequest -> GenHaxl u Text
getPullRequestDiff pr = dataFetch $ GetPullRequestDiff pr