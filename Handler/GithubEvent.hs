{-# LANGUAGE OverloadedStrings #-}
module Handler.GithubEvent where

import Import

import Data.Text (pack, append)
import Data.Text.Encoding (decodeLatin1)

import Data.Time (UTCTime)
import Github.Data.Definitions
    ( Error(..)
    , PullRequestEvent(..)
    , PingEvent(..)
    , DetailedPullRequest(..)
    , PullRequestCommit(..)
    , GithubDate(..)
    , GithubOwner(..)
    , repoName
    , repoOwner
    , PullRequestEventType(..)
    )
import Github.Data()


postGithubEventR :: Handler Text
postGithubEventR = do
    maybeEventType <- lookupHeader "X-GitHub-Event"
    -- Dispatch to handlers depending on the event type
    case maybeEventType of
        -- By calling "parseEvent" with the expected concrete output type of the event data type, it forces the compiler
        -- to attempt to interpret the JSON according to that type definition.
        Just "pull_request" -> requireJsonBody >>= handlePullRequestEvent
        Just "ping"         -> requireJsonBody >>= handlePingEvent
        Just et             -> return $ "Unhandled event type " `append` (decodeLatin1 et)
        Nothing             -> error "Required X-GitHub-Event header not found in request"

raiseError :: Either Error b -> b
raiseError = either (error . show) id

handlePullRequestEvent :: PullRequestEvent -> Handler Text
handlePullRequestEvent pev = do
    case pullRequestEventAction pev of
        PullRequestSynchronized -> handleUpdatedPullRequestEvent pev
        PullRequestOpened       -> handleUpdatedPullRequestEvent pev
        a                       -> return $ pack $ "Unhandled pull request event " ++ (show a)

handleUpdatedPullRequestEvent :: PullRequestEvent -> Handler Text
handleUpdatedPullRequestEvent pev = do
    runDB $ insert $ pullRequestDiffHistoryFromEvent pev
    return "Detected pull request update"

pullRequestDiffHistoryFromEvent :: PullRequestEvent -> PullRequestDiffHistory
pullRequestDiffHistoryFromEvent pev = PullRequestDiffHistory (getPullRequestOwner pev)
                                                             (getPullRequestRepoName pev)
                                                             (getPullRequestNumber pev)
                                                             (getBaseSha pev)
                                                             (getHeadSha pev)
                                                             (getUpdateTime pev)

getPullRequestOwner :: PullRequestEvent -> Text
getPullRequestOwner = pack . githubOwnerLogin . repoOwner . pullRequestRepository

getPullRequestRepoName :: PullRequestEvent -> Text
getPullRequestRepoName = pack . repoName . pullRequestRepository

getPullRequestNumber :: PullRequestEvent -> Int
getPullRequestNumber = detailedPullRequestNumber . pullRequestEventPullRequest

getHeadSha :: PullRequestEvent -> Text
getHeadSha = pack . pullRequestCommitSha . detailedPullRequestHead . pullRequestEventPullRequest

getBaseSha :: PullRequestEvent -> Text
getBaseSha = pack . pullRequestCommitSha . detailedPullRequestBase . pullRequestEventPullRequest

getUpdateTime :: PullRequestEvent -> UTCTime
getUpdateTime = fromGithubDate . detailedPullRequestUpdatedAt . pullRequestEventPullRequest

handlePingEvent :: PingEvent -> Handler Text
handlePingEvent _ = return "Alive and well!"
