module Model where

import Prelude (Int, String, Show, Read, Eq)
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Hashable (Hashable)

newtype PullRequest = PullRequest {unPR :: Int}
    deriving (Show, Read, Eq, PathPiece, Hashable)
newtype Owner = Owner {unOwner :: String}
    deriving (Show, Read, Eq, PathPiece, Hashable)
newtype Repo = Repo {unRepo :: String}
    deriving (Show, Read, Eq, PathPiece, Hashable)
newtype Sha = Sha {unSha :: String}
    deriving (Show, Read, Eq, PathPiece, Hashable)
newtype Path = Path {unPath :: String}
    deriving (Show, Read, Eq, PathPiece, Hashable)
newtype Ref = Ref {unRef :: String}
    deriving (Show, Read, Eq, PathPiece, Hashable)


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
