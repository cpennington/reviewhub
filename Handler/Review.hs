module Handler.Review where

import Import

import Data.Text (pack, lines)

{- source.txt
Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
Line 10
Line 11
Line 12
Line 13
Line 14
Line 15
Line 16
Line 17
Line 18
Line 19
Line 20
-}

{- dest.txt
Line 1
Replace 2
Line 3
Line 4
Insert 4.5
Insert 4.6
Line 5
Line 8
Line 9
Replace x
Replace y
Line 13
Line 14
Line 15
Replace a
Replace b
Replace c
Line 17
Line 18
Line 19
Line 20
-}

{-$ diff -u0 source.txt dest.txt
--- source.txt  2014-07-16 05:58:00.253622979 -0400
+++ dest.txt    2014-07-16 05:59:12.193620434 -0400
@@ -2 +2 @@
-Line 2
+Replace 2
@@ -4,0 +5,2 @@
+Insert 4.5
+Insert 4.6
@@ -6,2 +7,0 @@
-Line 6
-Line 7
@@ -10,3 +10,2 @@
-Line 10
-Line 11
-Line 12
+Replace x
+Replace y
@@ -16 +15,3 @@
-Line 16
+Replace a
+Replace b
+Replace c
-}

data Hunk = Hunk (Int, Int) (Int, Int) Text

dummySourceFile :: Text
dummySourceFile = pack $ unlines ["Line " ++ show l | l <- [1..20]]

dummyHunks :: [Hunk]
dummyHunks = [
    Hunk (2, 1) (2, 1) (pack "Replace 2"), -- Single line substitution
    Hunk (4, 0) (5, 2) (pack "Insert 4.5\nInsert 4.6"), -- Insertion
    Hunk (6, 2) (7, 0) (pack ""), -- Deletion
    Hunk (10, 3) (10, 2) (pack "Replace x\nReplace y"), -- Replacement with fewer lines
    Hunk (16, 1) (15, 3) (pack "Replace a\nReplace b\nReplace c") -- Replacement with more lines
    ]

sourceLines :: Text -> Hunk -> [(Int, Text)]
sourceLines source (Hunk (sStart, sLength) _ _) = zip [sStart..] $ take sLength $ drop (sStart - 1) $ lines source

destLines :: Hunk -> [(Int, Text)]
destLines (Hunk _ (dStart, _) text) = zip [dStart..] $ lines text

getReviewR :: PullRequest -> Handler Html
getReviewR pr = do
    let hunks = dummyHunks -- TODO: Replace this w/ a request to the github api and diff parsing
        source = dummySourceFile -- TODO: Replace this w/ a request to github api
    defaultLayout $ do
        setTitle $ toHtml $ "Reviewing " ++ show pr
        addScriptRemote "//code.jquery.com/jquery-2.1.1.min.js"
        $(widgetFile "review")