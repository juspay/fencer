-- | Types useful for rule testing.
module Fencer.Rules.Test.Types
  ( RuleFile(..)
  , simpleRuleFile)
where

import           BasePrelude

import           Data.Text (Text)
import qualified System.Directory as Dir
import           System.FilePath (FilePath)

-- | A record useful in testing, which groups together a file path,
-- its contents and file permissions.
data RuleFile = MkRuleFile
  {  -- | The path to the file
    ruleFilePath :: FilePath
    -- | The contents of the file in plain text
  , ruleFileContents :: Text
    -- | A function specifying how the file permissions should be
    -- changed, i.e., what they should be once the file is written to
    -- disk.
  , ruleFileModifyPermissions :: Dir.Permissions -> Dir.Permissions
  }

simpleRuleFile :: FilePath -> Text -> RuleFile
simpleRuleFile p c = MkRuleFile p c id
