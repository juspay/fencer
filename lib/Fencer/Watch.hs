{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Watching Fencer's config directory for changes.
--
-- Like @lyft/ratelimit@, Fencer adopts an atomic approach to config
-- reloading: the config directory has to be a symlink, and we watch changes
-- to that symlink, not changes to the directory pointed at by the symlink.
module Fencer.Watch
    ( watchSymlink
    )
where

import BasePrelude

import Named ((:!), arg)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory)
import qualified System.FSNotify as FSNotify

-- | Fork a thread that watches a symlink and executes an action when it's
-- replaced with another symlink.
watchSymlink
    :: "symlink" :! FilePath
    -> "onChange" :! IO ()
    -> IO ()
watchSymlink (arg #symlink -> symlink) (arg #onChange -> onChange) =
    -- Implementation note: we watch the directory *containing* the symlink,
    -- and we care only about 'Added' or 'Modified' events. Unfortunately,
    -- fsnotify doesn't allow watching a file, only a directory.
    void $ forkOS $ FSNotify.withManager $ \manager -> do
        symlinkPath <- makeAbsolute symlink
        let parent = takeDirectory symlinkPath
        let predicate = \case
                FSNotify.Added path _ _ ->
                    path == symlinkPath
                FSNotify.Modified path _ _ ->
                    path == symlinkPath
                _ ->
                    False
        void $ FSNotify.watchDir manager parent predicate $ \_ -> onChange
        forever $ threadDelay 1000000
