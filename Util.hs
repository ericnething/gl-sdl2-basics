module Util
       (
         overPtr
       ) where

import Foreign
import Foreign.C
import Control.Monad.IO.Class (MonadIO, liftIO)

overPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
overPtr f = liftIO . alloca $
            \p -> do
              _ <- f p
              peek p
