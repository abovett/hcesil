-- Functions for "compiling" the scanned CESIL code lines into an
-- executable representation.

module Compile
  ( makeJumpTable
  ) where

import qualified Data.Map as Map
import ScanSource

-- Create the jump table
makeJumpTable :: [CodeLine] -> Map.Map String Integer
makeJumpTable cl = foldr jtBuilder Map.empty cl
  where jtBuilder (CodeLine lineNo "" _ _) jt = jt
        jtBuilder (CodeLine lineNo label _ _) jt = Map.insert label lineNo jt
