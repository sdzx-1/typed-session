{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ring.Type where

import TypedSession.TH

data RingRole = A | B | C | D
  deriving (Eq, Ord, Show, Enum, Bounded)

data RingBranchSt = Stop | Continue
  deriving (Eq, Ord, Show, Enum, Bounded)

ringPrtocol = protocol @RingRole @RingBranchSt "Ring" ''RingRole ''RingBranchSt
