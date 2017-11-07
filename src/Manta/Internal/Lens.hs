{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Manta.Internal.Lens
    (
      -- Lens for MantaEntity
      time
    , name
    , durability
    , entityType
    -- Lens for MantaEntityType
    ) where
import qualified Manta.Types as T
import qualified Lens.Micro.TH as TH


TH.makeFields ''T.MantaEntity
