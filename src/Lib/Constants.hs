{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Constants (tier) where

import           Data.String

tier :: IsString s => s
#ifdef DEVELOPMENT
tier = "development"
#else
tier = "production"
#endif
