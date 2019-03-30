
-- Useful for compsite effect handlers.

{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Helper.Eff where

import Helper.Co

-- Used to 'pass through' syntax that isn't handled by handler.
-- Like (▽) from Fusion for Free (p.5)
pattern Other s = R s
