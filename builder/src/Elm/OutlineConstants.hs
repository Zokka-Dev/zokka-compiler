module Elm.OutlineConstants
  ( zelmOutlineFile
  , vanillaElmOutlineFile
  )
  where

{-

We need both a string for Zelm configuration files as well as Elm configuration
files because we will potentially be pulling in vanilla Elm packages in a Zelm project.

So everywhere we are looking for a local project configuration file we should be looking 
for zelmConfigurationFile, but everywhere we are looking for a remote project configuration
file (i.e. a dependency), we should look for a zelmConfigurationFile *or* 
vanillaElmConfigurationFile.

-}

zelmOutlineFile :: FilePath
zelmOutlineFile = "zelm.json"

vanillaElmOutlineFile :: FilePath
vanillaElmOutlineFile = "elm.json"
