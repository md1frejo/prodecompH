module Constants where

-- NMR constants

sweep :: Float
sweep = 75.1231

data Aminoacid = Aminoacid { 
      longName :: String,
      threeName :: String,
      oneName :: String
    } deriving (Show)

glycine :: Aminoacid
glycine = Aminoacid {longName = "Glycine", threeName = "GLY", oneName = "G"}

valine :: Aminoacid
valine = Aminoacid {longName = "Valine", threeName = "VAL", oneName = "V"}

data Aminoacids = GLY | VAL | LYS | SER
                  deriving (Eq, Ord, Show, Read, Bounded, Enum)

getName :: Aminoacid -> String
getName (Aminoacid {longName = l, threeName = t, oneName = o}) = l  
