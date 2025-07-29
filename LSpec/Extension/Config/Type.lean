import LSpec.Config
import LSpec.Extension.Tree

namespace LSpec.Extension

abbrev SpecTransformation := Config -> SpecForest -> SpecForest

def setSpecTransformation : SpecTransformation -> Config -> Config :=
  
  
def getSpecTransformation

def applySpecTransformation (config : Config) : SpecForest -> SpecForest :=
  getSpecTransformation config config
  
