you can specify which top level declarations to export from a module like so

Module Something
  ( exportedFunction )
  where 

use qualified imports to avoid namespace clashes

import qualified Data.Bool as B
-> B.bool
