import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Data.Char
import System.Exit
import System.IO
import System.Process
import System.Directory

main = let hooks     = simpleUserHooks
           lhs2TeX   = ("lhs", pplhs2TeX)
       in defaultMainWithHooks hooks { hookedPreProcessors = lhs2TeX:knownSuffixHandlers  }

lhs2TeXcustom :: String
lhs2TeXcustom = "lhs2TeX/custom.fmt"

pplhs2TeX :: BuildInfo -> LocalBuildInfo -> PreProcessor
pplhs2TeX build local =
   PreProcessor {
     platformIndependent = True,
     runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
     notice verbosity (inFile ++ " is being preprocessed to " ++ outFile)
     header <- readFile lhs2TeXcustom
     notice verbosity ("read contents from " ++ lhs2TeXcustom)
     source <- readFile inFile
     (Just hin, Just hout, _, _) <- createProcess (proc "lhs2TeX" ["--newcode"])
                                                     { std_in  = CreatePipe
                                                     , std_out = CreatePipe }
     hPutStr hin (header ++ source)
     target <- hGetContents hout
     notice verbosity "done"
     writeFile outFile target
     }
