Requirements : Install Haskell-Platform and Cabal and install monad-par package.
# Infer_Algorithm
A parallel inference skeleton algorithm in Haskell
for compiling a inferenceAlg :
on ghci : > :load inferAlgo.hs
          > main
on ghc :  > ghc -O2 inferAlg.hs -threaded
          > ./inferalg -N4(core number, change according to your processor)
# par_Infer
for compiling parInfer:
on ghci : > :load parInfer.hs
          > test "1 + 2"
            Int
on ghc : > ghc -O2 parInfer.hs -threaded
         > ./parInfer -N4(core number, change according to your processor)
Regards
