-- @File Main.hs
-- @Author p1002650
-- @Date 17-10-2013

module Main (main) where

import Term

-- examples

-- terme0 = TermVar "x"
-- terme1 = TermFunc "f" [TermFunc "g" [TermVar "x"], TermVar "y"]

-- sub1 = [("x", TermFunc "f" [TermVar "w", TermVar "x"]), ("y", TermVar "z")]
-- sub2 = [("w", TermFunc "g" [TermVar "y"]), ("z", TermVar "c")]

-- theta  = [("x", TermFunc "f" [TermVar "y"]), ("y", TermVar "z")]
-- lambda = [("x", TermVar "a"), ("y", TermVar "b"), ("z", TermVar "y")]

-- fxgz = TermFunc "f" [TermVar "x", TermFunc "g" [TermVar "z"]]
-- fgzx = TermFunc "f" [TermFunc "g" [TermVar "z"], TermVar "x"]

pb0 = [(constructTerm "f" [varToTerm "x", constructTerm "g" [varToTerm "z"]],
        constructTerm "f" [constructTerm "g" [varToTerm "z"], varToTerm "x"]),
       (varToTerm "x", constructTerm "g" [varToTerm "z"])]

pb1 = [(constructTerm "f" [constructTerm "g" [constructTerm "k" [varToTerm "x"]],
                           varToTerm "y"],
       constructTerm "f" [varToTerm "y", constructTerm "g" [varToTerm "x"]])]

pb2 = [(constructTerm "f" [constructTerm "g" [varToTerm "x"], varToTerm "x"],
        constructTerm "f" [varToTerm "y", constructTerm "g" [varToTerm "z"]]),
       (constructTerm "g" [varToTerm "x"], varToTerm "y")]

pb3 = [(constructTerm "f" [varToTerm "y",
                           constructTerm "k" [varToTerm "y"],
                           constructTerm "g" [varToTerm "x"]],
        constructTerm "f" [constructTerm "k" [varToTerm "y"],
                           constructTerm "k" [varToTerm "y"],
                           varToTerm "y"])]

pbs = [pb0, pb1, pb2, pb3]
unifs = map unify pbs
z = zip pbs unifs

main = printPbs z
  where
    printPbs [] = return ()
    printPbs ((pb,unifier):z) = do
        putStrLn $ "The unifier of " ++ show pb ++ " is "
        putStrLn $ "\t" ++ show unifier ++ "\n"
        printPbs z

