-- @File Term.hs
-- @Author Sebastien DELECRAZ, Eloi PERDEREAU
-- @Date 08-10-2013

module Term (
    Var,
    FuncSymb,
    Signature,
    Term,
    varToTerm,
    constructTerm,
    variables,
    sig,
    Substitution,
    identity,
    (*!),
    (@@)
) where

import Data.Maybe

type Var = String

type FuncSymb = String

type Signature f = [(f, Int)]

data Term v f = TermVar v | TermFunc f [Term v f] deriving (Eq)

instance (Show v, Show f) => Show (Term v f) where
    show (TermVar v) = show v
    show (TermFunc f ts) = show f ++ "(" ++ args ++ ")"
                where
                    tsStr = show ts
                    args = drop 1 (take (length tsStr -1) tsStr)

varToTerm :: v -> Term v f
varToTerm v = TermVar v

constructTerm :: f -> [Term v f] -> Term v f
constructTerm f ts = TermFunc f ts

variables :: Term v f -> [v]
variables (TermVar v) = [v]
variables (TermFunc f ts) = concat (map variables ts)

sig :: Term v f -> Signature f
sig (TermVar _) = []
sig (TermFunc f ts) = (f, length ts):concat (map sig ts)

args :: Term v f -> [Term v f]
args (TermFunc _ ts) = ts
args _ = []

type Substitution v f = [(v, Term v f)]

identity :: Substitution v f
identity = []

(*!) :: Eq v => Term v f -> Substitution v f -> Term v f
(*!) t [] = t
(*!) (TermVar v) (s:ss) = if fst s == v then snd s
                                        else (TermVar v) *! ss
(*!) (TermFunc f ts) s = TermFunc f (map (\x -> x *! s) ts)

(@@) :: Eq v => Substitution v f -> Substitution v f -> Substitution v f
(@@) s t = s1 ++ s2
    where
    s0 = map (\x -> (fst x, snd x *! t)) s          -- t applied to s's terms
    s1 = filter isVarNotTheTerm s0                  -- s0's synonyms removed
    sVars = map fst s                               -- variables of s
    s2 = filter (\x -> not (fst x `elem` sVars)) t  -- remove t elems from which
                                                    -- the fst is in s variables
    isVarNotTheTerm (u, TermVar v) = u /= v
    isVarNotTheTerm _              = True

unif :: Eq v => [(Term v f, Term v f)] -> Maybe (Substitution v f)
unif [] = Just identity
unif ((TermVar v1, TermVar v2):ps) = unif ps
unif ((s@(TermVar v1), t@(TermFunc v2 ts)):ps) =
  if v1 `elem` (variables t) then Nothing
  else let newSub = [(v1, t)]
           nextPs = map (\c -> ((fst c) *! newSub, (snd c) *! newSub)) ps
           jtau = unif nextPs
       in if isNothing jtau then Nothing
          else do
               tau <- jtau
               return (tau @@ newSub)
unif ((s@(TermFunc _ _), t@(TermVar _)):ps) = unif ((t,s):ps)
--unif ((s@(TermFunc v f), t@(TermFunc v f)):ps) = TODO

-- examples

terme0 = TermVar "x"
terme1 = TermFunc "f" [TermFunc "g" [TermVar "x"], TermVar "y"]

sub1 = [("x", TermFunc "f" [TermVar "w", TermVar "x"]), ("y", TermVar "z")]
sub2 = [("w", TermFunc "g" [TermVar "y"]), ("z", TermVar "c")]

theta  = [("x", TermFunc "f" [TermVar "y"]), ("y", TermVar "z")]
lambda = [("x", TermVar "a"), ("y", TermVar "b"), ("z", TermVar "y")]
