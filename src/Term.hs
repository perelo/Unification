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
    compareVar,
    variables,
    sig,
    Substitution,
    identity,
    (*!),
    (@@),
    UnifProblem,
    unif
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

-- compareVar returns True if the Term given in second argument is
-- a TermVar of the first argument, False otherwise
compareVar :: Eq v => v -> Term v f -> Bool
compareVar x (TermVar y) = x == y
compareVar _ _ = False

variables :: Term v f -> [v]
variables (TermVar v) = [v]
variables (TermFunc f ts) = concat (map variables ts)

sig :: Term v f -> Signature f
sig (TermVar _) = []
sig (TermFunc f ts) = (f, length ts):concat (map sig ts)

type Substitution v f = [(v, Term v f)]

identity :: Substitution v f
identity = []

(*!) :: Eq v => Term v f -> Substitution v f -> Term v f
(*!) t [] = t
(*!) t@(TermVar tvar) ((svar,sterm):ss) = if svar == tvar then sterm
                                                          else t *! ss
(*!) (TermFunc f ts) s = constructTerm f (map (\t -> t *! s) ts)

(@@) :: Eq v => Substitution v f -> Substitution v f -> Substitution v f
(@@) sigma tau = s1 ++ s2
  where
    -- apply sigma to all terms of tau
    s0 = map (\(var,term) -> (var, term *! sigma)) tau
    -- remove couples where the var is the term
    s1 = filter (not.(uncurry compareVar)) s0
    -- remove from sigma couples where the var is in the vars of tau
    tauVars = map fst tau
    s2 = filter (\(var,_) -> not (elem var tauVars)) sigma

type UnifProblem v f = [(Term v f, Term v f)]

unif :: (Eq v, Eq f) => UnifProblem v f -> Maybe (Substitution v f)
unif [] = Just identity
unif ((s@(TermFunc _ ts1), t@(TermFunc _ ts2)):ps) =
      let sSig = sig s !! 0
          tSig = sig t !! 0
      in if sSig /= tSig then Nothing
                         else unif (zip ts1 ts2 ++ ps)
unif ((s@(TermVar v), t):ps) =
    if compareVar v t then unif ps
    else
      if v `elem` (variables t) || isNothing maybeTau then Nothing
      else return $ fromJust maybeTau @@ newSub
           where
             newSub = [(v, t)]
             nextPs = map (\(var,term) -> (var *! newSub, term *! newSub)) ps
             maybeTau = unif nextPs
unif ((s@(TermFunc _ _), t):ps) = unif ((t,s):ps)

