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

type Var = String

type FuncSymb = String

type Signature = [(FuncSymb, Int)]

data Term v = TermVar v | TermFunc FuncSymb [Term v]

varToTerm :: Var -> Term Var
varToTerm v = TermVar v

constructTerm :: FuncSymb -> [Term v] -> Term v
constructTerm f ts = TermFunc f ts

variables :: Term v -> [v]
variables (TermVar v) = [v]
variables (TermFunc f ts) = concat (map variables ts)

sig :: Term v -> Signature
sig (TermVar _) = []
sig (TermFunc f ts) = (f, length ts):concat (map sig ts)

type Substitution v = [(v, Term v)]

identity :: Substitution a
identity = []

(*!) :: Eq v => Term v -> Substitution v -> Term v
(*!) t [] = t
(*!) (TermVar v) (s:ss) = if fst s == v then snd s
                                        else (TermVar v) *! ss
(*!) (TermFunc f ts) s = TermFunc f (map (\x -> x *! s) ts)

(@@) :: Eq v => Substitution v -> Substitution v -> Substitution v
(@@) s t = map (\c -> (fst c, snd c *! s)) t

-- examples

terme0 = TermVar "x"
terme1 = TermFunc "f" [TermFunc "g" [TermVar "x"], TermVar "y"]

sub1 = [("x", TermFunc "f" [TermVar "w", TermVar "x"]), ("y", TermVar "z")]
sub2 = [("w", TermFunc "g" [TermVar "y"]), ("z", TermVar "c")]
