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

type Signature f = [(f, Int)]

data Term v f = TermVar v | TermFunc f [Term v f] deriving (Eq)

instance (Show v, Show f) => Show (Term v f) where
    show (TermVar v) = show v
    show (TermFunc f ts) = show f ++ "(" ++ args ++ ")"
                where
                    tsStr = show ts
                    args = drop 1 (take (length tsStr -1) tsStr)

varToTerm :: Var -> Term Var f
varToTerm v = TermVar v

constructTerm :: FuncSymb -> [Term v FuncSymb] -> Term v FuncSymb
constructTerm f ts = TermFunc f ts

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
(*!) (TermVar v) (s:ss) = if fst s == v then snd s
                                        else (TermVar v) *! ss
(*!) (TermFunc f ts) s = TermFunc f (map (\x -> x *! s) ts)

(@@) :: Eq v => Substitution v f -> Substitution v f -> Substitution v f
(@@) s t = map (\c -> (fst c, snd c *! s)) t

-- examples

terme0 = TermVar "x"
terme1 = TermFunc "f" [TermFunc "g" [TermVar "x"], TermVar "y"]

sub1 = [("x", TermFunc "f" [TermVar "w", TermVar "x"]), ("y", TermVar "z")]
sub2 = [("w", TermFunc "g" [TermVar "y"]), ("z", TermVar "c")]
