Algorithme d'unification - Projet de programmation fonctionnelle
================================================================

Sébastien Delecraz
Éloi Perdereau

M1 Informatique,
Luminy,
Aix-Marseille Université

Le module Term
--------------

Le type de donnée `Term` est définit comme étant soit
* une variable (`TermVar`)
* une fonction (`TermFunc`)
Une variable n'est que composée de son symbole.
Une fonction contient en plus une liste de `Term` qui correspond à la liste de
ses arguments. `Term` est donc définit récursivement.
Le type des symboles des variables et des fonctions est générique.

Les fonctions `varToTerm` et `constructTerm` sont des sortes d'alias aux
constructeurs `TermVar` et `TermFunc`. Elles sont exportés par le module Term.
Ainsi, on peut "cacher" l'implémentation de `Term` en n'exportant pas ses
constructeurs mais en laissant tout de même la possibilité à l'utilisateur de
construire des objets de type `Term`.

La fonction de composition se décompose en quatre étapes. Pour (sigma @@ tau)
avec sigma = [ (x1,t1), ..., (xn,tn) ]
     tau   = [ (y1,s1), ..., (ym,sm) ]
1. Appliquer sigma à tous les termes de tau.
   On obtient `[ (y1, s1 *! sigma), ..., (ym, sm *! sigma) ]`.
2. Supprimer les termes redondants, ie. les termes où `yi == si *! sigma`.
3. Supprimer de sigma les couples où la variable est une variable de tau.
4. Concaténer la liste obtenue en 2. et celle obtenue en 3.

Le type `UnifProblem` a été introduit pour représenter un problème
d'unification. C'est un alias vers le type liste de couples de `Term`.

La fonction d'unification a été écrit en deux versions: `unify` et `unifyBis`.
* `unify` n'est que la traduction de l'algorithme d'unification fourni en
  langage Haskell. Cependant, deux points méritent un approfondissement :
    * La comparaison de deux fonction se fait par le premier élément de leur
      signature. Ainsi, on compare leur symbole de fonction ainsi que leur arité
      d'un coup.
    * Les lignes 10 et 13 de l'algorithme ont été compactées en une seule (ligne
      95 de Term.hs) pour économiser un niveau d'indentation.  L'appel récursif
      ne sera pas effectué si la condition à gauche du symbole `||` est évaluée
      à `True` grace à l'évaluation paresseuse de Haskell.
* La deuxième version est basé sur le point 3 du lemme 1.37 du sujet. Elle est
  découpée en trois fonctions :
    * `unifyBis`, c'est une sorte de nettoyage du problème d'unification. Elle
      élimine d'abord les couples de termes triviaux (deux variables
      identiques). Puis parcours la liste à la recherche d'un couple seul qui
      n'aurait pas d'unificateur. Si il y en a au moins un, alors on renvoie
      directement `Nothing`, sinon on unifie le problème d'unification
      normalement avec `unifyBis'` (toujours privée des couples triviaux).
    * `unifyBis'` unifie justement un problème d'unification mais sans vérifier
      ni les cas d'échec immédiats, ni les couples triviaux (lignes 4, 9 et 10
      de l'algorithme).
    * `isFailCase` vérifie si le couple passé en paramètre est un cas d'échec
      immédiat de l'algorithme d'unification. Elle est utilisée dans `unifyBis`.
  Cette décomposition permet de ne pas lancer tous les appels récursifs si un
  couple seul (qui peut se trouver à la fin de la liste) prive la liste entière
  d'unificateur.


Le module ArbitraryTerm
-----------------

Ce module permet de générer aléatoirement des `Term` et des `Substitution`. Les
types `Var`, `FuncSymb`, et `UnifProblem` sont des alias vers des types qui sont
déjà des instances de la classe `Arbitrary`. Il suffit donc d'appeler
`arbitrary` pour les générer (Haskell inférera le type à générer).

Le type `Term` est, lui, instancié à la classe `Arbitrary`. La génération d'un
terme aléatoire est définie comme suit :
* Générer aléatoirement soit une variable soit une fonction.
* Générer une variable : générer aléatoirement son symbole.
* Générer une fonction :
    * Générer aléatoirement son symbole
    * Générer une liste de `Term` de taille n entre 0 et 2 :
        * Générer récursivement un `Term` et une liste de `Term` de taille n-1.
La limite de taille des termes est arbitraire, elle permet juste de ne pas
générer des `Term` trops longs, ce qui rendrait le débogage difficile.

Cette méthode n'est en fait pas très correcte. Il se peut qu'on génère un même
symbole pour une variable ou pour des fonctions avec des arités différentes. Une
meilleure façon de faire aurait été de générer préalablement une liste de
symboles de variable et une liste de symboles de fonctions avec leur arités
respectives. Puis lors de la génération des symboles, prendre au hasard un
symbole dans la liste adéquate. Cette solution n'a pas été implémentée car
l'erreur commise a été découverte trop tard.

Pour générer des substitutions, il a fallu introduire un nouveau type car
`Substitution` est un alias vers un type de Haskell, par conséquent nous ne
pouvons pas définir comment sont générés les éléments de ce type. Le type
`TSubstitution` n'est qu'une encapsulation du type `Substitution`. Lors des
tests, on utilisera donc ce type.

La génération des `TSubstitution` se fait tout simplement en générant une
`Substitution' (donc une liste, c'est Haskell le fait pour nous), puis en
supprimant les couples qui la rendent incorrecte. C'est à dire :
* Supprimer les couples triviaux, ie. où la variable a substituer est la même que
  celle du terme.
* Supprimer les couples qui apparaissent deux fois dans la substitution (ne
  garder que le premier). Cette opétation est quadratique en la taille de la
  substitution. La génération de longues substitutions peut donc prendre un
  temps assez conséquent, ce qui a pour effet de ralentir certains tests.


Les tests avec le module QuickCheck
-----------------------------------

Plusieurs fonctions de tests ont été écrits, toutes vérifiant des propriétés de
problème d'unification, des termes ou des substitutions.

* `testApplicationId` vérifie que la substitution identité est bien l'élément
  neutre de la substitution d'un terme.
* `testComposId` vérifie que la substitution identité est bien l'élément neutre
  de la composition de substitutions.
* `testComposAssoc` vérifie la propriété d'associativité de la composition.
* `testComposDef` vérifie la définition formelle de la composition (en fonction
  de la substitution).
* `testUnifIdempotence` vérifie que la composition d'un unificateur avec
  lui-même est bien idempotente.
* `testUnif` vérifie si l'unificateur résultant de l'algorithme `unify` est bien
  un unificateur du problème d'unification passé en paramètre.
  ie. avec sigma = unify [(s1,t1), ..., (sn,tn)],
      pour 1 <= i <= n, si *! sigma = ti *! sigma.
* `testFailUnif` vérifie que si, pour un problème donné, il existe un couple
  seul qui n'a pas d'unificateur, alors le problème n'a pas d'unificateur. (voir
  `unifBis`.

La fonction `main` exécute tous ces tests en instanciant le type des symboles de
variables par `Var` et le type des symboles fonctions par `FuncSymb`. Pour plus
rapidité d'exécution des tests de facilité de débogage, on peut les instanciés
avec `Char`. Certains tests sont effectués plus rapidement que d'autres en
raison de leur temps d'exécution trop élevé.


Compiler et exécuter
--------------------

Pour compiler les tests, un simple `make` suffit (`make test` fonctionne
également). `make hand` compile un ficher qui exécute des tests simples et
affiche les résultats. Tous les fichiers exécutables sont en extension `.run`

