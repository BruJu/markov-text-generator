(* 
  le type des arcs du graphe de la chaîne de Markov 
  les token peuvent être des `string`, des `char`, des `int`...
  le type des arcs est donc polymorphe en celui des token  
*)

type 'token edge = {
  token: 'token;  (* le token qui étiquette l'arc *)
  mutable weight: int;    (* le poids de l'arc *)
  dest: int       (* l'état où pointe l'arc *)
}

(* NOTE : les états sont représentés par un identifiant entier 
   les poids sont représentés par des entiers
   il faut renormaliser pour obtenir la distribution de probabilité
*)



(* la chaîne de Markov, ou plus précisément son graphe *)

type 'token markov_chain = 
  MarkovChain of 'token edge list array

(* 
  note: le graphe est représenté par sa liste d'adjacence
  cf https://fr.wikipedia.org/wiki/Liste_d%27adjacence 
*)

(* exemple de chaîne de Markov tirée du sujet en ligne *)

let mc_example : char markov_chain = MarkovChain [|
  (* liste d'adjacence de s0 *)
  [{token = 'a'; weight = 1; dest = 1}];
  (* liste d'adjacence de s1 *)
  [{token = 'b'; weight = 1; dest = 3};
   {token = 'g'; weight = 1; dest = 2}];
  (* liste d'adjacence de s2 *)
  [];
  (* liste d'adjacence de s3 *)
  [{token = 'c'; weight = 3; dest = 5};
   {token = 'd'; weight = 1; dest = 2};
   {token = 'e'; weight = 2; dest = 4}];
  (* liste d'adjacence de s4 *)
  [{token = 'f'; weight = 1; dest = 1}];
  (* liste d'adjacence de s5 *)
  []
|]

let mc_smaller_example = MarkovChain [|
  [{token = "a"; weight = 1; dest=0}; {token = "b"; weight = 1; dest=1};];
  [{token = "c"; weight = 2; dest=1}; {token = "d"; weight = 1; dest=2};];
  []
|]