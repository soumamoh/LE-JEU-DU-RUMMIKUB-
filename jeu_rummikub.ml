(*

********************************************************************************                                                                                                                                                                                  
*          PROJET - LE JEU DU RUMMIKUB                                         *        
*                                                                              *
********************************************************************************
* Soumahoro Mohamed kevin                                                      *
******************************************************************************** 

*)

(* 3.1 FONCTIONS OPERANT SUR LES MULTI-ENSEMBLES *)

(* 3.2 IMPLEMENTATION DES MULTI-ENSEMBLES SUR UN TYPE SOMME *)

(* Q1. *)

type nat = int;;
type 'a multielement = 'a*int;;
type 'a multiensemble = V | A of 'a multielement*'a multiensemble;;

(* Definissions de mes multi-ensembles pour verifier au fur et mesure mes codes *)

let ens1 = A((3,2),A((4,1),A((6,3),V))) and ens2 = A((6,3),A((3,2),A((4,1),V)));;

(* 1 - Cardinalite *)

let rec cardinal (multE : 'a multiensemble) : int =
match multE with
V -> 0
|A((_,m),suite) -> m + cardinal suite;;


(* 2 - Nombre d'occurences *)

let rec nbocc (x : 'a) (multE : 'a multiensemble) : int =
match multE with
V -> 0
|A((e,m),suite) -> if x=e then m else (nbocc x suite);;


(* 3 - Appartenance *)

let rec appartient (x:'a) (multE : 'a multiensemble) : bool =
match multE with
V -> false
|A((e,_),suite) -> (x=e) || (appartient x suite);;

(* 4 - Inclusion *)

let rec inclus (multE1 : 'a multiensemble) (multE2 : 'a multiensemble) : bool =
match multE1,multE2 with
(V,_) -> true
|(A((e1,_),suite1),_) -> if appartient e1 multE2 then if (nbocc e1 multE1) <= (nbocc e1 multE2) then inclus suite1 multE2 else false else inclus suite1 multE2;; 


(* 5 - Ajout *)

let rec ajoute ((a,b) : 'a multielement) (multE : 'a multiensemble) : 'a multiensemble =
match multE with
V -> A((a,b),V)
|A((e,m),suite) -> if a=e then A((e,m+b),suite) else A((e,m),ajoute (a,b) suite);;


(* 6 - Suppression *)

let rec supprime ((a,b) : 'a multielement) (multE : 'a multiensemble) : 'a multiensemble =
match multE with
V -> V
|A((e,m),suite) -> if a=e then if b>=m then suite else A((e,m-b),suite) else A((e,m),supprime (a,b) (suite));;


(* 7 - Egalite *)

let egaux (multE1 : 'a multiensemble) (multE2 : 'a multiensemble) : bool =
(inclus multE1 multE2) && (inclus multE2 multE1);;


(* 8 - Intersection *)

let rec intersection (multE1 : 'a multiensemble) (multE2 : 'a multiensemble) : 'a multiensemble =
match multE1,multE2 with
(V,_) -> V
|(A((e,m),suite),_) -> if appartient e multE2 then if (nbocc e multE1) <= (nbocc e multE2) then A((e, nbocc e multE1),intersection suite multE2) else A((e,nbocc e multE2),intersection suite multE2) else intersection suite multE2;;


(* 9 - Difference *)

let rec difference (multE1 : 'a multiensemble) (multE2 : 'a multiensemble) : 'a multiensemble =
match multE1,multE2 with
(V,_) -> V
|(A((e,m),suite),_) -> if appartient e multE2 then if (nbocc e multE1) <= (nbocc e multE2) then difference suite multE2 else A((e,((nbocc e multE1) - (nbocc e multE2))),difference suite multE2) else A((e,m),difference suite multE2);;


(* 10 - Obtention *)

let rec ieme (i : int) (multE : 'a multiensemble) : 'a =
match multE with
V -> failwith "ieme"
|A((e,m),suite) -> if i=1 then e else ieme (i-1) suite;;

let rec longueur (multE : 'a multiensemble) : int =
match multE with
V -> 0
|A((_,_),suite) -> 1 + longueur suite;;

let rec un_dans (multE : 'a multiensemble) : 'a =
  match multE with
  V -> failwith "un_dans"
  |A((e,_),V) -> e
  |A((e1,m1),A((e2,m2),suite)) -> let (x,i) = (((Random.int (cardinal multE)) + 1),m1) in if ( x <= i) then e1 else (un_dans (A((e2,m1+m2),suite)));;

(* SANS LA FONCTION RANDOM *)

(* let rec un_dans (multE : 'a multiensemble) : 'a =
match multE with
V -> failwith "un_dans"
|A((e,m),N) -> e
|A((e1,m1),A((e2,m2),suite)) -> if m1>=m2 then un_dans (ajoute (e1,m1) suite) else
un_dans (ajoute (e2,m2) suite);; *)


(* 4 - REUSINAGE *)

(* Q2. *)

type 'a multiensemble = 'a multielement list;;

(* Definissions de mes multi-ensembles pour verifier au fur et mesure mes codes *)

let ens1 = [(3,2);(4,1);(6,3)] and ens2 = [(6,3);(3,2);(4,1)];;

(* 1 - Cardinal *)

let rec cardinal (multE : 'a multiensemble) : int =
match multE with
[] -> 0
|(e,m)::suite -> m + cardinal suite;;

(* 2 - Nombre d'occurences *)

let rec nbocc (x : 'a) (multE : 'a multiensemble) : int =
match multE with
[] -> 0
|(e,m)::suite -> (if x=e then m else 0) + (nbocc x suite);;

(* 3 - Appartenance *)

let rec appartient (x : 'a) (multE : 'a multiensemble) : bool =
match multE with
[] -> false
|(e,m)::suite -> (x=e) || (appartient x suite);;

(* 4 - Inclusion *)

let rec inclus (multE1 : 'a multiensemble) (multE2 : 'a multiensemble) : bool =
match multE1,multE2 with
([],_) -> true
|((e,_)::suite,_) -> if appartient e multE2 then if (nbocc e multE1 <= nbocc e multE2) then inclus suite multE2 else false else inclus suite multE2;;

(* 5 - Ajout *)

let rec ajoute ((x,n) : 'a multielement) (multE : 'a multiensemble) : 'a multiensemble =
match multE with
[] -> [(x,n)] 
|(e,m)::suite-> if x=e then (e,m+n)::suite else (e,m)::ajoute (x,n) suite;;

(* 6 - Suppression *)

let rec supprime ((a,b) : 'a multielement) (multE : 'a multiensemble) : 'a multiensemble =
match multE with
[] -> []
|(e,m)::suite -> if a=e then if b>=m then suite else (e,m-b)::suite else (e,m)::supprime (a,b) (suite);;

(* 7 - Egalites *)

let egaux (multE1 : 'a multiensemble) (multE2 : 'a multiensemble) : bool =
(inclus multE1 multE2) && (inclus multE1 multE2);;

(* 8 - Intersection *)

let rec intersection (multE1 : 'a multiensemble) (multE2 : 'a multiensemble) : 'a multiensemble =
match multE1,multE2 with
([],_) -> []
|((e,m)::suite,_) -> if appartient e multE2 then if nbocc e multE1 <= nbocc e multE2 then (e,nbocc e multE1)::intersection suite multE2 else (e,nbocc e multE2)::intersection suite multE2 else intersection suite multE2;;

(* 9 - Difference *)

let rec difference (multE1 : 'a multiensemble) (multE2 : 'a multiensemble) : 'a multiensemble =
match multE1,multE2 with
([],_) -> []
|((e,m)::suite,_) -> if appartient e multE2 then if (nbocc e multE1) <= (nbocc e multE2) then difference suite multE2 else (e,(nbocc e multE1) - (nbocc e multE2))::difference suite multE2 else (e,m)::difference suite multE2;;

(* 10 - Obtention aleatoire *)

let rec ieme (i : int) (multE : 'a multiensemble) : 'a =
match multE with
[] -> failwith "ieme"
|(e,m)::suite -> if i<=m then e else (ieme (i-m) (suite));;


let un_dans (multE : 'a multiensemble) : 'a =
match multE with
|[] -> failwith "un_dans"
|_ -> let x = (Random.int(cardinal multE))+1 in (ieme x multE);;


(* Q3. *)

(* 1 - Cardinalite *)
 
let cardinal (m:'a multiensemble) : int = 
List.fold_left ( fun x (y,n) -> n+x ) 0 m;;

(* 2 - Nombres d'occurences *)

let nbocc (a:'a) (m:'a multiensemble) : int = 
List.fold_left ( fun x (y,n) -> if a=y then n else x ) 0 m;;

(* 3 - Appartenance *)

let appartient (a:'a) (m:'a multiensemble) : bool = 
List.exists ( fun (x,_) -> a=x ) m;;

(* 4 - Inclusion *)

let inclus (m1:'a multiensemble) (m2:'a multiensemble) : bool = 
List.fold_left ( fun x (y,n) -> if appartient y m2 then if (nbocc y m1) <= (nbocc y m2) then x else false else x ) true m1;;

(* 5 - Ajout *)

let ajoute ((a,b):'a multielement) (m:'a multiensemble) : 'a multiensemble = 
if not (appartient a m) then (a,b)::m else List.fold_left ( fun x (y,n) -> if y=a then (y,n+b)::x else (y,n)::x ) [] m;;

(* 6 - Suppression *)

let supprime ((a,b):'a multielement) (m:'a multiensemble) : 'a multiensemble = 
List.fold_left ( fun x (y,n) -> if y=a then if b>=n then x else (y,n-b)::x else (y,n)::x ) [] m;;

(* 7 - Egalites *)

let egaux (m1:'a multiensemble) (m2:'a multiensemble) : bool = 
(inclus m1 m2) && (inclus m2 m1);;

(* 8 - Intersection *)

let intersection (m1:'a multiensemble) (m2:'a multiensemble) : 'a multiensemble = 
List.fold_left ( fun x (y,n) -> if appartient y m2 then if (nbocc y m1) <= (nbocc y m2) then (y,nbocc y m1)::x else (y,nbocc y m2)::x else x ) [] m1;;

(* 9 - Difference *)

let difference (m1:'a multiensemble) (m2:'a multiensemble) : 'a multiensemble = 
List.fold_right ( fun (x,n) y -> if appartient x m2 then if (nbocc x m1) <= (nbocc x m2) then y else (x,(nbocc x m1) - (nbocc x m2))::y else y ) [] m1;;
 

(* 5 - Le Rummikub *)

(* 6 - Modelisation des donnees *)

(* 6.1 - Les tuiles *)

(* Q4. *)

type couleur = Bleu | Rouge | Jaune | Noir ;;
type valeur = int;;
type tuile = Joker | T of valeur*couleur ;;

(* 6.2 - Combinaisons, tables et poses *)

(* Q5. *)

type combinaison = tuile list ;;
type table = combinaison list ;;
type pose = table ;;

(* 6.3 - Mains et pioches *)

(* Q6. *)

type main = (tuile*int) list ;;
type pioche = main ;;

(* Q7 *)

(* PROFIL  inserer : (tuile*int) -> main -> main = <fun> 
   SEMANTIQUE : inserer t m insere la tuile t au bon endroit dans la main m *)
   
let rec inserer (t:(tuile*int)) (m:main) : main =
match m with
[] -> [t]
|(T(v,c),n)::s -> ( match t with
(Joker,n) -> t::m
|(T(a,b),p) -> if b<c then t::m else if b=c then if a<v then t::m else if a=v then (T(v,c),n+p)::s else (T(v,c),n)::inserer t s else (T(v,c),n)::inserer t s )
|(Joker,n)::s -> ( match t with
(Joker,n1) -> (Joker,n+n1)::s
|_-> (Joker,n)::inserer t s );;

let en_ordre (m:main) : main = 
List.fold_left ( fun x y -> inserer y x ) [] m ;;

(* 6.4 - Les joueurs *)

type joueur = J1 | J2 ;;
type statut = joueur * bool * main ;;

(* 6.5 - Etat d'une partie *)

type etat = (statut * statut) * table * pioche * joueur ;;

(* 6.5.1 - Etat initial *)

(* Q8. *)

(* 1 - Extraction de tuiles d'une pioche *)

let cst_PIOCHE_INIT : pioche = [ (Joker,2) ;
T(1,Rouge),2;T(2,Rouge),2;T(3,Rouge),2;T(4,Rouge),2;T(5,Rouge),2;T(6,Rouge),2;T(7,Rouge),2;T(8,Rouge),2;T(9,Rouge),2;T(10,Rouge),2;T(11,Rouge),2;T(12,Rouge),2;T(13,Rouge),2;
T(1,Bleu),2;T(2,Bleu),2;T(3,Bleu),2;T(4,Bleu),2;T(5,Bleu),2;T(6,Bleu),2;T(7,Bleu),2;T(8,Bleu),2;T(9,Bleu),2;T(10,Bleu),2;T(11,Bleu),2;T(12,Bleu),2;T(13,Bleu),2;
T(1,Jaune),2;T(2,Jaune),2;T(3,Jaune),2;T(4,Jaune),2;T(5,Jaune),2;T(6,Jaune),2;T(7,Jaune),2;T(8,Jaune),2;T(9,Jaune),2;T(10,Jaune),2;T(11,Jaune),2;T(12,Jaune),2;T(13,Jaune),2;
T(1,Noir),2;T(2,Noir),2;T(3,Noir),2;T(4,Noir),2;T(5,Noir),2;T(6,Noir),2;T(7,Noir),2;T(8,Noir),2;T(9,Noir),2;T(10,Noir),2;T(11,Noir),2;T(12,Noir),2;T(13,Noir),2]


(* PROFIL  choix : int -> pioche -> tuile*int = <fun> 
   SEMANTIIQUE : choix n p choisi la nieme tuile dans la pioche p *)
    
let rec choix (n:int) (p:pioche) : tuile * int =
match p with
[] -> failwith"pioche vide"
|t::s -> if n=1 then let (a,b) = t in (a,1) else choix (n-1) s;;


(* PROFIL  ajouter : tuile*int -> pioche -> pioche = <fun>
   SEMANTIQUE : ajouter t p ajoute la tuile t dans la pioche p *)
   
let rec ajouter (t:(tuile*int)) (p:pioche) : pioche = 
match p with
[] -> [t]
|z::s -> let (a,b)=t and (c,d)=z in if a=c then (a,b+d)::s else (c,d)::ajouter t s;;


(* PROFIL  supprimer : tuile*int -> pioche -> pioche = <fun>
   SEMANTIQUE : supprimer t p supprime la tuile t dans la pioche p *)
   
let supprimer (t:(tuile*int)) (p:pioche) : pioche = 
List.fold_right ( fun (c,d) y -> let (a,b) = t in if a=c then if b=d then y else if b>d then y else (a,d-b)::y else (c,d)::y ) p [] ;;

(* PROFIL  card : pioche -> int = <fun> 
   SEMANTIQUE : card p renvoie le nombre d'element de la pioche p *)
   
let card (p:pioche) : int = 
List.fold_right ( fun _ y -> 1 + y ) p 0 ;;


let rec extraire (n:int) (p:pioche) : main*pioche =
match p with
[] -> [],[]
|p -> let c = choix ((Random.int (card p))+1) p in let s = supprime c p in if n=1 then (ajoute c [],(en_ordre s)) else let (a,b) = extraire (n-1) s in (ajoute c a,b);;

(* 2 - Distribution *) 

let distrib () : main*main*pioche = 
let (a,b) = extraire 14 cst_PIOCHE_INIT in 
let (c,d) = extraire 14 b in (a,c,d);;

(* 3 - Etat initial *)

let init_partie () : etat =
let (mJ1,mJ2,p) = distrib () in ((J1,false,mJ1),(J2,false,mJ2)),[],p,J1 ;;

(* 6.5.2 - Acces aux informations d'un etat *)

(* Q9. *)

(* 1 - joueur_courant, joueur_suivant *)

let joueur_courant (((s1,s2),t,p,j):etat) : joueur =
j ;;

let joueur_suivant (e:etat) : joueur = 
if (joueur_courant e = J1) then J2 else J1 ;;

(* 2 - la_table *)

let la_table (((s1,s2),t,p,j):etat) : table =
t ;;

(* 3 - la_pioche *)

let la_pioche (((s1,s2),t,p,j):etat) : pioche =
p ;;

(* 4 - le_statut *)

let le_statut (jo:joueur) (((s1,s2),t,p,j):etat) : statut =
match jo with
J1 -> s1
|J2 -> s2 ;;

(* 5 - la_main *)

let la_main (jo:joueur) (((s1,s2),t,p,j):etat) : main =
match jo with
J1 -> let (a,b,c) = s1 in c
|J2 -> let (d,e,f) = s2 in f ;;


(* 7 - Mise en oeuvre des regles *)

(* 1 - est_suite *)

(* PROFIL  suite_valide : int -> couleur -> int -> combinaison -> bool = <fun>
   SEMANTIQUE : suite_valide e c occ b verifie que la tuile de couleur c et d'entiers e est une suite valide par rapport a la tuile suivante dans la combinaison b *)
   
let rec suite_valide (e:int) (c:couleur) (occ:int) (b:combinaison) : bool =
match b with
[] -> occ >= 3
|Joker::s -> suite_valide (e+1) c (occ+1) s
|T(ent,coul)::s -> if ent = e+1 && coul = c then suite_valide (e+1) (coul) (occ+1) s else false ;;

let est_suite (c:combinaison) : bool =
match c with
[] -> true
|[Joker]|[Joker;Joker] -> false
|Joker::T(ent,coul)::s -> suite_valide (ent) (coul) 2 s
|Joker::Joker::T(ent,coul)::s -> suite_valide (ent) (coul) 3 s
|T(ent,coul)::s -> suite_valide (ent) (coul) 1 s
|_ -> false ;;

(* 2 - est_groupe *)

(* PROFIL  groupe_valide : int -> couluer -> int -> combinaison -> bool = <fun>
   SEMANTIQUE : groupe_valide e c occ b verifie que la tuile de couleur c et d'entiers e est un groupe valide par rapport a la tuile suivante dans la combinaison b *)
   
let rec groupe_valide (e:int) (c:couleur) (occ:int) (b:combinaison) : bool =
match b with
[] -> occ >=3 && occ<=4
|Joker::s -> groupe_valide e c (occ+1) s
|T(ent,coul)::s -> if (ent = e) && (c <> coul) then groupe_valide (e) (coul) (occ+1) s else false ;;

let est_groupe (c:combinaison) : bool =
match c with
[] -> true
|[Joker]|[Joker;Joker] -> false
|Joker::T(ent,coul)::s -> groupe_valide (ent) (coul) 2 s
|Joker::Joker::T(ent,coul)::s -> groupe_valide (ent) (coul) 3 s
|T(ent,coul)::s -> groupe_valide (ent) (coul) 1 s
|_ -> false ;;


(* 3 - Validite d'une combinaison *)

let combinaison_valide (c:combinaison) : bool =
(est_suite c) || (est_groupe c) ;;

(* 4 - Validite d'une sequence de combinaison *)

let combinaison_valides (c_l:combinaison list) : bool = 
List.for_all (combinaison_valide) c_l ;;

(* 7.2 - Calcul de points *)

(* 1 - Points d'une suite *)

(* PROFIL nbElt : tuile list -> int = <fun>
  SEMANTIQUE : nbElt l renvoie le nombre d'element d'un sequence de tuile *)
  
let nbElt (l: tuile list) : int  = 
List.fold_right ( fun _ y -> 1 + y ) l 0;;

let points_suite (c:combinaison) : int = 
if (est_suite c) then 
match c with
|T(ent,_)::s -> (2*ent+(nbElt s))*(nbElt c)/2
|Joker::T(ent,_)::s -> (2*ent-2+((nbElt s)+1))*(nbElt c)/2
|Joker::Joker::T(ent,_)::s ->  (2*ent-4+((nbElt s)+2))*(nbElt c)/2
|_ -> 0
else failwith "Suite Invalide" ;;

(* 2 - Points d'un groupe *)

let points_groupe (c:combinaison) : int =
if (est_groupe c) then
match c with
|T(ent,_)::_ -> ent*(nbElt c)
|Joker::T(ent,_)::_ -> ent*(nbElt c)
|Joker::Joker::T(ent,_)::_ -> ent*(nbElt c)
|_ -> 0
else failwith "Groupe Invalide" ;;

(* Points d'une pose *)

let points_pose (p:pose) : int = 
if (combinaison_valides p) then List.fold_right ( fun x y -> if (est_suite x) then (points_suite x)+y else (points_groupe x)+y ) p 0 else failwith "Combinaison Invalide" ;;


(* 8 - GAMEPLAY *)

(* Q12. *)

let tableVmens (t:table) : (tuile*int) list =
let e = List.fold_right ( fun x y -> ( List.map ( fun z -> (z,1) ) x)@y ) t [] in en_ordre e;; 

(* 8.1 - Validite du premier coup *)

(* Q13. *)

(* PROFIL  tuileIntVtuile : main -> combinaison = <fun>
   SEMANTIQUE : tuileIntVtuile m converti la main ((tuile*int) list) m en combinaison (tuile list) *)
   
let tuileIntVtuile (m:main) : combinaison = 
List.fold_right ( fun (x,i) y -> if i=2 then [x;x]@y else [x]@y ) m [];;


(* PROFIL  app : tuile*int -> main = <fun>
   SEMANTIQUE : app t m verifie que la tuile t appartient a la main m *)
   
let app (t:(tuile*int)) (m:main) : bool = 
List.fold_left ( fun y (a,b) -> let (c,d) = t in (a=c) || y ) false m ;;


(* PROFIL  app_for_all : pose -> main -> bool = <fun>
   SEMANTIQUE : app_for_all p m verifie que tous les elements de la pose p appartiennent la main m *)
   
let rec app_for_all (p:pose) (m:main) : bool =
let po = tableVmens p in
match po with
[] -> true
|e::s -> (app e m) && (app_for_all [tuileIntVtuile s] m) ;;


(* PROFIL  supp_for_all : pose -> main -> main = <fun>
   SEMANTIQUE : supp_for_all p m supprime tous les elements de la pose p dans la main m *)
   
let rec supp_for_all (p:pose) (m:main) : main =
let po=tableVmens p in
match po,m with
|[],_-> m
|_,[] -> []
|e::s,_ -> supp_for_all [tuileIntVtuile s] (supprimer e m) ;; 


let premier_coup_ok (m0:main) (p0:pose) (m1:main) : bool =
(app_for_all p0 m0) && ((combinaison_valides p0) && (points_pose p0 >= 30)) && ((supp_for_all p0 m0) = m1);;


(* 2 - Validite des autres coups *)

(* PROFIL  supprime_t : table -> table = <fun>
   SEMANTIQUE : supprime_t t0 t1 supprime tous les elements de la table t0 dans la table t1 *)
   
let rec supprime_t (p0:table) (p1:table) : table =
let po1 = tableVmens p0 and po2 = tableVmens p1 in
match po1 with
[] -> p1
|e::s -> supprime_t [tuileIntVtuile s] [tuileIntVtuile (supprimer e po2)]


let coup_ok (t0:table) (m0:main) (t1:table) (m1:main) : bool =
let q = supprime_t t0 t1 in (combinaison_valides t1) && (app_for_all q m0) && ((supp_for_all q m0) = m1) ;;


(* 8.2 - Ajout d'une tuile sur la table *)

(* Q14. *)

(* PROFIL  tuileVtuileInt : tuile list -> (tuile*int) list = <fun>
   SEMANTIQUE : tuileVtuileInt tL converti la tuile list tL en ((tuile*int) list) *)

let tuileVtuileInt (tL:tuile list) : (tuile*int) list = 
let e = List.fold_left ( fun x y -> [(y,1)]@x ) [] tL in en_ordre e ;;


let rec ajouter_tuile (t:tuile) (tbl:table) : table =
if (combinaison_valides tbl) then 
match tbl with
[] -> []
|e::s -> let r = tuileVtuileInt e in let i = inserer (t,1) r in let i1 = tuileIntVtuile i in if (combinaison_valide i1) then i1::s else let a = ajouter_tuile t s in if a=[] then [] else e::a  
else [] ;;

(* 8.3 - Recherche de combinaison dans une main *)

(* 1 - Extraction d'une combinaison *)

(* Q15. *)

(* UTILISATION DE L'ALGORITHME 1 : Extraire au hasard un certain nombre de tuiles de la main et verifier si elles forment un suite (rep. un groupe). Si ce n'est pas le cas                                            , essyer a nouveau. Le nombre d'essais est a determiner. *)


(* PROFIL  inser_groupe : main -> table -> table = <fun>
   SEMANTIQUE : inser_groupe m t insere tous les tuiles possibles de la main m dans la table t pour obtenir une table avec un groupe valide *)
   
let inser_groupe (m:main) (t:table) : table = 
List.fold_right ( fun (a,b) y -> let r = ajouter_tuile a t in if r = [] then y else if (est_groupe (List.flatten r)) then r else y) m t ;;


(* PROFIL  inser_suite : main -> table -> table = <fun>
   SEMANTIQUE : inser_suite m t insere tous les tuiles possibles de la main m dans la table t pour obtenir une table avec une suite valide *)
   
let rec inser_suite  (m:main) (t:table) : table =
match m with
[] -> t
|e::s -> let (a,b) = e in let r = ajouter_tuile a t in if r = [] then inser_suite s t else if (est_suite (List.flatten r)) then  inser_suite s r else inser_suite s t ;;


(* PROFIL  ext_suite : main -> int -> combinaison*pioche = <fun>
   SEMANTIQUE : ext_suite m n essaie n fois de trouver une suite valide dans la main m *)
   
let rec ext_suite (m:main) (n:int) : combinaison*pioche =
let (c,d) = extraire 3 m in let h = tuileIntVtuile c in 
if (n=0) then ([],m) else if (est_suite h) then (h,d) else ext_suite m (n-1) ;;

let extraction_suite (m:main) : combinaison = 
let (x,d) = ext_suite m (10000) in List.flatten (inser_suite d [x]) ;;


(* PROFIL  ext_groupe : main -> int -> combinaison*pioche = <fun>
   SEMANTIQUE : ext_groupe m n essaie n fois de trouver un groupe valide dans la main m *)
   
let rec ext_groupe (m:main) (n:int) : combinaison*pioche =
let (c,d) = extraire 3 m in let h = tuileIntVtuile c in if (n=0) then ([],m) else if (est_groupe h) then (h,d) else ext_groupe m (n-1) ;;

let  extraction_groupe (m:main) : combinaison = 
let (x,d) = ext_groupe m (10000) in List.flatten (inser_groupe d [x]) ;;


(* 8.4 - Changement d'etat lors d'une pioche *)

(* 1 - Changement d'etat lors d'une pioche *)

(* Q16. *)

let piocher (e: etat) : etat =
let (((j_1,b1,m1),(j_2,b2,m2)),t,p,j) = e in if p=[] then e else let j_c = joueur_courant e and j_s = joueur_suivant e and (x,d) = extraire 1 p in if (j_c = J1) then (((j_c,b1,(m1)@(x)),(j_2,b2,m2)),t,d,j_s) else (((j_1,b1,m1),(j_c,b2,(m2)@(x))),t,d,j_s) ;;

(* 2 - Changement d'etat lors du coup *)

let jouer_1_coup (e:etat) (tbl:table) : etat =
let (((j_1,b1,m1),(j_2,b2,m2)),t,p,j) = e in if ((b1=false) && (joueur_courant e = J1)) || ((b2=false) && (joueur_courant e = J2)) then e else let q = supprime_t t tbl and j_s = joueur_suivant e in  if (joueur_courant e = J1) then let s = supp_for_all q m1 in if (coup_ok t m1 tbl s) then (((j_1,b1,s),(j_2,b2,m2)),tbl,p,j_s) else e else let s = supp_for_all q m2 in if (coup_ok t m2 tbl s) then (((j_1,b1,m1),(j_2,b2,s)),tbl,p,j_s) else e ;;

(* 3 - Changement d'etat lors du premier coup *)

let jouer_1er_coup (e:etat) (p0:pose) : etat =
let (((j_1,b1,m1),(j_2,b2,m2)),t,p,j) = e in if ((b1=true) && (joueur_courant e = J1)) || ((b2=true) && (joueur_courant e = J2)) then e else let j_s = joueur_suivant e in if (joueur_courant e = J1) then let s = supp_for_all p0 m1 in if (premier_coup_ok m1 p0 s) then (((j_1,true,s),(j_2,b2,m2)),p0@t,p,j_s) else e else let s = supp_for_all p0 m2 in if (premier_coup_ok m2 p0 s) then (((j_1,b1,m1),(j_2,true,s)),p0@t,p,j_s) else e ;;


(* PROPOSONS UN AUTRE SCENARIO DE JEU 

(* Q17. *)

(* 0. Debut de partie, on (re)initialise l'etat initial *)

let etat_init : etat = init_partie () ;;

(* Definissions de la main des deux joueurs *)

let m1 = la_main J1 etat_init ;; (* m1 = [T(8,Noir),1;T(2,Jaune),1;T(9,Rouge),1;T(9,Bleu),1;
					 T(10,Rouge),1;T(8,Jaune),1;T(11,Noir),1;
					 T(5,Rouge),1;T(10,Noir),1;T(3,Rouge),1;T(1,Bleu),1;
					 T(10,Bleu),1;T(4,Rouge),2] *)
					 
let m2 = la_main J2 etat_init ;; (* m2 = [T(11,Jaune),1;T(10,Bleu),1;T(1,Rouge),1;T(9,Noir),1;
					T(9,Jaune),1;T(3,Jaune),2;T(7,Noir),1;
					 T(1,Bleu),1;T(11,Bleu),1;T(4,Noir),1;T(2,Rouge),1;
					 T(13,Noir),2] *)

(* 1. J1 joue son premier coup *)

let coup1 = extraction_suite m1 ;; (* coup1 = [T(3,Rouge);T(4,Rouge);T(5,Rouge)] *)
let coup2 = extraction_groupe m1 ;; (* coup2 = [T(10,Bleu);T(10,Noir);T(10,Rouge)] *)

let e1 : etat = jouer_1er_coup etat_init [coup1,coup2] ;;

let m1 = la_main J1 e1 ;;
let m2 = la_main J2 e1 ;;

(* 2. J2 pioche *)

let e2 : etat = piocher e1 ;;

let m1 = la_main J1 e2 ;;
let m2 = la_main J2 e2 ;;

(* 3. J1 pioche *)

let e3 : etat = piocher e2 ;;

let m1 = la_main J1 e3 ;;
let m2 = la_main J2 e3 ;;

(* 4. J2 joue son premier coup *)

let coup1 = extraction_suite m2 ;; (* coup1 = [T(10,Bleu);T(11,Bleu);T(12,Bleu)] *)

let e4 : etat = jouer_1er_coup e3 [coup1] ;;

let m1 = la_main J1 e4 ;;
let m2 = la_main J2 e4 ;;

let tbl = la_table e4 ;; (* tbl = [[T(3,Rouge);T(4,Rouge);T(5,Rouge)];
			           [T(10,Bleu);T(10,Noir);T(10,Rouge)];
				   [T(10,Bleu);T(11,Bleu);T(12,Bleu)]] *)
				  
(* 5. J1 pose sur la table *)

let tbl1 = ajouter_tuile (T(9,Bleu)) tbl ;; (* tbl1 = [[T(3,Rouge);T(4,Rouge);T(5,Rouge)];
			                               [T(10,Bleu);T(10,Noir);T(10,Rouge)];
				  		       [T(9,Bleu);T(10,Bleu);T(11,Bleu);T(12,Bleu)]] *)

let e5 : etat = jouer_1_coup e4 tbl1 ;;
let m1 = la_main J1 e5 ;;
let m2 = la_main J2 e5 ;;

let tbl = tbl1 ;;

(* 6. J2 pose sur la table *)

let tbl1 = ajouter_tuile (T(1,Rouge)) (ajouter_tuile (T(2,Rouge)) tbl) ;; (* tbl1 = [[T(1,Rouge);T(2,Rouge);T(3,Rouge);T(4,Rouge);T(5,Rouge)];
			                                                             [T(10,Bleu);T(10,Noir);T(10,Rouge)];
				  		                                     [T(9,Bleu);T(10,Bleu);T(11,Bleu);T(12,Bleu)]] *)

let e6 : etat = jouer_1_coup e5 tbl1 ;;
let m1 = la_main J1 e6 ;;
let m2 = la_main J2 e6 ;;
let tbl = tbl1 ;;

(* 7. J1 pioche *)

let e7 : etat = piocher e6 ;;
let m1 = la_main J1 e7 ;;
let m2 = la_main J2 e7 ;;

(* 8. J2 pioche *)
					       
let e8 : etat = piocher e7 ;;
let m1 = la_main J1 e8 ;;
let m2 = la_main J2 e8 ;;

(* 9. J1 pioche *)
						
let e9 : etat = piocher e8 ;;
let m1 = la_main J1 e9 ;;
let m2 = la_main J2 e9 ;;	

(* 10. .. *)				


*)							

	
									(* FIN DU PROJET RUMMIKUB *)
