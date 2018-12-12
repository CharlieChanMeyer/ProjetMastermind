(** Algorithmes de recherche de code *)
#use "methode_knuth.ml";;
#use "methode_naif.ml";;

module IA :
     sig
     (** Nombre d'algorithmes developpes *)
     val nombre_methodes : int

     (** Choisit un code a proposer
       * @param methode 0 pour l'algorithme naif,
                        1 pour l'algorithme de KNUTH
                        ... et ainsi de suite
       * @param essais la liste des codes deja proposes
       * @param possibles la liste des codes possibles
       * @return le prochain code a essayer
       *)
     val choix : int -> Code.t list -> Code.t list -> Code.t

     (** Filtre les codes possibles
       * @param methode 0 pour l'algorithme naif,
                        1 pour l'algorithme de KNUTH
                        ... et ainsi de suite
       * @param (code, rep) le code essaye et la reponse correspondante
       * @param  possibles la liste courante de codes possibles
       * @return la nouvelle liste de codes possibles
       *)
     val filtre : int -> (Code.t * (int * int) option) -> Code.t list -> Code.t list
     end =
     struct

     let nombre_methodes = 2;;

     let filtre_naif reponse possibles =
          let proposition = fst(reponse) and rep = snd(reponse) in
               match (rep) with
                    | Some(4,0) -> [proposition]
                    | Some(n1,n2) when (n1+n2 = 4) -> let res = Naif.supprime_code4 possibles proposition in res
                    | Some(n1,n2)                  -> let res = Naif.supprime_couleur possibles proposition in res;;

     let filtre methode reponse possibles =
          match methode with
               | 0 -> let res = filtre_naif reponse possibles in res;;
               (*| 1 -> filtre_knuth reponse possibles;;*)


end ;;

let methode_naif tour courant reponse =
     if tour = 1 then
          let proposition = Naif.per_p (Code.tous) in
               let courant = Naif.supprime_element (Code.tous) proposition in
                    (proposition,courant)
     else
          let courant = IA.filtre 0 reponse courant in
               let proposition = Naif.per_p courant in
                    (proposition,courant)
