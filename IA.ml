(** Algorithmes de recherche de code *)
#use "methode.ml";;

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

     let choix methode essais possibles =
          match (methode) with
               | 0 -> List.nth (possibles) 0
               | _ -> (
                    let rep = Code.reponse (List.hd possibles) (List.hd(List.rev essais)) in
                         let res = Knuth.liste_codes_meme_score_maximum possibles rep in
                              Knuth.mini (List.tl res) (List.hd res)
                    );;


     let filtre_naif reponse possibles =
          let proposition = fst(reponse) and rep = snd(reponse) in
               match (rep) with
                    | Some(4,0) -> [proposition]
                    | Some(n1,n2) when (n1+n2 = 4) -> let res = Naif.supprime_code4 possibles proposition in res
                    | Some(n1,n2)                  -> let res = Naif.supprime_couleur possibles proposition (n1+n2) in res
                    | None -> [];;

     let rec filtre_knuthRT courant proposition rep acc =
          match (courant) with
               | [] -> acc
               | _  -> (
                    let test = List.hd courant in
                         match (Code.reponse test proposition) with
                              | Some(testbp,testmp) -> (
                                   match (rep) with
                                        | Some(propbp,propmp) -> (
                                             if ((testbp=propbp)&&(testmp=propmp)) then
                                                  filtre_knuthRT (List.tl courant) proposition rep (acc @ [test])
                                             else
                                                  filtre_knuthRT (List.tl courant) proposition rep acc
                                             )
                                        | None -> filtre_knuthRT courant proposition rep acc
                                   )
                              | None -> filtre_knuthRT courant proposition rep acc
                    );;

     let filtre_knuth reponse possibles =
          let proposition = fst(reponse) and rep = snd(reponse) in
               let res = supprime_element possibles proposition in
                    let res = filtre_knuthRT res proposition rep [] in
                         res;;

     let filtre methode reponse possibles =
          match methode with
               | 0 -> let res = filtre_naif reponse possibles in res
               | _ -> let res = filtre_knuth reponse possibles in res;;

end ;;

let methode_naif tour courant reponse =
     if tour = 1 then
          let proposition = IA.choix 0 [] courant in
               let courant = supprime_element (Code.tous) proposition in
                    (proposition,courant)
     else
          let courant = IA.filtre 0 reponse courant in
               let proposition = IA.choix 0 [] courant in
                    (proposition,courant)

let methode_knuth tour courant reponse  =
     if tour = 1 then
          let proposition = [1;1;2;2] in
               let courant = supprime_element (Code.tous) proposition in
                (proposition,courant)
     else
          let courant = IA.filtre 1 reponse courant in
               let proposition = IA.choix 1 [fst(reponse)] courant in
                    (proposition,courant)
