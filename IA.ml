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
          match (methode) with               (*Vérifie la méthode choisis*)
               | 0 -> List.nth (possibles) 0      (*Si méthode naïve, retourne le première élément de la liste*)
               | _ -> (
                    match (List.length possibles) with      (*Sinon, vérifie la taille de la liste des éléments courants*)
                         | 1 -> (List.nth possibles 0)      (*Si la taille est égale à 1, alors retourne le seul élément présent*)
                         | _ -> (
                              let rep = Code.reponse (List.hd possibles) (List.hd(List.rev essais)) in   (*Sinon, calcule la réponse obtenue entre le premier élement de la liste et le dernier élement proposé*)
                                   let res = Knuth.liste_codes_meme_score_maximum possibles rep in  (*Calcule la liste des élements enlevant le plus de code possibles*)
                                        Knuth.mini (List.tl res) (List.hd res)) (*Retourne le code le plus petit possible*)
                    );;


     let filtre_naif reponse possibles =
          let proposition = fst(reponse) and rep = snd(reponse) and pions = Code.nombre_pions in    (*Définis les variables temporaires de la fonctions*)
               match (rep) with
                    | Some(pions,0) -> [proposition] (*Si la réponse précedente est Some(4,0), alors retourne la proposition*)
                    | Some(n1,n2) when (n1+n2 = pions) -> let res = Naif.supprime_code4 possibles proposition in res  (*Si la réponse précedente est ~pions, alors filtre selon supprime_code4*)
                    | Some(n1,n2)                  -> let res = Naif.supprime_couleur possibles proposition (n1+n2) in res (*Sinon, filtre selon supprime_couleur*)
                    | None -> [];; (*Si le code n'était pas valide, retourne une liste vide*)

     let rec filtre_knuthRT courant proposition rep acc =
          match (courant) with
               | [] -> acc         (*Si la liste des courants est vide, retourne l'accumulateur*)
               | _  -> (
                    let test = List.hd courant in                (*Prends en variable la tête de la liste*)
                         match (Code.reponse test proposition) with  (*Match la réponse obtenue entre la tête et la dernière proposition*)
                              | Some(testbp,testmp) -> (
                                   match (rep) with
                                        | Some(propbp,propmp) -> (
                                             if ((testbp=propbp)&&(testmp=propmp) || (testbp = Code.nombre_pions)) then  (*Si la réponse obtenue lors de la dernière tentative est égale à la réponse que l'on vient de calculer*)
                                                  filtre_knuthRT (List.tl courant) proposition rep (acc @ [test]) (*relance la fonction sur la suite de la liste courante en ajoutant l'élement testé à l'accumulateur*)
                                             else
                                                  filtre_knuthRT (List.tl courant) proposition rep acc (*Sinon, relance la fonction sur la suite de la liste courante sans toucher à l'accumulateur*)
                                             )
                                        | None -> filtre_knuthRT courant proposition rep acc
                                   )
                              | None -> filtre_knuthRT courant proposition rep acc
                    );;

     let filtre_knuth reponse possibles =
          let proposition = fst(reponse) and rep = snd(reponse) in (*Définie des variables temporaires*)
               let res = filtre_knuthRT possibles proposition rep [] in (*Filtre selon la methode de KNUTH*)
                    let res = supprime_element res proposition in          (*Supprime l'élement proposé lors de la dernière tentative de la liste des courants*)
                         res;;          (*Retourne le résultat*)

     let filtre methode reponse possibles =
          match methode with       (*Filtre selon la méthode choisie*)
               | 0 -> let res = filtre_naif reponse possibles in res
               | _ -> let res = filtre_knuth reponse possibles in res;;

end ;;

let methode_naif tour courant reponse =
     if tour = 1 then         (*Si le tour de jeu est 1*)
          let proposition = IA.choix 0 [] courant in        (*Choisis un code à proposé*)
               let courant = supprime_element (Code.tous) proposition in (*Enlève l'élement à proposer de la liste des codes courants*)
                    (proposition,courant) (*Retourne un couple proposition,courant*)
     else
          let courant = IA.filtre 0 reponse courant in           (*Sinon, filtre la liste des courants en fonction de la réponse obtenue au dernier tour*)
               let proposition = IA.choix 0 [] courant in (*Choisis un code à proposer parmis la nouvelle liste courante*)
                    (proposition,courant)    (*Retourne un couple proposition,courant*)

let methode_knuth tour courant reponse  =
     if tour = 1 then          (*Si le tour de jeu est 1*)
          let proposition = [1;1;2;2] in               (*Propose le code [1;1;2;2] selon la méthode de KNUTH*)
               let courant = supprime_element (Code.tous) proposition in        (*Enlève l'élement à proposer de la liste des codes courants*)
                (proposition,courant)   (*Retourne un couple proposition,courant*)
     else
          let courant = IA.filtre 1 reponse courant in (*Sinon, filtre la liste des courants en fonction de la réponse obtenue au dernier tour*)
               let proposition = IA.choix 1 [fst(reponse)] courant in (*Choisis un code à proposé*)
                    (proposition,courant)    (*Retourne un couple proposition,courant*)
