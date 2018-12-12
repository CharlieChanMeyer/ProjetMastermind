(** Algorithmes de la methode naif *)
#use "code.ml";;

module Naif :
     sig
     (** Calcule aleatoirement une proposition a donner
       * @param possible la liste courante des codes possibles
       * @return un code aleatoire parmis tous les codes possibles
       *)
     val per_p : Code.t list -> Code.t

     (** Supprime un element de la liste
       * @param liste liste des codes courants
       * @param element element de la liste courante à supprimer
       * @return la liste sans l'element à supprimer
       *)
     val supprime_element : Code.t list -> Code.t -> Code.t list

     (** Supprime des elements de la liste en accord avec la reponse obtenue
       * @param liste liste des codes courants
       * @param element element de la liste courante dont au moins un pion est "valide"
       * @return la liste sans les elements à supprimer
       *)
     val supprime_couleur : Code.t list -> Code.t -> Code.t list

     (** Supprime des elements de la liste en accord avec la reponse obtenue
       * @param liste liste des codes courants
       * @param element element de la liste courante dont les 4 pions sont "valide"
       * @return la liste sans les elements à supprimer
       *)
     val supprime_code4 : Code.t list -> Code.t -> Code.t list

     end=
     struct

     let per_p possibles=
          let tailles = List.length possibles in
               List.nth possibles (Random.int tailles);;

     let rec supprime_elementRT liste element acc=
          match (liste) with
               | []  ->  acc
               | (n :: suite) when (List.for_all2 (fun x y -> x=y) n element) -> supprime_elementRT suite element acc
               | (n :: suite)                                                 -> supprime_elementRT suite element (acc @ [n]);;

     let supprime_element liste element = supprime_elementRT liste element [];;

     let rec supprime_couleurRT liste element acc=
          match (liste) with
               | [] -> acc
               | (n :: suite) when (List.exists2 (fun x y -> x=y) n element) -> supprime_couleurRT suite element (acc @ [n])
               | (n :: suite)                                                -> supprime_couleurRT suite element acc;;

     let supprime_couleur liste element = supprime_couleurRT liste element [];;

     let rec search acc2 liste element acc =
          match (liste) with
               | [] -> acc
               | (n :: suite) when (n = element) -> acc
               | (n :: suite)                    -> search acc2 suite element (acc+1);;

     let rec modifie liste nth acc res=
          match (liste) with
               | [] -> res
               | (n :: suite) when (n=1)       -> modifie suite nth (acc+1) (res @ [1])
               | (n :: suite) when (acc = nth) -> modifie suite nth (acc+1) (res @ [1])
               | (n :: suite)                  -> modifie suite nth (acc+1) (res @ [0]);;

     let rec creer_liste_0 acc liste=
          if (acc=Code.nombre_pions) then
               liste
          else
               creer_liste_0 (acc+1) (liste @ [0]);;

     let rec verif2RT mp n etat acc =
          match (n) with
               | [] -> (false,etat)
               | (np :: suite) -> (if ((mp=np)&& (List.nth etat acc)=0) then
                                        let etat = modifie etat acc 0 [] in
                                             (true,etat)
                                   else
                                        verif2RT mp suite etat (acc+1));;

     let rec verifRT element n etat acc=
          match (element) with
               | [] -> acc
               | (mp :: suite) -> (let (valeur,etatp) = verif2RT mp n etat 0 in
                                        if valeur then
                                             let etat = etatp in
                                                  verifRT suite n etat acc
                                        else
                                             false);;

     let verif element n =
          let acc2 = creer_liste_0 0 [] in
               verifRT element n acc2 true ;;

     let supprime_code4 liste element = let res = List.filter (fun x -> verif element x) liste in supprime_element res element;;

end;;
