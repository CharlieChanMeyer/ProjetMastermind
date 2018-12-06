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

     let per_p possible=
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

     let rec supprime_code4RT liste element acc =
          match (liste) with
               | [] -> acc
               | (n :: suite) when List.for_all (fun x -> List.exists (fun y -> x=y) element) n -> supprime_code4RT suite element (acc @ [n])
               | (n :: suite)                                                                   -> supprime_code4RT suite element acc;;

     let supprime_code4 liste element = supprime_code4RT liste element [];;

end;;
