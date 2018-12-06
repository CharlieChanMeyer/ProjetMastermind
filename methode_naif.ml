(** Algorithmes de la methode naif *)
#use "code.ml";;

module Naif :
     sig
     (** Calcule aleatoirement la premiere proposition a donner*)
     val per_p : Code.t

     (** Supprime un element de la liste
       * @param liste liste des codes courants
       * @param element element de la liste courante à supprimer
       * @return la liste sans l'element à supprimer
       *)
     val supprime : Code.t list -> Code.t -> Code.t list

     (**Donne la liste des codes courants (sans filtrage) après la première proposition*)
     val liste_perp : Code.t list

     end=
     struct

     let per_p =
          let possibles = Code.tous in
               let tailles = List.length possibles in
                    List.nth possibles (Random.int tailles);;

     let rec supprimeRT liste element acc=
          match (liste) with
               | []  ->  acc
               | (n :: suite) when (List.for_all2 (fun x y -> x=y) n element) -> supprimeRT suite element acc
               | (n :: suite)                                                 -> supprimeRT suite element (acc @ [n]);;

     let supprime liste element = supprimeRT liste element [];;

     let liste_perp = supprime (Code.tous) per_p;;

end;;
