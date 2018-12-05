(** Algorithmes de la methode naif *)
#use "code.ml"

module Naif :
     sig
     end=
     struct
     (** Calcule aleatoirement la premiere proposition a donner*)
     let per_p =
          let possibles = Code.tous in
               let taille = List.length possibles in
                    List.nth (Random.int tailles) possibles;; 



end;;
