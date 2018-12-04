(** Module de definition d'un code dans le jeu Mastermind *)
module Code :
     sig
     (** Le type d'un pion *)
     type pion = int

     (** Le type d'un code *)
     type t = pion list

     (** Nombre de pions par code *)
     val nombre_pions : int

     (** Liste des couleurs possibles *)
     val couleurs_possibles : pion list

     (** Compare deux codes
       * @param code1 premier code a comparer
       * @param code2 second code a comparer
       * @return 0 si les deux codes sont identiques,
                 un entier positif si [code1] est strictement plus grand que [code2]
                 un entier negatif si [code1] est strictement plus petit que [code2]
       *)
     val compare : t -> t -> int

     (** Conversion code vers chaine de caracteres (pour affichage)
       * @param code code a convertir
       * @return la representation en chaine de caracteres de [code]
       *)
     val string_of_code : t -> string

     (** Conversion chaine de caracteres vers code (pour saisie)
       * @param string chaine de caractere saisie
       * @return le code correspondant a la saisie si la conversion est possible
                 [None] si la conversion n'est pas possible
       *)
     val code_of_string : string -> t option

     (** La liste de tous les codes permis *)
     val tous : t list

     (** La liste de toutes les reponses possibles *)
     val toutes_reponses : (int * int) list ;;

     (** Calcule la reponse d'un code par rapport au code cache
       * @param code le code propose
       * @param vrai_code le code cache
       * @return un couple (nombre de pions bien places, nombre de pions mal places)
                 [None] si la reponse ne peut etre calculee
       *)
     val reponse : t -> t -> (int * int) option

     end =
     struct

     type pion = int;;

     type t = pion list;;

     let nombre_pions = 4;;

     let couleurs_possibles = [1;2;3;4;5;6];;

     let cmp n1 n2 = (n1 - n2);;

     let rec liste_search liste mb acc =
          match liste with
               | (mp :: suite) when (mp = mb) -> acc
               | _                            -> liste_search (List.tl liste) mb acc+1;;

     let compare code_p code_s =
          List.fold_left (fun acc x -> cmp x (List.nth code_s (liste_search code_p x 0))) 0 code_p;;


     let int2color valeur = 
        match valeur with
          |1->"Rouge"
          |2->"Bleu"
          |3->"Vert"
          |4->"Jaune"
          |5->"Violet"
          |6->"Blanc"
          |_->"pas une couleur";;



     let string_of_code liste_pion = List.fold_left (fun acc elem -> acc^(int2color elem)^" | ") " | " liste_pion;;


     end;;
