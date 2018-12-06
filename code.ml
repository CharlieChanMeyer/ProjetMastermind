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

     (** Ligne de commande a rentrer avant cette fonction pour qu elle fonctionne :   #load "str.cma";; *)
     let split_code str = Str.split (Str.regexp "|") str;;


     let color2int color =
        match color with
            |"Rouge"->1
            |"Bleu"->2
            |"Vert"->3
            |"Jaune"->4
            |"Violet"->5
            |"Blanc"->6
            |_->(-1);;


     let code_of_string str =
        let list_split = (split_code str) in
          let rec code_of_stringRT list_split acc =
            match list_split with
              |[]->Some(acc)
              |h :: t -> if color2int h = -1 then
                            None
                          else
                            code_of_stringRT t (acc@([color2int h]))

      in code_of_stringRT list_split [];;

     let rec tous_t acc pos =
               if (pos = nombre_pions) then
                    acc
               else
                    tous_t (List.fold_left (fun acc1 x -> acc1 @ List.fold_left (fun acc2 y -> acc2 @ [y @ [x]]) [] acc) [] couleurs_possibles) (pos+1);;

     let tous = tous_t [[]] 0;;

      let list_couples nb_pions =
         let rec list_couples_RT nb_pions acc acc2 liste_acc =
            match acc with
              |valeur when valeur <= (nb_pions-acc2) -> list_couples_RT nb_pions (acc+1) acc2 (liste_acc@[(acc2,valeur)])
              |valeur when (acc2<nb_pions) ->let acc =(-1) in list_couples_RT nb_pions (acc+1) (acc2+1) (liste_acc@[(acc2,valeur)])
              |_->liste_acc

        in list_couples_RT nb_pions 0 0 [];;


      let toutes_reponses nb_pions = List.filter(fun (x,y) -> x+y<=nb_pions) (list_couples nb_pions);;






     end;;
