#load "str.cma";;
#load "unix.cma";;

(** Module de definition d'un code dans le jeu Mastermind *)
module Code =
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


      let toutes_reponses = List.filter(fun (x,y) ->  (x+y<=nombre_pions) && ((x,y)<>(nombre_pions-1,1)) ) (list_couples nombre_pions);;

     let nb_fois_present element vrai_code =
        let rec nb_fois_presentAcc element vrai_code acc =
          match vrai_code with
            |[]->acc
            |h :: t when h=element -> nb_fois_presentAcc element t (acc+1)
            |h :: t when h<>element -> nb_fois_presentAcc element t acc

        in nb_fois_presentAcc element vrai_code 0;;

     let reponseBienPlace code vrai_code =
        let rec reponseBienPlaceAcc code vrai_code acc =
          match code with
          | [] -> acc
          | h :: t when h=(List.hd vrai_code) -> reponseBienPlaceAcc t (List.tl vrai_code) (acc+1)
          | h :: t -> reponseBienPlaceAcc t (List.tl vrai_code) acc 

      in reponseBienPlaceAcc code vrai_code 0;;


      let replace_nth l elem pos =
        let rec replace_nthAcc l elem pos acc acc_liste =
          match l with
          | [] -> acc_liste
          | h :: t when acc=pos -> replace_nthAcc t elem pos (acc+1) acc_liste@[elem] 
          | h :: t -> replace_nthAcc t elem pos (acc+1) (acc_liste)@[h]

        in let new_l= replace_nthAcc l elem pos 0 [] in 
              List.rev new_l;;



(* Prend le code choisi et le code a trouver en parametres, s il y a des pions bien places : 
  remplace dans les positions correspondantes par des (-1) dans le vrai_code *)
      let replace_bien_place code vrai_code = 
        let rec replace_bien_placeRT code vrai_code acc =
          match code with
          | [] -> acc
          | h :: t when h = (List.hd vrai_code) -> replace_bien_placeRT t (List.tl vrai_code) acc@[(-1)]
          | h :: t ->replace_bien_placeRT t (List.tl vrai_code) acc@[List.hd vrai_code] 

        in let new_l = replace_bien_placeRT code vrai_code []
            in List.rev new_l;;


        let copie_moins_un code vrai_code_modif =
          let rec copie_moins_unRT code vrai_code_modif acc =
            match code with
            | [] -> acc
            | h :: t when (List.hd vrai_code_modif)=(-1) -> copie_moins_unRT t (List.tl vrai_code_modif) acc@[(-1)]
            | h :: t -> copie_moins_unRT t (List.tl vrai_code_modif ) acc@[h]

          in let new_l = copie_moins_unRT code vrai_code_modif [] in
            List.rev new_l;;


      let pos_premier_elem_rencontre liste element =
        let rec pos_premier_elem_rencontreRT liste element acc =
        match liste with
        | h :: t when element = h -> acc
        | _ -> pos_premier_elem_rencontreRT (List.tl liste) element (acc+1)
      in pos_premier_elem_rencontreRT liste element 0;;



     let reponseMalPlace code vrai_code =
        let vrai_code_modif = replace_bien_place code vrai_code in
          let code_modif = copie_moins_un code vrai_code_modif in
        let rec reponseMalPlaceAcc code_modif vrai_code_modif acc pos =
          match code_modif with
          | [] -> acc
          | h :: t when h=(List.nth vrai_code_modif pos) -> reponseMalPlaceAcc t (replace_nth vrai_code_modif (-1) pos) acc (pos+1) 
          | h :: t when (nb_fois_present h vrai_code_modif)<>0 -> reponseMalPlaceAcc t (replace_nth vrai_code_modif (-1) (pos_premier_elem_rencontre vrai_code_modif h)) (acc+1) (pos+1) 
          | h :: t when (nb_fois_present h vrai_code_modif)=0 -> reponseMalPlaceAcc t vrai_code_modif acc (pos+1) 

        in reponseMalPlaceAcc code_modif vrai_code_modif 0 0;; 


      let existence_valeur_negative code = List.exists (fun x -> x<0) code;;

     let reponse code vrai_code = 
        match code with
        | l when List.length l <> List.length vrai_code -> None
        | l when existence_valeur_negative l -> None
        | _ -> Some((reponseBienPlace code vrai_code), (reponseMalPlace code vrai_code)) ;;

     end;;
