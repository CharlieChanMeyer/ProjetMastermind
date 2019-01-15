#load "str.cma";;
#load "unix.cma";;

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

     let nombre_pions = 4;; (* Variable qui definit le nombre de pions par code *)

     let couleurs_possibles = [1;2;3;4;5;6];; (* Permet de choisir le nombre de couleurs, allant jusqu'à 9 *)

     let cmp n1 n2 = (n1 - n2);;


     (** Renvoie la position du premier element cherche
       * @param liste la liste dans laquelle on cherche l element
       * @param mb l entier recherche
       * @param acc accumulateur qui doit etre initialise a 0
       * @return un entier qui est la position de l element 
       *)
     let rec liste_search liste mb acc =
          match liste with
               | (mp :: suite) when (mp = mb) -> acc
               | _                            -> liste_search (List.tl liste) mb acc+1;;

     let compare code_p code_s =
          List.fold_left (fun acc x -> cmp x (List.nth code_s (liste_search code_p x 0))) 0 code_p;;

     (** Renvoie la couleur correspondant a l entier en parametre
       * @param valeur entier saisi
       * @return une chaine de caracteres selon l entier
                 un message d erreur "pas une couleur" si l entier n est pas correct
       *)
     let int2color valeur =
        match valeur with
          |1->"Rouge"
          |2->"Bleu"
          |3->"Vert"
          |4->"Jaune"
          |5->"Violet"
          |6->"Blanc"
          |7->"Orange"
          |8->"Rose"
          |9->"Marron"
          |_->"pas une couleur";;



     let string_of_code liste_pion = List.fold_left (fun acc elem -> acc^(int2color elem)^"|") "" liste_pion;;

     (* Ligne de commande a rentrer avant la fonction 'split_code' pour qu elle fonctionne :   #load "str.cma";; *)

     (** Renvoie une liste de plusieurs chaines de caracteres separees par le caractere | 
       * @param str une chaine de caractere representant le code 
       * @return une liste de chaines de caracteres    
     *)
     let split_code str = Str.split (Str.regexp "|") str;;

     (** Renvoie l entier correspondant a la chaine de caracteres (couleur) en parametre
       * @param color couleur saisie
       * @return un entier selon la couleur 
                 (-1) si la couleur saisie n est pas correcte
       *)
     let color2int color =
        match color with
            |"Rouge"->1
            |"Bleu"->2
            |"Vert"->3
            |"Jaune"->4
            |"Violet"->5
            |"Blanc"->6
            |"Orange"->7
            |"Rose"->8
            |"Marron"->9
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

     (** Renvoie la liste des couples de toutes les reponses possibles selon le nombre de pions a quelques couples pres
       * @param nb_pions le nombre de pions par code
       * @return une liste de couples contenant les reponses possibles selon le nombre de pions
       *)
      let list_couples nb_pions =
         let rec list_couples_RT nb_pions acc acc2 liste_acc =
            match acc with
              |valeur when valeur <= (nb_pions-acc2) -> list_couples_RT nb_pions (acc+1) acc2 (liste_acc@[(acc2,valeur)])
              |valeur when (acc2<nb_pions) ->let acc =(-1) in list_couples_RT nb_pions (acc+1) (acc2+1) (liste_acc@[(acc2,valeur)])
              |_->liste_acc

        in list_couples_RT nb_pions 0 0 [];;


      let toutes_reponses = List.filter(fun (x,y) ->  (x+y<=nombre_pions) && ((x,y)<>(nombre_pions-1,1)) ) (list_couples nombre_pions);; (* Supprime les codes de la forme (pions_bien_placés-1,1) 
                                                                                                                                          car par exemple si le nombre de pions est 4, le code (3,1) n'est pas valide comme le code (8,2) puisque 8+2>4 *)
     (** Renvoie le nombre de fois qu un element est present dans le code en parametre 
       * @param element entier recherche dans la liste 
       * @param vrai_code liste d entiers
       * @return l entier correspondant a l occurence de l element dans cette liste
       *)                                                                                                                                    
     let nb_fois_present element vrai_code =
        let rec nb_fois_presentAcc element vrai_code acc =
          match vrai_code with
            |[]->acc
            |h :: t when h=element -> nb_fois_presentAcc element t (acc+1)
            |h :: t -> nb_fois_presentAcc element t acc

        in nb_fois_presentAcc element vrai_code 0;;


      (** Renvoie le nombre de pions bien places 
        * @param code le code propose 
        * @param vrai_code le code secret qui doit etre devine 
        * @return un entier correspondant au nombre de pions bien places dans le premier code par rapport au code secret
        *)
     let reponseBienPlace code vrai_code =
        let rec reponseBienPlaceAcc code vrai_code acc =
          match code with
          | [] -> acc
          | h :: t when h=(List.hd vrai_code) -> reponseBienPlaceAcc t (List.tl vrai_code) (acc+1)
          | h :: t -> reponseBienPlaceAcc t (List.tl vrai_code) acc

      in reponseBienPlaceAcc code vrai_code 0;;

      (** Remplace un element par un autre a une certaine position dans une liste rentres en parametres
        * @param l une liste d entiers
        * @param elem un entier 
        * @param pos la position a laquelle on souhaite remplacer l element actuel par elem
        * @return une liste dont l element a la position pos a ete remplace par elem
        *)
      let replace_nth l elem pos =
        let rec replace_nthAcc l elem pos acc acc_liste =
          match l with
          | [] -> acc_liste
          | h :: t when acc=pos -> replace_nthAcc t elem pos (acc+1) acc_liste@[elem]
          | h :: t -> replace_nthAcc t elem pos (acc+1) (acc_liste)@[h]

        in let new_l= replace_nthAcc l elem pos 0 [] in
              List.rev new_l;;



      (** Renvoie le code secret modifie avec des (-1) dans les positions correspondant aux pions bien places
        * @param code le code propose
        * @param vrai_code le code secret a deviner
        * @return une liste d entiers qui est le code secret et s il y a des pions bien places : remplace dans les positions correspondantes par des (-1)  
        *)
      let replace_bien_place code vrai_code =
        let rec replace_bien_placeRT code vrai_code acc =
          match code with
          | [] -> acc
          | h :: t when h = (List.hd vrai_code) -> replace_bien_placeRT t (List.tl vrai_code) acc@[(-1)]
          | h :: t ->replace_bien_placeRT t (List.tl vrai_code) acc@[List.hd vrai_code]

        in let new_l = replace_bien_placeRT code vrai_code []
            in List.rev new_l;;

       (** Prend deux codes en parametres et renvoie le premier code avec des (-1) aux memes positions que celles ou se situent les (-1) dans le second code 
         * @param code le code propose
         * @param vrai_code_modif code secret modifie par la fonction 'replace_bien_place'
         * @return le premier code modifie avec des (-1) a l endroit ou les pions sont bien places
         *)
        let copie_moins_un code vrai_code_modif =
          let rec copie_moins_unRT code vrai_code_modif acc =
            match code with
            | [] -> acc
            | h :: t when (List.hd vrai_code_modif)=(-1) -> copie_moins_unRT t (List.tl vrai_code_modif) acc@[(-1)]
            | h :: t -> copie_moins_unRT t (List.tl vrai_code_modif ) acc@[h]

          in let new_l = copie_moins_unRT code vrai_code_modif [] in
            List.rev new_l;;

       (** Renvoie la position de la premiere occurence d un element dans une liste
         * @param liste une liste d entiers (ici un code)
         * @param element l entier dont la position est recherchee
         * @return un entier qui est la position de la premiere occurence de l element en parametres
         *)
      let pos_premier_elem_rencontre liste element =
        let rec pos_premier_elem_rencontreRT liste element acc =
        match liste with
        | h :: t when element = h -> acc
        | _ -> pos_premier_elem_rencontreRT (List.tl liste) element (acc+1)
      in pos_premier_elem_rencontreRT liste element 0;;


     (** Renvoie le nombre de pions mal places 
       * @param code le code propose 
       * @param vrai_code le code secret qui doit etre devine 
       * @return un entier correspondant au nombre de pions bien places dans le premier code par rapport au code secret
       *)
     let reponseMalPlace code vrai_code =
        let vrai_code_modif = replace_bien_place code vrai_code in    (* modifie le vrai_code en changeant les pions bien places par (-1) pour eviter de les recompter lors du calcul des pions mal places*)
          let code_modif = copie_moins_un code vrai_code_modif in    (* même traitement que pour vrai_code, on place (-1) aux mêmes endroits que dans vrai_code_modif *)
        let rec reponseMalPlaceAcc code_modif vrai_code_modif acc pos =
          match code_modif with
          | [] -> acc
          | h :: t when h=(List.nth vrai_code_modif pos) -> reponseMalPlaceAcc t (replace_nth vrai_code_modif (-1) pos) acc (pos+1)
          | h :: t when (nb_fois_present h vrai_code_modif)<>0 -> reponseMalPlaceAcc t (replace_nth vrai_code_modif (-1) (pos_premier_elem_rencontre vrai_code_modif h)) (acc+1) (pos+1)
          | h :: t -> reponseMalPlaceAcc t vrai_code_modif acc (pos+1)                  (* si la valeur de la tête de la liste n'est presente nulle part dans le code secret, alors on ne change pas l'accumulateur et on avance dans la liste en incrémentant l'accumulateur*)

        in reponseMalPlaceAcc code_modif vrai_code_modif 0 0;;

      (** Verifie s il y a des valeurs negatives dans une liste d entiers
        * @param code une liste d entiers
        * @return true s il y a au moins une valeur negative dans la liste sinon renvoie false
        *)
      let existence_valeur_negative code = List.exists (fun x -> x<0) code;; 

     let reponse code vrai_code =
        match code with
        | l when List.length l <> List.length vrai_code -> None                      (* reponse non calculable si les deux codes ne font pas la même longueur *)
        | l when existence_valeur_negative l -> None                      (* reponse non calculable s'il y a des valeurs negatives dans le code propose *)
        | _ -> Some((reponseBienPlace code vrai_code), (reponseMalPlace code vrai_code)) ;;


     end;;