(** Algorithmes de la methode naif *)
#use "code.ml";;

(** Supprime un element de la liste
 * @param liste liste des codes courants
 * @param element element de la liste courante à supprimer
 * @param acc accumulateur qui garde en mémoire la nouvelle liste
 * @return la liste sans l'element à supprimer
 *)
let rec supprime_elementRT liste element acc=
      match (liste) with
           | []  ->  acc
           | (n :: suite) when (List.for_all2 (fun x y -> x=y) n element) -> supprime_elementRT suite element acc
           | (n :: suite)                                                 -> supprime_elementRT suite element (acc @ [n]);;
(** Supprime un element de la liste
  * @param liste liste des codes courants
  * @param element element de la liste courante à supprimer
  * @return la liste sans l'element à supprimer
  *)
let supprime_element liste element = supprime_elementRT liste element [];;

module Naif :
     sig
     (** Supprime des elements de la liste en accord avec la reponse obtenue
       * @param liste liste des codes courants
       * @param element element de la liste courante dont au moins un pion est "valide"
       * @return la liste sans les elements à supprimer
       *)
     val supprime_couleur : Code.t list -> Code.t -> int -> Code.t list

     (** Supprime des elements de la liste en accord avec la reponse obtenue
       * @param liste liste des codes courants
       * @param element element de la liste courante dont les 4 pions sont "valide"
       * @return la liste sans les elements à supprimer
       *)
     val supprime_code4 : Code.t list -> Code.t -> Code.t list

     end=
     struct

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

     let rec verifRT element n etat=
          match (element) with
               | [] -> true
               | (mp :: suite) -> (let (valeur,etatp) = verif2RT mp n etat 0 in
                                        if valeur then
                                                  verifRT suite n etatp
                                        else
                                             false);;

     let verif element n =
          let acc2 = creer_liste_0 0 [] in
               verifRT element n acc2;;

     let rec verif_etat etat acc =
          match (etat) with
               | []           -> acc
               | (0 :: suite) -> verif_etat suite acc
               | _ -> verif_etat (List.tl etat) (acc+1);;

     let rec verifRT_couleur element n nc etat=
          match (verif_etat etat 0,element) with
               | (nb,_) when (nb=nc) -> true
               | (_,[])              -> false
               | (_,mp :: suite)     -> (
                    let (_,etatp) = verif2RT mp n etat 0 in
                              verifRT_couleur suite n nc etatp
                    );;

     let verif_couleur element n nc =
          let acc2 = creer_liste_0 0 [] in
               verifRT_couleur element n nc acc2;;

     let supprime_couleur liste element nc = let res = List.filter (fun x -> verif_couleur element x nc) liste in supprime_element res element;;

     let supprime_code4 liste element = let res = List.filter (fun x -> verif element x) liste in supprime_element res element;;

 end;;

(** Algorithmes de la methode knuth *)
module Knuth (*:
      sig
      end*)=
      struct

      (** Verifie si la valeur du premier parametre est plus petite que celle du second  
        * @param test une liste d entiers (un code) 
        * @param minimum une liste d entiers (un code)
        * @return un booleen : true si test est plus petit que minimum, false sinon
        *)
      let rec verif test minimum =
           match (test,minimum) with
                | ([],[])                                         -> true
                | (t :: tsuite,m :: msuite) when (t > m)          -> false
                | (t :: tsuite,m :: msuite) when (m > t)          -> true
                | (_,_) when ((List.hd test) = (List.hd minimum)) -> verif (List.tl test) (List.tl minimum)

      (** Calcule le plus petit code d'une liste de codes 
        * @param possibles une liste de listes d entiers (une liste de codes)
        * @param acc une liste d entiers (un code)
        * @return une liste d entiers (un code)  
        *)
      let rec mini possibles acc =
           match (possibles) with
                | [] -> acc
                | _  -> (
                     let test = List.hd possibles and suite = List.tl possibles in
                          if (verif test acc) then
                               mini suite test
                          else
                               mini suite acc
                     );;
    (** Calcule le maximum d une liste d entiers
      * @param liste une liste d entiers
      * @return un entier qui est la plus grande valeur contenue dans cette liste d entiers 
      *)
     let max_liste liste =
          let rec max_listeRT liste acc =
               match liste with
                    | [] -> acc
                    | h :: t when (h > acc) -> max_listeRT t h
                    | _ -> max_listeRT (List.tl liste) acc

          in max_listeRT liste 0 ;;

     (** Calcule score d un code par rapport a une liste de codes selon la reponse c est a dire le nombre de codes que le code rentre en second parametre peut eliminer de la liste de codes pour reduire la taille de la liste pour la prochaine tentative
       * @param liste_codes une liste de listes d entiers 
       * @param code le code propose : une liste d entiers
       * @param couple_reponse le couple de pions (BienPlaces, MalPlaces) 
       * @return le score du code
       *)
     let calcule_score liste_codes code couple_reponse =
          let rec calcule_scoreRT liste_codes code couple_reponse acc =
               match liste_codes with
                    | []                                               -> acc
                    | h :: t when ((Code.reponse code h) <> couple_reponse) -> calcule_scoreRT t code couple_reponse (acc+1)
                    | _                                                -> calcule_scoreRT (List.tl liste_codes) code couple_reponse acc

          in calcule_scoreRT liste_codes code couple_reponse 0 ;;

    (** Calcule la liste contenant le score de chaque code appartenant a une liste de codes selon une certaine reponse 
      * @param liste_codes une liste de listes d entiers (une liste de codes)
      * @param couple_reponse le couple de pions (BienPlaces, MalPlaces)
      * @return une liste des scores de chaque code de la liste_codes 
     *)
     let calcule_liste_score liste_codes couple_reponse =
          let rec calcule_liste_scoreRT liste_codes couple_reponse pos acc_liste =
               match liste_codes with
                    | h :: t when (pos+1) = List.length liste_codes -> acc_liste
                    | _                                             -> calcule_liste_scoreRT liste_codes couple_reponse (pos+1) (acc_liste@[calcule_score liste_codes (List.nth liste_codes (pos+1)) couple_reponse])

          in calcule_liste_scoreRT liste_codes couple_reponse (-1) [] ;;

    (** Calcule la liste contenant les codes ayant fait le score maximum de la liste des scores calculée avec la fonction 'calcule_liste_score'
      * @param liste_codes une liste de listes d'entiers (une liste de codes)
      * @param couple_reponse le couple de pions (BienPlaces, MalPlaces)
      * @return une liste de codes ayant le plus grand score
      *)

     let liste_codes_meme_score_maximum liste_codes couple_reponse =
          let maximum_liste_score = max_liste ( calcule_liste_score liste_codes couple_reponse) in  (* on definit la valeur du score maximum *)
               let rec liste_codes_meme_score_maximumRT liste_codes couple_reponse pos acc =
                    match liste_codes with
                         | h :: t when (pos+1) = List.length liste_codes                                                            -> acc         (* Condition d'arrêt : lorsqu'on atteint la dernière position de la liste *)
                         | h :: t when (maximum_liste_score = (calcule_score liste_codes (List.nth liste_codes pos) couple_reponse))-> liste_codes_meme_score_maximumRT liste_codes couple_reponse (pos+1) (acc@[(List.nth liste_codes pos)])
                         | _                                                                                                        -> liste_codes_meme_score_maximumRT liste_codes couple_reponse (pos+1) acc

          in liste_codes_meme_score_maximumRT liste_codes couple_reponse 0 [] ;;

 end;;
