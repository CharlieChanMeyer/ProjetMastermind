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
       * @param nc le nombre de pions biens et mal placés lors de la dernière proposition
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
     (** Modifie la liste rentrée en paramètre
       * @param liste liste d'état de la vérification du code
       * @param nth numéro de la position à modifier
       * @param acc position actuelle de la fonction dans la liste à modifier
       * @param res accumulateur contenant la nouvelle liste
       * @return la liste avec un 1 supplémentaire à la position demandée
       *)
     let rec modifie liste nth acc res=
          match (liste) with
               | [] -> res              (*Si la liste est vide, retourne le résultat*)
               | (n :: suite) when (n=1)       -> modifie suite nth (acc+1) (res @ [1])             (*Si la liste possédait déjà un 1 à cet emplacement, relance la fonction sur la suite de la liste en gardant le 1*)
               | (n :: suite) when (acc = nth) -> modifie suite nth (acc+1) (res @ [1])             (*Si la position actuelle de la fonction est la position à modifier, relance la fonction sur la suite de la liste en ajoutant un 1*)
               | (n :: suite)                  -> modifie suite nth (acc+1) (res @ [0]);;           (*Sinon, relance la fonction sur la liste de la suite en laissant le 0*)

     (** Crée une liste de zeros
       * @param acc la "taille" actuelle de la liste de zeros en cours de création
       * @param liste la liste de zeros en cours de création
       * @return la liste de zeros créées
       *)
     let rec creer_liste_0 acc liste=
          if (acc=Code.nombre_pions) then
               liste               (*Si la liste a la taille souhaitée, retourne la liste*)
          else
               creer_liste_0 (acc+1) (liste @ [0]);;        (*Sinon, relance la fonction en ajoutant 1 à sa taille et un nouveau 0 à la liste*)
     (** Vérifie si l'élément actuel de la dernière proposition est contenu dans la liste en cours de vérification
       * @param mp élément de la dernière proposition actuellement en cours de vérification
       * @param n liste en cours de vérification
       * @param etat liste de l'état actuel de la vérification
       * @param acc la position actuelle de la fonction dans la liste
       * @return un couple contenant si la liste vérifie la condition et l'état de la vérification
       *)
     let rec verif2RT mp n etat acc =
          match (n) with
               | [] -> (false,etat)          (*Si on arrive à la fin de la liste sans avoir trouvé de correspondance, retourne faux*)
               | (np :: suite) -> (if ((mp=np)&& (List.nth etat acc)=0) then    (*S'il y a correspondance et que la position n'a pas déjà été vérifiée*)
                                        let etat = modifie etat acc 0 [] in     (*Modifie l'état puis retourne le couple réponse*)
                                             (true,etat)
                                   else
                                        verif2RT mp suite etat (acc+1));;       (*Sinon, relance la vérification sur la suite de la liste en augmentant la position de 1*)
     (** Vérifie chaque élément de la dernière proposition dans le code actuellement en cours de vérification
       * @param élément code de la dernière proposition
       * @param n code actuellement en cours de vérification
       * @param etat liste contenant l'état actuel de la vérification
       * @return true si le code vérifié vérifie les conditions
                 false sinon
       *)
     let rec verifRT element n etat=
          match (element) with
               | [] -> true        (*Si tous les éléments ont vérifié la condition, retourne true*)
               | (mp :: suite) -> (let (valeur,etatp) = verif2RT mp n etat 0 in (*Récupère le couple de vérification*)
                                        if valeur then (*Si la vérification renvoie true*)
                                                  verifRT suite n etatp    (*Relance la vérification sur la suite du code en prenant le nouvel état de vérification*)
                                        else
                                             false);;  (*Sinon, retourne false*)
     (** Vérifie un code par rapport à la dernière proposition
       * @param élément code de la dernière proposition
       * @param n code actuellement en cours de vérification
       * @return true si le code vérifié vérifie les conditions
                 false sinon
       *)
     let verif element n =
          let acc2 = creer_liste_0 0 [] in        (*Crée une liste d'état*)
               verifRT element n acc2;;      (*Lance la vérification*)
     (** Vérifie le nombre de 1 dans la liste d'état de la vérification
       * @param etat la liste d'état de la vérification
       * @param acc le nombre de 1 dans la liste d'état de la vérification
       * @return le nombre de 1 dans la liste d'état de la vérification
       *)
     let rec verif_etat etat acc =
          match (etat) with
               | []           -> acc         (*Une fois toute la liste d'état parcourue, retourne l'accumulateur*)
               | (0 :: suite) -> verif_etat suite acc       (*Si l'élément actuel de la liste d'état est un 0, relance la fonction sur la suite de l'état*)
               | _ -> verif_etat (List.tl etat) (acc+1);;   (*Sinon, relance la fonction sur la suite de la liste d'état en ajoutant 1 à l'accumulateur*)
     (** Vérifie chaque élément de la dernière proposition dans le code actuellement en cours de vérification
       * @param élément code de la dernière proposition
       * @param n code actuellement en cours de vérification
       * @param nc le nombre de pions à vérifier pour répondre à la condition
       * @param etat liste contant l'état actuel de la vérification
       * @return true si le code vérifié vérifie les conditions
                 false sinon
       *)
     let rec verifRT_couleur element n nc etat=
          match (verif_etat etat 0,element) with
               | (nb,_) when (nb=nc) -> true      (*Si le nombre de pions à vérifier est bon, retourne true*)
               | (_,[])              -> false     (*Si on ne trouve pas assez de pions pour valider les conditions, retourne false*)
               | (_,mp :: suite)     -> (
                    let (_,etatp) = verif2RT mp n etat 0 in      (*Vérifie l'élement et modifie la liste d'état en fonction*)
                              verifRT_couleur suite n nc etatp   (*Relance la fonction sur la suite du code en prenant la nouvelle liste d'état*)
                    );;
     (** Vérifie un code par rapport à la dernière proposition
       * @param élément code de la dernière proposition
       * @param n code actuellement en cours de vérification
       * @return true si le code vérifié vérifie les conditions
                 false sinon
       *)
     let verif_couleur element n nc =
          let acc2 = creer_liste_0 0 [] in   (*Crée une liste d'état*)
               verifRT_couleur element n nc acc2;;           (*Lance la vérification*)

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
