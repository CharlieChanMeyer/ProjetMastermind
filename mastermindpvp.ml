(** Module principal du jeu Mastermind *)
#use "code.ml";;

module Mastermindpvp :
     sig
     (** Lance le jeu mastermind
       * @param joueur nom du joueur
       * @param nb_tentative le nombre maximal de tentatives par partie
       * @param nb_partie le nombre de parties à jouer
       * @param auto verifie si les reponses sont calculées automatiquement
       * @return le score final
       *)
     val mastermindpvp : string -> string -> int -> int -> unit
     end=
     struct
(** Nettoie le terminal
  *)
     let clear () = ignore (Unix.system "clear") ;;
(** Verifie si le nombre de parties est valide
  * @param nb_partie nombre de parties demandé par le joueur
  * @return nombre de parties valides (strictement paire)
  *)
let verif_nb_partie nb_partie =
     if ((nb_partie mod 2)=0) then
          (nb_partie)
     else
          (nb_partie+1);;
(** Verifie si l'entrée est un nombre
  * @param ligne l'entrée utilisateur
  * @return Some(nombre) si l'entrée est un nombre
            None si l'entrée utilisateur n'est pas valide
  *)
let int_of_string_option ligne =
     try (Some(int_of_string (ligne))) with |_ -> None;;
(** Demande à l'utilisateur de rentrer une proposition de code
  * @param nb le numéro du tour
  * @return un code sous forme de liste
  *)
let rec main_prop nb =
     print_endline((string_of_int (nb)) ^ " Tapez une proposition : (ex : " ^ (Code.string_of_code (List.nth (Code.tous) (Random.int (List.length (Code.tous))))) ^ ")"); (*Affiche les consignes*)
     print_endline ((string_of_int (nb)) ^ " Les couleurs disponibles sont : " ^ (Code.string_of_code (Code.couleurs_possibles)));
     let input_list = read_line () in (*Demande à l'utilisateur de rentrer un code*)
          match (Code.code_of_string input_list) with (*Vérifie le code rentré par l'utilisateur*)
               | None -> (print_endline("");print_endline("Le code proposé n'est pas valide");print_endline("");main_prop nb) (*L'entrée utilisateur n'étant pas correcte, on lui demande d'en rerentrer une valide*)
               | Some(liste) -> liste;; (*Retourne le code valide proposé par l'utilisateur*)
(** Demande à l'utilisateur de verifier une proposition de code
  * @param code le code proposé par l'ordinateur
  * @param code_secret le code secret choisi par le joueur en début de partie
  * @return un couple designant le nombre de pions bien placés et le nombre de pions mal placés
  *)
let rec prop_res joueur1 code code_secret =
     print_endline ("Le code proposé par " ^ joueur1 ^ " est : " ^ (Code.string_of_code code));         (*Affiche les consignes*)
     print_endline ("Le code secret est : " ^ (Code.string_of_code code_secret));
     print_endline ("Merci de rentrer le nombre de pions bien placés puis le nombre de pions mal placés : (ex: 2 -> Entrée -> 2)");
     let input_pbp = read_line() and input_pmp = read_line() in           (*Récupère les valeurs entrées par l'utilisateur*)
      match (int_of_string_option input_pbp,int_of_string_option input_pmp) with (*Vérifie les valeurs entrées par l'utilisateur*)
          | (Some(pbp),Some(pmp)) -> (
               if (((pbp + pmp)>=0) && ((pbp + pmp)<=Code.nombre_pions)) then   (*Si les valeurs sont correctes,retourne le couple de valeur*)
                    (pbp,pmp)
               else                                                             (*Sinon, redemande à l'utilisateur de rentrer des valeurs valides*)
                    (print_endline ("Au moins l'une des valeurs rentrée n'est pas correcte");
                    prop_res joueur1 code code_secret)
               )
          | (_,_)                 -> (print_endline ("Merci de ne rentrer que des nombres !");print_endline("");prop_res joueur1 code code_secret)
    ;;
(** Affiche l'état du plateau de jeu
  * @param liste la liste contenant chaque proposition du joueur et son résultat correspondant
  * @param acc le numéro de la tentative a afficher
  * @return unit
  *)
let rec afficher_plateau liste acc=
     match (liste) with
          | [] -> print_endline("");print_endline("");  ()
          | (liste :: suite) -> (
               let (code,(nbp,nmp)) = liste in
                    print_endline("");
                    print_endline ("Tentative " ^ (string_of_int acc) ^ " : " ^ Code.string_of_code code);
                    print_endline((string_of_int nbp) ^ " pions sont bien placés ; " ^ (string_of_int nmp) ^ " pions sont mal placés");
                    afficher_plateau suite (acc+1)
               );;
(** Verifie si le nombre de parties est valide
  * @param nb_tentative le nombre maximal de tentatives par partie
  * @param code_secret le code secret choisi aléatoirement par l'ordinateur
  * @parem acc un accumulateur contenant le numéro du tour et une liste contenant elle-même chaque proposition déjà effectuée ainsi que leur réponse associée.
  * @return le scole final
  *)
let rec joueurjoueRT joueur1 joueur2 nb_tentative code_secret acc =
     match (acc) with                                                   (*Vérifie le numéro du tour de la partie*)
          | (nb,liste) when (nb = nb_tentative) -> (                        (*Si c'est le dernier tour de la partie*)
               clear ();
               print_endline (joueur1 ^ ", à vous de jouer !");
               Unix.sleep 1;
               afficher_plateau liste 1;                                (*Affiche le plateau de jeu*)
               let proposition = main_prop nb in                        (*Demande au joueur1 de rentrer une proposition*)
                    match (Code.reponse proposition code_secret) with
                         | Some(nbp,nmp) -> (
                         clear ();
                         print_endline (joueur2 ^ ", à vous de jouer !");
                         Unix.sleep 3;
                         let (res1,res2) = prop_res joueur1 proposition code_secret in      (*Récupère la réponse du joueur2*)
                              if ((res1 = nbp) && (res2 = nmp)) then      (*Si le joueur2 n'a pas triché*)
                                   (if (nbp = 4) then                   (*Si le code est bon, dit au joueur qu'il a gagné, et retourne le score final de la partie*)
                                        (print_endline (joueur1 ^" a trouvé le code secret, " ^ joueur2 ^ " a perdu.");
                                        let res = 1 in res)
                                   else                                  (*Sinon, le code est faux, et le joueur perd*)
                                        (print_endline (joueur1 ^" n'a pas trouvé le code secret, " ^ joueur2 ^ " a gagné.");
                                        let res = 0 in res))
                              else                                  (*Si le joueur2 a triché, il pert et retourne le score final de la partie*)
                                   (print_endline (joueur1 ^" a gagné car " ^ joueur2 ^ " a triché.");
                                   let res = 1 in res))
                         | _ -> joueurjoueRT joueur1 joueur2 nb_tentative code_secret acc
                    )
          | (nb,liste)                      -> (
               clear ();
               print_endline (joueur1 ^ ", à vous de jouer !");
               Unix.sleep 1;
               afficher_plateau liste 1;                (*Affiche le plateau de jeu*)

               let proposition = main_prop nb in            (*Demande au joueur1 de rentrer une proposition*)
               match (Code.reponse proposition code_secret) with
                    | Some(nbp,nmp) -> (
                         clear ();
                         print_endline (joueur2 ^ ", à vous de jouer !");
                         Unix.sleep 3;
                         let (res1,res2) = prop_res joueur1 proposition code_secret in      (*Récupère la réponse du joueur2*)
                              if (res1 = nbp) && (res2 = nmp) then               (*Si le joueur2 n'a pas triché*)
                                   (if (nbp = 4) then                           (*Si le code est bon, dit au joueur qu'il a gagné, et retourne le score final de la partie*)
                                        (print_endline (joueur1 ^" a trouvé le code secret, " ^ joueur2 ^ " a perdu.");
                                        let res = 1 in res)
                                   else
                                        let liste = liste @ [(proposition,(nbp,nmp))] in            (*Ajoute à la variable liste le tour qui vient de passer*)
                                             joueurjoueRT joueur1 joueur2 nb_tentative code_secret (nb+1,liste))        (*Lance le tour suivant*)
                              else                  (*Si le joueur2 a triché, il perd et retourne le score final de la partie*)
                                   (print_endline (joueur1 ^" a gagné car " ^ joueur2 ^ " a triché.");
                                   let res = 1 in res))
                    | _ -> joueurjoueRT joueur1 joueur2 nb_tentative code_secret acc
          );;

(** Lance une partie où le joueur doit créer le code
  * @param joueur le nom du joueur
  * @param nb_tentative le nombre maximal de tentatives par partie
  * @return le score final
  *)
let rec joueurjoue joueur1 joueur2 nb_tentative =
     print_endline (joueur2 ^ ", merci de créer le code secret (ex: "^(Code.string_of_code (List.nth (Code.tous) (Random.int (List.length (Code.tous)))))^")");
     print_endline (" Les couleurs disponibles sont : " ^ (Code.string_of_code (Code.couleurs_possibles)));
          let code_secret = read_line () in                 (*Récupère le code proposé par le joueur2*)
               match (Code.code_of_string code_secret) with
                    | Some(code_secret) -> (            (*Si le code secret est valide, lance la partie*)
               clear ();
               print_endline (joueur1 ^ ", devinez la combinaison secrète en " ^ (string_of_int nb_tentative) ^ " coups maximum.");
               Unix.sleep 1;
               joueurjoueRT joueur1 joueur2 nb_tentative code_secret (1,[]))
                    | _ -> joueurjoue joueur1 joueur2 nb_tentative;;        (*Sinon redemande un code secret valide au joueur*)

(** Lance le jeu mastermind
  * @param joueur1 nom du joueur1
  * @param joueur2 nom du joueur2
  * @param nb_tentative le nombre maximal de tentatives par partie
  * @param nb_partie le nombre de parties à jouer
  * @param acc_partie le nombre de parties déjà jouées
  * @param acc_score le score des différentes parties
  * @return le score final
  *)
let rec mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie acc_partie acc_score=
     let (score_j1,score_j2) = acc_score in
     match (acc_partie) with
          | (nb,_) when (nb=nb_partie+1) -> (                       (*Lorsque toutes les parties ont été jouées,...*)
               if (score_j1>score_j2) then                          (*Si le joueur1 a gagné, lui indique et termine le programme*)
                    (print_endline ("Bravo, " ^ joueur1 ^ ", vous avez gagné plus de partie que " ^ joueur2 ^ " !"))
               else
                    (if (score_j1=score_j2) then                    (*S'il y a égalité, l'ordinateur l'indique et termine le programme*)
                         (print_endline ("Vous avez gagné autant de partie. Egalité !"))
                    else                                            (*Si le joueur2 a gagné, lui indique et termine le programme*)
                         (print_endline ("Bravo, " ^ joueur2 ^ ", vous avez gagné plus de partie que " ^ joueur1 ^ " !"))
               ))
          | (nb,true)                         -> (
               let partie = joueurjoue joueur1 joueur2 nb_tentative in      (*Lance une partie et récupère son score*)
                    if (partie = 1) then            (*Si le joueur1 a gagné la partie, augmente son score de 1 et lance la partie suivante*)
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (nb+1,false) (score_j1+1,score_j2)
                    else                            (*Sinon, augmente le score du joueur2 de 1 et lance la partie suivante*)
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (nb+1,false) (score_j1,score_j2+1)
               )
          | (nb,false)                        -> (
               let partie = joueurjoue joueur2 joueur1 nb_tentative in
                    if (partie = 0) then        (*Si le joueur1 a gagné la partie, augmente son score de 1 et lance la partie suivante*)
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (nb+1,true) (score_j1+1,score_j2)
                    else                        (*Sinon, augmente le score du joueur2 de 1 et lance la partie suivante*)
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (nb+1,true) (score_j1,score_j2+1)
               );;

let mastermindpvp joueur1 joueur2 nb_tentative nb_partie =
     let nb_partie = verif_nb_partie nb_partie in       (*Vérifie le nombre de partie*)
          let debut = Random.bool() in                  (*Définit de manière aléatoire qui commencera*)
               mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (1,debut) (0,0);;         (*Lance la première partie du jeu*)

end;;

open Mastermindpvp;;
