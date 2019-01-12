(** Module principal du jeu Mastermind *)
#use "IA_local.ml";;

module Mastermindpvp :
     sig
     (** Lance le jeu mastermind
       * @param joueur nom du joueur
       * @param nb_tentative le nombre maximale de tentative par partie
       * @param nb_partie le nombre de parties à jouer
       * @param auto verifie si les reponses sont calculer automatiquement
       * @return le score final
       *)
     val mastermindpvp : string -> string -> int -> int -> unit
     end=
     struct
(** Verifie si le nombre de partie est valide
  * @param nb_partie nombre de partie demandé par le joueur
  * @return nombre de partie valide (strictement paire)
  *)
let verif_nb_partie nb_partie =
     if ((nb_partie mod 2)=0) then
          (nb_partie)
     else
          (nb_partie+1);;
(** Demande à l'utilisateur de rentrer une proposition de code
  * @param nb le numéro du tour
  * @return un code sous forme de liste
  *)
let rec main_prop nb =
     print_endline((string_of_int (nb)) ^ " Tapez une proposition : (ex : Rouge|Vert|Bleu|Jaune)");
     print_endline ((string_of_int (nb)) ^ " Les couleurs disponibles sont : Rouge / Vert / Bleu / Jaune / Violet / Blanc");
     let input_list = read_line () in
          match (Code.code_of_string input_list) with
               | None -> main_prop nb
               | Some(liste) -> liste;;
(** Demande à l'utilisateur de verifier une proposition de code
  * @param code le code proposé par l'ordinateur
  * @param code_secret le code secret choisis par le joueur en début de partie
  * @return un couple designant le nombre de pions bien placés et le nombre de pions mal placés
  *)
let rec prop_res joueur1 code code_secret =
     print_endline ("Le code proposé par " ^ joueur1 ^ " est : " ^ (Code.string_of_code code));
     print_endline ("Le code secret est : " ^ (Code.string_of_code code_secret));
     print_endline ("Merci de rentrer le nombre de pion bien placé puis le nombre de pion mal placé : (ex: 2 -> Entrée -> 2)");
     let input_pbp = int_of_string(read_line()) and input_pmp = int_of_string(read_line()) in
     try (
          if (((input_pbp + input_pmp)>=0) && ((input_pbp + input_pmp)<=Code.nombre_pions)) then
               (input_pbp,input_pmp)
          else
               (print_endline ("Au moins l'une des valeurs rentrée n'est pas correcte");
               prop_res joueur1 code code_secret)
          )
     with
          | _ -> (
               print_endline ("Merci de ne rentrer que des nombres !");
               prop_res joueur1 code code_secret
               )
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
(** Verifie si le nombre de partie est valide
  * @param nb_tentative le nombre maximale de tentative par partie
  * @param code_secret le code secret choisis aléatoirement par l'ordinateur
  * @parem acc un accumulateur contenant le numéro du tour et une liste contenant elle-même chaque proposition déjà effectué ainsi que leur réponse associée.
  * @return le scole finale
  *)
let rec joueurjoueRT joueur1 joueur2 nb_tentative code_secret acc =
     match (acc) with
          | (nb,_) when (nb = nb_tentative) -> (
               Unix.system "clear";
               print_endline (joueur1 ^ ", à vous de jouer !");
               Unix.sleep 1;
               let proposition = main_prop nb in
                    let Some(nbp,nmp) = Code.reponse proposition code_secret in
                         Unix.system "clear";
                         print_endline (joueur2 ^ ", à vous de jouer !");
                         Unix.sleep 3;
                         let (res1,res2) = prop_res joueur1 proposition code_secret in
                              if ((res1 = nbp) && (res2 = nmp)) then
                                   (if (nbp = 4) then
                                        (print_endline (joueur1 ^" a trouvé le code secret, " ^ joueur2 ^ " a perdu.");
                                        let res = 1 in res)
                                   else
                                        (print_endline (joueur1 ^" n'a pas trouvé le code secret, " ^ joueur2 ^ " a gagné.");
                                        let res = 0 in res))
                              else
                                   (print_endline (joueur1 ^" a gagné car " ^ joueur2 ^ " a triché.");
                                   let res = 1 in res)
                    )
          | (nb,liste)                      -> (
               Unix.system "clear";
               print_endline (joueur1 ^ ", à vous de jouer !");
               Unix.sleep 1;
               let proposition = main_prop nb in
                    let Some(nbp,nmp) = Code.reponse proposition code_secret in
                         Unix.system "clear";
                         print_endline (joueur2 ^ ", à vous de jouer !");
                         Unix.sleep 3;
                         let (res1,res2) = prop_res joueur1 proposition code_secret in
                              if (res1 = nbp) && (res2 = nmp) then
                                   (if (nbp = 4) then
                                        (print_endline (joueur1 ^" a trouvé le code secret, " ^ joueur2 ^ " a perdu.");
                                        let res = 1 in res)
                                   else
                                        let liste = liste @ [(proposition,(nbp,nmp))] in
                                             afficher_plateau liste 1;
                                             Unix.sleep 1;
                                             joueurjoueRT joueur1 joueur2 nb_tentative code_secret (nb+1,liste))
                              else
                                   (print_endline (joueur1 ^" a gagné car " ^ joueur2 ^ " a triché.");
                                   let res = 1 in res)
          );;

(** Lance une partie où le joueur doit créer le code
  * @param joueur le nom du joueur
  * @param nb_tentative le nombre maximale de tentative par partie
  * @return le score final
  *)
let joueurjoue joueur1 joueur2 nb_tentative =
     print_endline (joueur2 ^ ", merci de créer le code secret (ex: Rouge|Vert|Jaune|Violet)");
     print_endline (" Les couleurs disponibles sont : Rouge / Vert / Bleu / Jaune / Violet / Blanc");
     let methode = 0 in
          let code_secret = read_line () in
               let Some(code_secret) = Code.code_of_string code_secret in
               Unix.system "clear";
               print_endline (joueur1 ^ ", devinez la combinaison secrète en " ^ (string_of_int nb_tentative) ^ " coups maximum.");
               Unix.sleep 1;
               joueurjoueRT joueur1 joueur2 nb_tentative code_secret (1,[]);;

(** Lance le jeu mastermind
  * @param joueur1 nom du joueur1
  * @param joueur2 nom du joueur2
  * @param nb_tentative le nombre maximale de tentative par partie
  * @param nb_partie le nombre de parties à jouer
  * @param acc_partie le nombre de partie déjà joué
  * @param acc_score le score des différentes parties
  * @return le score final
  *)
let rec mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie acc_partie acc_score=
     let (score_j1,score_j2) = acc_score in
     match (acc_partie) with
          | (nb,_) when (nb=nb_partie+1) -> (
               if (score_j1>score_j2) then
                    (print_endline ("Bravo, " ^ joueur1 ^ ", vous avez gagné plus de partie que " ^ joueur2 ^ " !"))
               else
                    (if (score_j1=score_j2) then
                         (print_endline ("Vous avez gagné autant de partie. Egalité !"))
                    else
                         (print_endline ("Bravo, " ^ joueur2 ^ ", vous avez gagné plus de partie que " ^ joueur1 ^ " !"))
               ))
          | (1,true)                          -> (
               let partie = joueurjoue joueur1 joueur2 nb_tentative in
                    if (partie = 1) then
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (2,false) (score_j1+1,score_j2)
                    else
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (2,false) (score_j1,score_j2+1)
               )
          | (1,false)                         -> (
               let partie = joueurjoue joueur2 joueur1 nb_tentative in
                    if (partie = 0) then
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (2,true) (score_j1+1,score_j2)
                    else
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (2,true) (score_j1,score_j2+1)
               )
          | (nb,true)                         -> (
               let partie = joueurjoue joueur1 joueur2 nb_tentative in
                    if (partie = 1) then
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (nb+1,false) (score_j1+1,score_j2)
                    else
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (nb+1,false) (score_j1,score_j2+1)
               )
          | (nb,false)                        -> (
               let partie = joueurjoue joueur2 joueur1 nb_tentative in
                    if (partie = 0) then
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (nb+1,true) (score_j1+1,score_j2)
                    else
                         mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (nb+1,true) (score_j1,score_j2+1)
               );;

let mastermindpvp joueur1 joueur2 nb_tentative nb_partie =
     let nb_partie = verif_nb_partie nb_partie in
          let debut = Random.bool() in
               mastermindpvpRT joueur1 joueur2 nb_tentative nb_partie (1,debut) (0,0);;

end;;

open Mastermindpvp;;
