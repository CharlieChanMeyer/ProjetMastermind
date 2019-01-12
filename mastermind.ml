(** Module principal du jeu Mastermind *)
#use "IA_local.ml";;

module Mastermind :
     sig
     (** Lance le jeu mastermind
       * @param joueur nom du joueur
       * @param nb_tentative le nombre maximale de tentative par partie
       * @param nb_partie le nombre de parties à jouer
       * @param auto verifie si les reponses sont calculer automatiquement
       * @return le score final
       *)
     val mastermind : string -> int -> int -> bool -> unit
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
let rec prop_res code code_secret =
     print_endline ("Le code proposé par l'ordi est : " ^ (Code.string_of_code code));
     print_endline ("Le code secret est : " ^ (Code.string_of_code code_secret));
     print_endline ("Merci de rentrer le nombre de pion bien placé puis le nombre de pion mal placé : (ex: 2 -> Entrée -> 2");
     let input_pbp = int_of_string(read_line()) and input_pmp = int_of_string(read_line()) in
     try (
          if (((input_pbp + input_pmp)>=0) && ((input_pbp + input_pmp)<=Code.nombre_pions)) then
               (input_pbp,input_pmp)
          else
               (print_endline ("Au moins l'une des valeurs rentrée n'est pas correcte");
               prop_res code code_secret)
          )
     with
          | _ -> (
               print_endline ("Merci de ne rentrer que des nombres !");
               prop_res code code_secret
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
(** Affiche l'état du plateau de jeu
  * @param liste la liste contenant chaque proposition de l'ordinateur
  * @param acc le numéro de la tentative a afficher
  * @return unit
  *)
let rec afficher_plateau_ordi liste acc=
     match (liste) with
          | [] -> print_endline("");print_endline("");  ()
          | (liste :: suite) -> (
               print_endline("");
               print_endline ("Tentative " ^ (string_of_int acc) ^ " : " ^ Code.string_of_code liste);
               afficher_plateau_ordi suite (acc+1)
               );;
(** Verifie si le nombre de partie est valide
  * @param nb_tentative le nombre maximale de tentative par partie
  * @param code_secret le code secret choisis aléatoirement par l'ordinateur
  * @parem acc un accumulateur contenant le numéro du tour et une liste contenant elle-même chaque proposition déjà effectué ainsi que leur réponse associée.
  * @return le scole finale
  *)
let rec joueurjoueRT nb_tentative code_secret acc =
     match (acc) with
          | (nb,_) when (nb = nb_tentative) -> (
               let proposition = main_prop nb in
                    let Some(nbp,nmp) = Code.reponse proposition code_secret in
                         if (nbp = 4) then
                              (print_endline ("Bravo ! Vous avez trouvé le code secret");
                              let res = 1 in res)
                         else
                              (print_endline ("Vous avez dépassé le nombre de tentative, vous pavez perdu ! Le code secret était " ^ (Code.string_of_code code_secret));
                              let res = 0 in res)
               )
          | (nb,liste)                      -> (
               let proposition = main_prop nb in
                    let Some(nbp,nmp) = Code.reponse proposition code_secret in
                         if (nbp = 4) then
                              (print_endline ("Bravo ! Vous avez trouvé le code secret");
                              let res = 1 in res)
                         else
                              (print_endline((string_of_int nbp) ^ " pions sont bien placés ; " ^ (string_of_int nmp) ^ " pions sont mal placés");
                              afficher_plateau liste 1;
                              let liste = liste @ [(proposition,(nbp,nmp))] in
                              joueurjoueRT nb_tentative code_secret (nb+1,liste))
               );;
(** Lance une partie où le joueur doit créer le code
  * @param joueur le nom du joueur
  * @param nb_tentative le nombre maximale de tentative par partie
  * @return le score final
  *)
let joueurjoue joueur nb_tentative =
     print_endline (joueur ^ ", devinez la combinaison secrète en " ^ (string_of_int nb_tentative) ^ " coups maximum.");
     let code_secret = List.nth (Code.tous) (Random.int (List.length Code.tous)) in
          joueurjoueRT nb_tentative code_secret (1,[]);;
(** Lance une partie où le joueur doit créer le code
  * @param nb_tentative le nombre maximale de tentative par partie
  * @param code_secret le code secret créer par le joueur
  * @param acc un accumulateur contenant le nb de tour, la liste des codes déjà proposé, la liste des codes possibles, un couple reponse contenant la dernière proposition et le resultat de ce code
  * @return le score final
  *)
let rec ordijoueautoRT_naif nb_tentative code_secret acc =
     match (acc) with
          | (nb,liste,courant,reponse) when (nb = nb_tentative) ->(
               let (proposition,courantp) = methode_naif nb courant reponse in
                    let Some(nbp,nmp) = Code.reponse proposition code_secret in
                    if (nbp = 4) then
                         (print_endline ("L'ordinateur a trouvé le code secret, vous avez perdu.");
                         let res = 0 in res)
                    else
                         (print_endline ("L'ordinateur n'a pas trouvé le code secret, vous avez gagné.");
                         let res = 1 in res)
               )
          | (nb,liste,courant,reponse)                          ->(
               let (proposition,courantp) = methode_naif nb courant reponse in
                    let Some(nbp,nmp) = Code.reponse proposition code_secret in
                         if (nbp = 4) then
                              (print_endline ("L'ordinateur a trouvé le code secret en " ^ (string_of_int nb) ^ ", vous avez perdu.");
                              let res = 0 in res)
                         else
                              (let liste = liste @ [proposition] in
                                   afficher_plateau_ordi liste 1;
                                   Unix.sleep 1;
                                   ordijoueautoRT_naif nb_tentative code_secret (nb+1,liste,courantp,(proposition,Some(nbp,nmp))))
               );;
(** Lance une partie où le joueur doit créer le code
  * @param nb_tentative le nombre maximale de tentative par partie
  * @param code_secret le code secret créer par le joueur
  * @param acc un accumulateur contenant le nb de tour, la liste des codes déjà proposé, la liste des codes possibles, un couple reponse contenant la dernière proposition et le resultat de ce code
  * @return le score final
  *)
let rec ordijoueRT_naif nb_tentative code_secret acc =
     match (acc) with
          | (nb,liste,courant,reponse) when (nb = nb_tentative) ->(
               let (proposition,courantp) = methode_naif nb courant reponse in
                    let Some(nbp,nmp) = Code.reponse proposition code_secret in
                         let (res1,res2) = prop_res proposition code_secret in
                              if ((res1 = nbp) && (res2 = nmp)) then
                                   (if (nbp = 4) then
                                        (print_endline ("L'ordinateur a trouvé le code secret, vous avez perdu.");
                                        let res = 0 in res)
                                   else
                                        (print_endline ("L'ordinateur n'a pas trouvé le code secret, vous avez gagné.");
                                        let res = 1 in res))
                              else
                                   (print_endline ("L'ordinateur a gagné car vous avez triché.");
                                   let res = 0 in res)
               )
          | (nb,liste,courant,reponse)                          ->(
               let (proposition,courantp) = methode_naif nb courant reponse in
                    let Some(nbp,nmp) = Code.reponse proposition code_secret in
                         let (res1,res2) = prop_res proposition code_secret in
                              if (res1 = nbp) && (res2 = nmp) then
                                   (if (nbp = 4) then
                                        (print_endline ("L'ordinateur a trouvé le code secret en " ^ (string_of_int nb) ^ ", vous avez perdu.");
                                        let res = 0 in res)
                                   else
                                        (let liste = liste @ [proposition] in
                                             afficher_plateau_ordi liste 1;
                                             Unix.sleep 1;
                                             ordijoueRT_naif nb_tentative code_secret (nb+1,liste,courantp,(proposition,Some(nbp,nmp)))))
                              else
                                   (print_endline ("L'ordinateur a gagné car vous avez triché.");
                                   let res = 0 in res)
               );;

let rec ordijoueautoRT_knuth nb_tentative liste acc = 1;;  (*A FAIRE*)

let rec ordijoueRT_knuth nb_tentative liste acc = 1;; (*A FAIRE*)
(** Lance une partie où le joueur doit créer le code
  * @param joueur nom du joueur
  * @param nb_tentative le nombre maximale de tentative par partie
  * @param auto verifie si les reponses sont calculer automatiquement
  * @return le score final
  *)
let rec ordijoue joueur nb_tentative auto =
     print_endline (joueur ^ ", merci de créer le code secret (ex: Rouge|Vert|Jaune|Violet)");
     print_endline (" Les couleurs disponibles sont : Rouge / Vert / Bleu / Jaune / Violet / Blanc");
     let methode = 0 in
          let code_secret = read_line () in
               match (Code.code_of_string code_secret) with
                    | None -> ordijoue joueur nb_tentative auto
                    | Some(liste) -> (
                         match (methode) with
                              | 0 -> (
                                   if (auto) then
                                        let res = ordijoueautoRT_naif nb_tentative liste (1,[],Code.tous,([],Some(0,0))) in res
                                   else
                                        let res = ordijoueRT_naif nb_tentative liste (1,[],Code.tous,([],Some(0,0))) in res
                                   )
                              | 1 -> (
                                   if (auto) then
                                        let res = ordijoueautoRT_knuth nb_tentative liste (1,[],Code.tous) in res
                                   else
                                        let res = ordijoueRT_knuth nb_tentative liste (1,[],Code.tous,false) in res
                                   )
                         )
                    ;;
(** Lance le jeu mastermind
  * @param joueur nom du joueur
  * @param nb_tentative le nombre maximale de tentative par partie
  * @param nb_partie le nombre de parties à jouer
  * @param auto verifie si les reponses sont calculer automatiquement
  * @param acc_partie le nombre de partie déjà joué
  * @param acc_score le score total
  * @return le score final
  *)
let rec mastermindRT joueur nb_tentative nb_partie auto acc_partie acc_score =
     let (score_j,score_o) = acc_score in
     match (acc_partie) with
          | (nb,_) when (nb=nb_partie+1) -> (
               if (score_j>score_o) then
                    (print_endline ("Bravo, " ^ joueur ^ ", vous avez gagné plus de partie que l'ordinateur !"))
               else
                    (if (score_j=score_o) then
                         print_endline ("Vous avez gagné autant de partie que l'ordinateur. Egalité !")
                    else
                         print_endline ("Dommage, mais vous avez perdu !"))
               )
          | (1,true)                          -> (
               let partie = joueurjoue joueur nb_tentative in
                    if (partie = 1) then
                         mastermindRT joueur nb_tentative nb_partie auto (2,false) (score_j+1,score_o)
                    else
                         mastermindRT joueur nb_tentative nb_partie auto (2,false) (score_j,score_o+1)
               )
          | (1,false)                         -> (
               let partie = ordijoue joueur nb_tentative auto in
                    if (partie = 1) then
                         mastermindRT joueur nb_tentative nb_partie auto (2,true) (score_j+1,score_o)
                    else
                         mastermindRT joueur nb_tentative nb_partie auto (2,true) (score_j,score_o+1)
               )
          | (nb,true)                         -> (
               let partie = joueurjoue joueur nb_tentative in
                    if (partie = 1) then
                         mastermindRT joueur nb_tentative nb_partie auto (nb+1,false) (score_j+1,score_o)
                    else
                         mastermindRT joueur nb_tentative nb_partie auto (nb+1,false) (score_j,score_o+1)
               )
          | (nb,false)                        -> (
               let partie = ordijoue joueur nb_tentative auto in
                    if (partie = 1) then
                         mastermindRT joueur nb_tentative nb_partie auto (nb+1,true) (score_j+1,score_o)
                    else
                         mastermindRT joueur nb_tentative nb_partie auto (nb+1,true) (score_j,score_o+1)
               );;

let mastermind joueur nb_tentative nb_partie auto =
     let nb_partie = verif_nb_partie nb_partie in
          let debut = Random.bool() in
               mastermindRT joueur nb_tentative nb_partie auto (1,debut) (0,0);;

end;;

open Mastermind;;
