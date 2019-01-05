(** Module principal du jeu Mastermind *)
#use "IA.ml";;

module Mastermind :
     sig
     val mastermind : string -> int -> int -> bool -> unit
     end=
     struct
(** Verifie si le nombre de partie est valide
  * @param nb_partie nombre de partie demandé par le joueur
  * @return nombre de partie valide (strictement impaire)
  *)
let verif_nb_partie nb_partie =
     if ((nb_partie mod 2)=0) then
          (nb_partie)
     else
          (nb_partie+1);;

let rec main_prop nb =
     print_endline((string_of_int (nb)) ^ " Tapez une proposition : (ex : Rouge|Vert|Bleu|Jaune)");
     print_endline ((string_of_int (nb)) ^ " Les couleurs disponibles sont : Rouge / Vert / Bleu / Jaune / Violet / Blanc");
     let input_list = read_line () in
          match (Code.code_of_string input_list) with
               | None -> main_prop nb
               | Some(liste) -> liste;;

let rec afficher_plateau liste acc=
     match (liste) with
          | [] ->print_endline("");print_endline("");  ()
          | (liste :: suite) -> (
               let (code,(nbp,nmp)) = liste in
                    print_endline("");
                    print_endline ("Tentative " ^ (string_of_int acc) ^ " : " ^ Code.string_of_code code);
                    print_endline((string_of_int nbp) ^ " pions sont bien placés ; " ^ (string_of_int nmp) ^ " pions sont mal placés");
                    afficher_plateau suite (acc+1)
               )

let rec joueurjoueRT nb_tentative code_secret acc =
     match (acc) with
          | (nb,_) when (nb = nb_tentative) -> (
               let proposition = main_prop nb in
                    let Some(nbp,nmp) = Code.reponse proposition code_secret in
                         if (nbp = 4) then
                              print_endline ("Bravo ! Vous avez trouvé le code secret")
                         else
                              print_endline ("Vous avez dépassé le nombre de tentative, vous pavez perdu ! Le code secret était " ^ (Code.string_of_code code_secret))
               )
          | (nb,liste)                      -> (
               let proposition = main_prop nb in
                    let Some(nbp,nmp) = Code.reponse proposition code_secret in
                         if (nbp = 4) then
                              print_endline ("Bravo ! Vous avez trouvé le code secret")
                         else
                              (print_endline((string_of_int nbp) ^ " pions sont bien placés ; " ^ (string_of_int nmp) ^ " pions sont mal placés");
                              afficher_plateau liste 1;
                              let liste = liste @ [(proposition,(nbp,nmp))] in
                              joueurjoueRT nb_tentative code_secret (nb+1,liste))
               );;

let joueurjoue joueur nb_tentative =
     print_endline (joueur ^ ", devinez la combinaison secrète en " ^ (string_of_int nb_tentative) ^ " coups maximum.");
     let code_secret = List.nth (Code.tous) (Random.int (List.length Code.tous)) in
          joueurjoueRT nb_tentative code_secret (1,[]);;

let ordijoueauto nb_tentative acc = ();;

let ordijoue nb_tentative acc = ();;

let rec mastermindRT joueur nb_tentative nb_partie auto acc_partie =
     match (acc_partie) with
          | (nb,_) when (nb=nb_partie+1) -> ()
          | (1,true)                          -> (
               let partie = joueurjoue joueur nb_tentative in
                    mastermindRT joueur nb_tentative nb_partie auto (2,false)
               )
          | (1,false)                         -> (
               if (auto) then
                    let partie = ordijoueauto nb_tentative (1,[0;0;0;0],Code.tous) in
                         mastermindRT joueur nb_tentative nb_partie auto (2,true)
               else
                    let partie = ordijoue nb_tentative (1,[0;0;0;0],Code.tous,false) in
                         mastermindRT joueur nb_tentative nb_partie auto (2,true)
               )
          | (nb,true)                         -> (
               let partie = joueurjoue joueur nb_tentative in
                    mastermindRT joueur nb_tentative nb_partie auto (nb+1,false)
               )
          | (nb,false)                        -> (
               if (auto) then
                    let partie = ordijoueauto nb_tentative (1,[0;0;0;0],Code.tous) in
                         mastermindRT joueur nb_tentative nb_partie auto (nb+1,true)
               else
                    let partie = ordijoue nb_tentative (1,[0;0;0;0],Code.tous,false) in
                         mastermindRT joueur nb_tentative nb_partie auto (nb+1,true)
               );;

let mastermind joueur nb_tentative nb_partie auto =
     let nb_partie = verif_nb_partie nb_partie in
          let debut = Random.bool() in
               mastermindRT joueur nb_tentative nb_partie auto (1,debut);;

end;;

open Mastermind;;
