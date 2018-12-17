(** Module principal du jeu Mastermind *)

(** Verifie si le nombre de partie est valide
  * @param nb_partie nombre de partie demandé par le joueur
  * @return nombre de partie valide (strictement impaire)
  *)
let verif_nb_partie nb_partie =
     if ((nb_partie mod 2)=0) then
          (nb_partie)
     else
          (nb_partie+1);;

let rec mastermindRT joueur nb_tentative nb_partie auto acc_partie =
     match (acc_partie) with
          | (nb,_) when (nb=nb_partie+1) -> ()
          | (1,true)                          -> (let partie = joueurjoue in mastermindRT joueur nb_tentative nb_partie auto (2,false))
          | (1,false)                         -> (let partie = ordijoue auto in mastermindRT joueur nb_tentative nb_partie auto (2,true))
          | (nb,true)                         -> (let partie = joueurjoue in mastermindRT joueur nb_tentative nb_partie auto (nb+1,false))
          | (nb,false)                        -> (let partie = ordijoue auto in mastermindRT joueur nb_tentative nb_partie auto (nb+1,true));;

let mastermind joueur nb_tentative nb_partie auto =
     let nb_partie = verif_nb_partie nb_partie in
          let debut = Random.bool() in
               mastermindRT joueur nb_tentative nb_partie auto (1,debut);;
