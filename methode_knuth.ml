(** Algorithmes de la methode knuth *)
#use "code.ml"

module Knuth (*:
     sig
     end*)=
     struct

     let rec verif test minimum =
          match (test,minimum) with
               | ([],[])                                         -> true
               | (t :: tsuite,m :: msuite) when (t > m)          -> false
               | (t :: tsuite,m :: msuite) when (m > t)          -> true
               | (_,_) when ((List.hd test) = (List.hd minimum)) -> verif (List.tl test) (List.tl minimum)

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

end;;
