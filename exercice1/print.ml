open Base
open Stdio



(* val extract: In_channel -> (string,string,string) list *)
(* lit chaque ligne du fichier (In_channel est équivalent du FILE* du langage C) *)
(* et crée une liste de triplets (forme,catégorie,lemme) *)
(* pour transformer le ic en string, vous utiliserez les fonctions input_line ou fold_lines *)
(* cf. la documentation https://ocaml.janestreet.com/ocaml-core/latest/doc/stdio/Stdio/In_channel/index.html *)
(* vous appellerez ensuite extract_line *)



(* fonction qui transforme un string en char list *)
let string_to_word (s:string) : ( char list ) = 
  let rec string_to_word_rec ch = match ch with
    | "" -> []
    | ch -> (String.get ch 0 ) :: ( string_to_word_rec (String.sub ch ~pos:1 ~len:( (String.length ch)-1) ) )  
  in string_to_word_rec s


(* fonction qui transforme char list en un string *)
let word_to_string (w: char list) : (string) =
  String.concat  (List.map ~f:(String.make 1) w)




let couper_lemme lemme :(char list) =  (* une fonction pour couper le lemme ps: on coupe le lemme dés que on trouve deux tirets de huit de suite *)
  let rec couper_lemme_rec lem (mot:char list) = match lem with
    |[]-> mot
    |h::queue -> if Char.equal h '_' then (if Char.equal (List.hd_exn queue) '_' then mot else couper_lemme_rec queue (List.rev(h::List.rev(mot)))) 
                 else couper_lemme_rec  queue (List.rev(h::List.rev(mot)))
  in couper_lemme_rec lemme []


let extract_line (s : string) : (string * string * string) =
  String.split s ~on:'\t'
  |> fun l -> (List.nth_exn l 0,List.nth_exn l 2,word_to_string(couper_lemme(string_to_word(List.nth_exn l 4)))) 

let extract ic : ((string * string * string) list)  = 
  let rec ext ich =
    In_channel.input_line ich
    |> (fun a -> match a with
      |Some s -> (extract_line s)::ext ich 
      |None -> [])
  in ext ic 



  let () =
  In_channel.create (Sys.argv).(1)
  |> extract
  |> List.iter ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l) 
