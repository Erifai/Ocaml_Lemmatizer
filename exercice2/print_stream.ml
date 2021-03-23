open Base
open Stdio

type 'a stream = Nil | Cons of 'a * 'a stream thunk and 'a thunk = unit -> 'a

let myhead l = match l with
| Nil -> failwith "liste vide"
| Cons (t,_) -> t;;
let mytail l = match l with
| Nil -> failwith "liste vide"
| Cons (_,q) -> q ();;


let string_to_word (s:string) : ( char list ) = 
  let rec string_to_word_rec ch = match ch with
    | "" -> []
    | ch -> (String.get ch 0 ) :: ( string_to_word_rec (String.sub ch ~pos:1 ~len:( (String.length ch)-1) ) )  
  in string_to_word_rec s


  let word_to_string (w: char list) : (string) =
  String.concat  (List.map ~f:(String.make 1) w)



let couper_lemme lemme :(char list) =  (* la meme fonction utilisé dans l'exercice 1 *)
  let rec couper_lemme_rec lem (mot:char list) = match lem with
    |[]-> mot
    |h::queue -> if Char.equal h '_' then (if Char.equal (List.hd_exn queue) '_' then mot else couper_lemme_rec queue (List.rev(h::List.rev(mot)))) 
                 else couper_lemme_rec  queue (List.rev(h::List.rev(mot)))
  in couper_lemme_rec lemme []

let extract_line (s : string) : (string * string * string) =
  String.split s ~on:'\t'
  |> fun l -> (List.nth_exn l 0,List.nth_exn l 2,word_to_string(couper_lemme(string_to_word(List.nth_exn l 4))))

(* val extract : In_channel -> (string*string*string) stream *)
let rec extract ic = 
  match (In_channel.input_line ic)  with
                                          |None -> Nil
                                          |Some s -> Cons(extract_line s,fun () -> extract ic )

(* val iter_stream : 'a stream -> ('a -> unit) -> unit *)
let rec iter_stream st ~f = 
  match st with
  |Nil -> ()
  |_ -> f(myhead st) ;  iter_stream (mytail st) ~f



let () =
  In_channel.create (Sys.argv).(1)
  |> extract
  |> iter_stream ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l)
