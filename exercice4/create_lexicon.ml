open Libtrie
open Base
open Stdio


let couper_lemme lemme :(char list) = 
  let rec couper_lemme_rec lem (mot:char list) = match lem with
    |[]-> mot
    |h::queue -> if Char.equal h '_' then (if Char.equal (List.hd_exn queue) '_' then mot else couper_lemme_rec queue (List.rev(h::List.rev(mot)))) 
                 else couper_lemme_rec  queue (List.rev(h::List.rev(mot)))
  in couper_lemme_rec lemme []


(* la meme fonction implementée dans l'exercice 1 sauf que la on change le type de la forme flechie de string à word  *)
let extract_line (s : string)  = 
  String.split s ~on:'\t'
  |> fun l -> (Trie.string_to_word (List.nth_exn l 0),List.nth_exn l 2,Trie.word_to_string(couper_lemme(Trie.string_to_word(List.nth_exn l 4))))


let extract ic  =  (* la meme fonction implementée dans l'exercice 1  *)
  let rec ext ich =
    In_channel.input_line ich
    |> (fun a -> match a with
      |Some s -> (extract_line s)::ext ich 
      |None -> [])
  in ext ic


let () =
  let lexicon = In_channel.create (Sys.argv).(1)
                |> extract
                |> List.fold ~init:(Trie.empty []) ~f:(fun acc (f,_,_) -> Trie.insert acc f ()) in
  let lwords = Trie.extract lexicon in
  let () = List.iter lwords ~f:(fun (s,_) -> printf "%s\n" (Trie.word_to_string s) ) in
  let oc = Stdlib.open_out_bin "lexicon.bin" in
  Caml.Marshal.to_channel oc lexicon []
