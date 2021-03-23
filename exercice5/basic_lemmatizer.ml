open Libtrie
open Base
open Stdio

type word_diff = int * char list [@@deriving eq,show] ;;
type word_diff_cat = {diff: word_diff; cat:string} [@@deriving eq,show];;
type t = word_diff_cat Trie.t [@@deriving show] ;;


(* code for diff and word_patch *)
let diff l f = 
  let rec diff_rec mot1 mot2 = match (mot1,mot2) with 
    |([],[]) -> (0,[])
    |([],_) -> ((List.length mot2)-(List.length mot1),[])
    |(_,[]) -> (0,(Trie.string_to_word(String.sub (Trie.word_to_string mot1) ~pos:(List.length mot2) ~len:((List.length mot1)- (List.length mot2)))))
    |(car1::queue1,car2::queue2) -> if phys_equal car1 car2 then diff_rec queue1 queue2 
                                    else ((List.length mot2)-((List.length mot1)-(List.length (car1::queue1))),car1::queue1)
    in diff_rec l f ;;



let word_patch w wd = (Trie.string_to_word (String.sub (Trie.word_to_string w) ~pos:0 ~len:((List.length w)- fst wd)))@(snd wd) ;;


let couper_lemme lemme :(char list) = 
  let rec couper_lemme_rec lem (mot:char list) = match lem with
    |[]-> mot
    |h::queue -> if Char.equal h '_' then (if Char.equal (List.hd_exn queue) '_' then mot else couper_lemme_rec queue (List.rev(h::List.rev(mot)))) 
                 else couper_lemme_rec  queue (List.rev(h::List.rev(mot)))
  in couper_lemme_rec lemme []


let extract_line (s : string)  = 
  String.split s ~on:'\t'
  |> fun l -> (List.nth_exn l 0,List.nth_exn l 2,Trie.word_to_string(couper_lemme(Trie.string_to_word(List.nth_exn l 4)))) ;;


let extract ic  = 
  let rec ext ich =
    In_channel.input_line ich
    |> (fun a -> match a with
      |Some s -> (extract_line s)::ext ich 
      |None -> [])
  in ext ic ;;



let lexicon =
  In_channel.create (Sys.argv).(1)
  |> extract
  |> List.fold
    ~init:(Trie.empty [])
    ~f:(fun acc (f,c,l) ->
        let d = diff (Trie.string_to_word l) (Trie.string_to_word f) in
        let wc = {diff=d;cat=c} in
        if List.mem (Trie.find acc (Trie.string_to_word f)) wc ~equal:equal_word_diff_cat
        then acc else Trie.insert acc (Trie.string_to_word f) wc) 



let () = printf "created trie %d\n%!" (Trie.size lexicon) ;


let oc = Stdlib.open_out_bin "lexicon.bin" in
  Caml.Marshal.to_channel oc lexicon [] 


let make_lemmatize t = fun s -> Trie.find t s |> List.map ~f:(fun {diff;cat} -> (word_patch s diff,cat))
let lemmatizer = make_lemmatize lexicon 


let rec loop () =
  let () = printf "Entrer forme à lemmatiser:\n%!" in
  match Stdio.In_channel.input_line Stdio.stdin with
  | None -> ()
  | Some s ->
    let l = Trie.string_to_word s |> lemmatizer in
    let () = List.iter ~f:(fun (l,c)-> printf "%s ------ %s\n%!" (Trie.word_to_string l) c) l
    in loop () 

  let () = loop()
