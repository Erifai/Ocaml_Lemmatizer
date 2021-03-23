open Libtrie
open Stdio

let () =
let t1 = Trie.word_to_trie (Trie.string_to_word "bonjour") [()] in
let t2 = Trie.insert t1 (Trie.string_to_word "bon")      () in 
let t3 = Trie.insert t2 (Trie.string_to_word "jour")     () in
let tt = Trie.insert t3 (Trie.string_to_word "bonsoir")     () in
let t4 = Trie.insert tt (Trie.string_to_word "bonhomme") () in


printf "appuyez sur entrée pour afficher le trie \n%!";

let _ = read_line() in 


printf "%s\n%!" (Trie.show
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   t4) ;
   
                   
printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";



printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 

printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";



printf "appuyez sur entrée pour afficher la taille de notre trie \n%!";
let _ = read_line() in 
printf "notre trie contient %d mots\n%!" (Trie.size t4) ;

printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";


printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 

printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";


printf "appuyez sur entrée pour afficher le nombre de caractères de notre trie \n%!";
let _ = read_line() in
printf "notre trie contient %d caractères\n%!" (Trie.arc_size t4) ;

printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";


printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 

printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";


printf "le mot 'bonhomme' appartient-il à notre trie ? appuyez sur entrée pour savoir  \n%!";
let _ = read_line() in
printf " le mot 'bonhomme' appartient à notre trie  ===> %b \n%!" (Trie.mem t4 (Trie.string_to_word "bonhomme")) ;

printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";



printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";



printf "le mot 'homme' appartient-il à notre trie ? appuyez sur entrée pour savoir  \n%!";
let _ = read_line() in
printf " le mot 'homme' appartient à notre trie  ===> %b \n%!" (Trie.mem t4 (Trie.string_to_word "homme")) ;


printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";




printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 

printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";



printf "le mot 'bonj' appartient-il à notre trie ? appuyez sur entrée pour savoir  \n%!";
let _ = read_line() in
printf " le mot 'bonj' appartient à notre trie  ===> %b \n%!" (Trie.mem t4 (Trie.string_to_word "bonj")) ;

printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";


printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";



printf " vous voulez tester ? OK ALLEZ Y ! Tapez un mot de votre choix puis appuyez sur entrée   \n%!"  ;
let mot = read_line () in
printf " le mot '%s' appartient à notre trie  ===> %b \n%!" mot  (Trie.mem t4 (Trie.string_to_word mot)) ;

printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";




printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";




printf "appuyez sur entrée pour afficher les mots contenus dans le trie  \n%!";
let _ = read_line() in
printf "liste des mots : \n%!"  ;
printf " \n%!";
List.iter (fun (li,_) -> printf "%s \n" (Trie.word_to_string li) ) (Trie.extract t4) ;

printf " \n%!";
printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";



printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";




printf "le mot 'ocaml' appartient-il à notre trie ? appuyez sur entrée pour savoir \n%!";
let _ = read_line() in
printf " le mot ocaml appartient à notre trie  ===> %b \n%!" (Trie.mem t4 (Trie.string_to_word "OCAML")) ;


printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";




printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";




printf " Oops ! ne vous inquiétez pas on va l'insérer  \n%!"  ;
printf "Veuillez tapez le mot 'ocaml' s'il vous plaît sinon tapez ce que vous voulez puis appuyez sur entrée \n%!" ; 
let mot = read_line() in 
let t5 = Trie.insert t4 (Trie.string_to_word mot) ()  in
printf "%s\n%!" (Trie.show
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   t5) ;


 printf "*\n%!";
 printf "*\n%!";
 printf "*\n%!";
 printf " \n%!";


printf "Tapez sur entrée pour continuer... \n%!";
let _ = read_line() in 

printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!";


printf " l'affichage est compliqué ? ok appuyez sur entrée pour afficher la liste des mots contenus dans le trie \n%!"  ;
 let _ = read_line() in 
 printf " la liste des mots : \n%!"  ;
List.iter (fun (li,_) -> printf "%s \n" (Trie.word_to_string li) ) (Trie.extract t5) ;

printf " \n%!";
printf "*\n%!";
printf "*\n%!";
printf "*\n%!";
printf " \n%!"; 



printf "Maintenant c'est le temps pour tester les zippers, pensez à ouvrir le fichier Test_zippers.pdf qui est sur le meme dossier pour bien comprendre le scénario\n%!" ;
printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 



let z = Trie.trie_to_zipper t4 in 
printf "%s\n%!" (Trie.show_zipper
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   z) ;



printf "notre zipper est créé et il contient les mots 'bon' 'bonjour' 'bonsoir' 'bonhomme' 'jour' \n%!" ;
printf " Maintenant on va essayer de deplacer le focus appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 



printf " d'abord on va deplacer le focus vers le bas appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 



let z1 = Trie.zip_down_exn z in 
printf "%s\n%!" (Trie.show_zipper
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   z1) ;


printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


printf " encore une fois vers le bas appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


let z2 = Trie.zip_down_exn z1 in 
printf "%s\n%!" (Trie.show_zipper
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   z2) ;



printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


printf " encore une fois vers le bas appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


let z3 = Trie.zip_down_exn z2 in 
printf "%s\n%!" (Trie.show_zipper
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   z3) ;



printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


printf " encore une fois vers le bas appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


let z4 = Trie.zip_down_exn z3 in 
printf "%s\n%!" (Trie.show_zipper
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   z4) ;


printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in



printf " maintenant le focus est sur la lettre j du mot 'bonjour' on va essayer de se deplacer vers la droite... \n%!";
printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


let z5 = Trie.zip_right_exn z4 in 
printf "%s\n%!" (Trie.show_zipper
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   z5) ;


printf " maintenant le focus est sur la lettre s du mot 'bonsoir' on va essayer de se deplacer vers la droite... \n%!";
printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 



let z6 = Trie.zip_right_exn z5 in 
printf "%s\n%!" (Trie.show_zipper
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   z6) ;



printf " maintenant le focus est sur la lettre h du mot 'bonhomme' y a plus de frères  droites on va se deplacer vers la gauche... \n%!";
printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 



let z7 = Trie.zip_left_exn z6 in 
printf "%s\n%!" (Trie.show_zipper
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   z7) ;



printf " maintenant le focus est sur la lettre s du mot 'bonsoir' on va monter à la raçine... \n%!";
printf "appuyez sur entrée pour continuer... \n%!";
let _ = read_line() in 


let z8 = Trie.zip_up_until (fun (Zipper(p,_)) -> match p with |Top -> true |_ -> false) z7 in 
printf "%s\n%!" (Trie.show_zipper
                   (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                   (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                   z8) ;



printf "C'EST LA FIN MERCI !\n%!"
