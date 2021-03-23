
open Base

(* your code here*)

type 'a t = Node of 'a list * ('a arc list) [@@deriving show] and 'a arc = char * 'a t [@@deriving show]

type word = char list

(* fonction qui transforme un string en char list *)
let string_to_word (s:string) : ( word ) = 
  let rec string_to_word_rec ch = match ch with
    | "" -> []
    | ch -> (String.get ch 0 ) :: ( string_to_word_rec (String.sub ch ~pos:1 ~len:( (String.length ch)-1) ) )  
  in string_to_word_rec s



(* fonction qui transforme char list en un string *)
let word_to_string (w:word) : (string) =
  String.concat  (List.map ~f:(String.make 1) w)



(* fonction qui crée un trie avec la chaîne vide comme préfixe *)
let empty (a:'a list) : ('a t) = Node(a,[])


(* crée un trie qui contient le premier argument avec les infos du 2nd argument *)
let word_to_trie (w : word) (l:'a list) : ('a t) =
  let rec word_to_trie_rec cl li = match cl with
                      | [] -> Node(li,[])
                      |x::q -> Node([],[(x, word_to_trie_rec q li)]) 
  in word_to_trie_rec w l 



(* renvoie le nombre de mots stockés dans le trie *)
let size (a:'a t) : (int) =
    let rec size_rec (Node(infolist, arclist)) = match infolist with
    |[] -> (match arclist with
              |[] -> 0
              |(_,l)::arc -> (size_rec l) + size_rec (Node([],arc))
            )
    |_ -> (match arclist with
            |[] -> 1
            |(_,l)::arc -> 1 + size_rec l + size_rec (Node([],arc)) 
          ) (* à chaque fois ou la liste d'info n'est pas vide on ajoute 1 (1 mot trouvé)  *)
    in size_rec a 


(* renvoie le nombre d'arcs (et donc de caractères) stockés dans le trie *)
let arc_size (a:'a t) : (int) =  
  let rec arc_size_rec (Node(_, arclist)) = match arclist with 
    |[] -> 0
    |(_,tri)::liste -> 1 + arc_size_rec(tri) + arc_size_rec(Node([],liste) )
  in arc_size_rec a (* à chaque fois qu'on trouve un caractère on fait +1 et on appel la fonction recursif sur le trie contenue 
                    dans l'arc + un autre appel sur le reste de la liste des arcs *)




(* renvoie la liste d'informations associée à un mot par le trie *)
(* renvoie la liste vide si le mot n'appartient pas au trie *)
let find  (a:'a t) (w:word): ('a list) =
  let rec find_rec (Node(infolist,arclist)) mot = match (mot,arclist) with
    |(_::_, []) ->  []
    |([],_) -> infolist
    |(car::queue , (c,tri)::arc) -> if Char.equal car c then find_rec tri queue else find_rec (Node(infolist,arc)) mot
    in find_rec a w  (* si c'est une trie vide elle renvoie une liste vide
                      sinon on renvoie la liste d'info du mot qui sera vide si le mot n'est pas dans le trie *)



(* renvoie vrai si le mot est dans le trie, faux sinon *)
let mem (a:'a t) (w:word) : (bool) =
  match find a w with 
  |[] -> false
  |_::_ -> true  


(* renvoie la liste de mots contenus associés à leurs informations *)
let extract (a:'a t)  = 
  (let listmot = [] in
  let rec extract_rec (Node(infolist,arclist)) mot = match infolist with
    |[] -> (match arclist with
            |[] -> []
            |(car,tri)::arc -> List.append (extract_rec tri (car::mot) ) (extract_rec (Node([],arc)) (mot))
           ) (* si la liste d'info est vide on va juste rappeler la fonction recursif sur le trie contenu dans l'arc
              concatené avec l'appel de fonction sur le reste de la liste des arcs   *)
    |_  -> (match arclist with
            |[] -> List.rev (mot)::[]
            |(car,tri)::arc -> List.append (List.rev(mot)::listmot) (List.append  (extract_rec tri (car::mot)) (extract_rec (Node([],arc)) (mot)))
           ) (* si la liste d'info n'est pas vide donc on a un mot alors on ajoute le mot à la liste des mots
                concatené avec les deux appels de la fonction comme le premier cas  *)
  in extract_rec a [])
  |> fun liste -> (List.map liste ~f:(fun li -> (li,(find a li))) ) 
  (* aprés avoir récupérer la liste des mots on utilise la fonction map pour associer 
      à chaque mot sa liste d'info (qui est surement pas vide)  *)


    
    

  
  type 'a path = Top | Noeud of 'a arc list * 'a path * char * 'a list *  'a arc list [@@deriving show]
                     (*     'a arc list   contexte gauche du focus 
                            'a path       chemin vers la raçine
                            char          caractère lu pour acceder au focus
                            'a list       la liste d'info du père du focus
                            'a arc liste  contexte droite du focus
                      *)
  type 'a zipper = Zipper of 'a path * 'a t [@@deriving show]
    
    exception Up;;
    exception Down;;
    exception Left;;
    exception Right;;

  
  
  
  let trie_to_zipper (a :'a t) : ('a zipper) =
    Zipper(Top,a) ;;



  let zip_up_exn (z:'a zipper) : ('a zipper) = 
    match z with 
    |Zipper(Top,_) -> raise Up 
    |Zipper(Noeud(glist,apath,car,infolist,dlist),a) -> Zipper(apath,Node(infolist,glist@((car,a)::dlist)));;




  let zip_down_exn (z:'a zipper) : ('a zipper) =
    let Zipper(apath,tr) = z in 
      let Node(infolist,arclist) = tr in 
        match arclist with 
        |[] -> raise Down
        |(car,t)::queue -> Zipper(Noeud([],apath,car,infolist,queue),t);; (* le premier element de la liste des arc 
                                                      devient focus si elle est pas vide sinon l'exception Down est levée *)




  let zip_left_exn (z:'a zipper) : ('a zipper) =
    let Zipper(apath,tr) = z in 
      match apath with 
      |Top -> raise Up 
      |Noeud(glist,p,c,infolist,dlist) -> let glist_inv = List.rev glist in 
                                          match glist_inv with 
                                          |[] -> raise Left
                                          |(car,t)::[] -> Zipper(Noeud([],p,car,infolist,(c,tr)::dlist),t) 
                                          |(car,t)::queue ->Zipper(Noeud(List.rev(queue),p,car,infolist,(c,tr)::dlist),t);;
                  (* si la liste des frères gauches contient un seul element il devient focus
                        et son contexte gauche est vide
                    sinon
                        on utilise List.rev pour récupèrer le dernier élement de la liste qui est le frère le plus proche
                        du focus  
                    *)





  let zip_right_exn (z:'a zipper) : ('a zipper) = 
    let Zipper(apath,tr) = z in 
      match apath with 
      |Top -> raise Up 
      |Noeud(_,_,_,_,[]) -> raise Right 
      |Noeud(glist,p,c,infolist,(car,t)::queue) -> Zipper(Noeud(List.rev((c,tr)::List.rev(glist)),p,car,infolist,queue),t);;
                    (* on utilise List.rev pour insèrer l'ancien focus à la fin du contexte gauche du nouveau focus *)







  let zip_up_until (f:('a zipper -> bool)) (z:'a zipper) : ('a zipper) =
    let rec zip_up_until_rec predicat zipp = if predicat zipp then zipp else zip_up_until_rec predicat (zip_up_exn zipp)
    in zip_up_until_rec f z  ;;




  let zip_down_until (f:('a zipper -> bool)) (z:'a zipper) : ('a zipper) =
    let rec zip_down_until_rec predicat zipp = if predicat zipp then zipp else zip_down_until_rec predicat (zip_down_exn zipp)
    in zip_down_until_rec f z ;;
    
    


  let zip_left_until (f:('a zipper -> bool)) (z:'a zipper) : ('a zipper) =
    let rec zip_left_until_rec predicat zipp = if predicat zipp then zipp else zip_left_until_rec predicat (zip_left_exn zipp)
    in zip_left_until_rec f z ;;
    
    


  let zip_right_until (f:('a zipper -> bool)) (z:'a zipper) : ('a zipper) =
    let rec zip_right_until_rec predicat zipp = if predicat zipp then zipp else zip_right_until_rec predicat (zip_right_exn zipp)
    in zip_right_until_rec f z ;;
    
    

  let zipper_to_trie (z:'a zipper) : ('a t) = 
    let rec zipper_to_trie_rec (Zipper(apath,trie)) = match apath with 
      |Top -> trie
      |Noeud(_,_,_,_,_) ->  zipper_to_trie_rec ( zip_up_until  
      (fun (Zipper(p,_)) -> match p with |Top -> true |_ -> false)
      z
      )
    in zipper_to_trie_rec z ;; (* on utlise la fonction zip_up_until pour monter dans à  la raçine et on renvoie 
                                  le trie complet   *)






  let zip_insert_right (z:'a zipper) (c:char) (t:'a t) : ('a zipper) =
    let (Zipper(apath,trie)) = z in 
      match apath with 
      |Top -> raise Up
      |Noeud(glist,path,car,infolist,dlist) -> Zipper(Noeud(glist,path,car,infolist,(c,t)::dlist),trie) ;;
              (* on insère le nouveau élement au debut de la liste du contexte droite *)



  

  let zip_insert_left (z:'a zipper) (c:char) (t:'a t) : ('a zipper) = 
    let (Zipper(apath,trie)) = z in 
      match apath with
      |Top -> raise Up
      |Noeud(glist,path,car,infolist,dlist) -> Zipper(Noeud(List.rev((c,t)::List.rev(glist)),path,car,infolist,dlist),trie) ;;
             (* on insère le nouveau élement à la de la liste du contexte gauche on utilisant la fonction List.rev *)


let car_trouve (Zipper(path,Node(_,_))) lettre =  match path with 
                                          |Top -> raise Up
                                          |Noeud(_,_,car,_,_) -> Char.equal lettre car 

let  is_limite z =  match z with 
                    |Zipper(Noeud(_,_,_,_,dlist),Node(_,_)) -> (match dlist with 
                                                                                  |[]-> true
                                                                                  |_ -> false )
                    |_ -> raise Up
                                
let predicat lettre z = car_trouve z lettre || is_limite z 




let insert (t:'a t) (w:word) (a:'a) : 'a t = let zip = trie_to_zipper t in 
    let rec  insert_rec zp mot = match mot with 
                                  | [] -> (match zp with  (* si le mot est vide donc on met à jour la liste d'info avec l'argument a *)
                                          |Zipper(p,Node(_,arclist)) -> Zipper(p,Node([a],arclist)))
                                  |lettre::queue -> (let nzp = zip_right_until (predicat lettre) (zip_down_exn zp)  in (* recherche *)
                                                    if car_trouve nzp lettre then let Zipper(path,Node(_,arclist)) = nzp in (* si caractère trouvé *)
                                                                            match path with 
                                                                            |Top -> failwith"cas impossible" (* cas impossible (raçine) *)
                                                                            |Noeud(_,_,_,_,_) -> (* insertion *)
                                                                                     match arclist with
                                                                                    |[] -> (match queue with (*si la liste d'arc est vide *)
                                                                                             |[] -> nzp (* mot vide donc on insère rien *)
                                                                                             |_::_ -> (let Node(_,arcl) = (word_to_trie queue [a]) in (* on cree un nouveau trie à partir de notre mot et on insère son arclist dans notre focus qui a déa une arclist vide *) 
                                                                                                     match nzp with 
                                                                                                      |Zipper(path,Node(inf,_)) -> Zipper(path,Node(inf,arcl))                                                                    
                                                                                                     )
                                                                                            )
                                                                                    |_ -> insert_rec nzp queue (* si la liste d'arcs n'est pas vide on va descendre *)
                                                    else zip_insert_right nzp lettre (word_to_trie queue [a]) ) (* si caractère non trouvé donc on va insère de le mot à droite *)
    in if size t = 0 then word_to_trie w [a]    (* le cas ou notre trie est vide on retourne un nouveau trie cree à partir de notre mot  *) 
      else  let Zipper(_,t) = zip_up_until (fun (Zipper(p,_)) -> match p with |Top -> true |_ -> false) (insert_rec zip w) in t 
      (* sinon on insère apres on monte jusqu'au Top puis on retourne le trie mis à jour *)
                                
