
module type Graph =
  sig
    type node
    module NodeSet : Set.S with type elt = node
    type graph
    val empty : graph
    val is_empty : graph -> bool
    val succs : node -> graph -> NodeSet.t
    val add_vertex : node -> graph -> graph
    val add_edge : node -> node -> graph -> graph
    val remove_edge : node -> node -> graph -> graph

end
module MakeGen(N:Set.OrderedType)(NS:Set.S with type elt = N.t)(NM:Map.S with type key = N.t)=
  struct
    type node = N.t
    module NodeSet = NS
    module NodeMap = NM
    type graph = NodeSet.t NodeMap.t
               
    let empty = NodeMap.empty
      
    let is_empty g = NodeMap.is_empty g

    let succs v g = NodeMap.find v g
      
    let add_vertex v g =
      if NodeMap.mem v g
      then g
      else NodeMap.add v NodeSet.empty g
      
    let add_edge v1 v2 g =
      let g' = add_vertex v1 (add_vertex v2 g) in
      let v1_succs = NodeMap.find v1 g' (* Ne peut pas échouer*) in
      let v1_succs' = NodeSet.add v2 v1_succs in
      NodeMap.add v1 v1_succs' g'


    (** g doit posséder v1. **)
    let remove_edge v1 v2 g =
      let v1_succs = succs v1 g (* Ne peut pas échouer*) in
      let v1_succs' = NodeSet.remove v2 v1_succs in
      NodeMap.add v1 v1_succs' g
 
  end
module Make_g(N:Set.OrderedType)=MakeGen(N)(Set.Make(N))(Map.Make(N));;

(* création d'un mosule avec le type String *)
module Flux = Make_g(String);;


(**
@requires une chaine de caractères représentant le nom du fichier (input.txt)
@ensures lire le fichier ligne par ligne 
**)


let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None;;

(**
@requires une chaine de caractères représentant le nom du fichier (input.txt)
@ensures avoir tous les mots d'un fichier
**)

let read_lines ic =
  let rec aux acc =
    match input_line_opt ic with
    | Some line -> aux (List.rev (String.split_on_char ' ' line )@ acc)
    | None -> (List.rev acc)
  in
  aux []


(**
@requires un indice n 
@ensures retourner la liste sans l'élement dans l'indice n,
**)
let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at (n-1) t;;


(**
@requires une chaine de caractères représentant le nom du fichier (input.txt)
@ensures lire les mots du fichier passé en paramètres, enlever les trois première ligne -> retourne la liste des mots qui représentent les noeuds, chaque deux noeuds succéssives représentent un edge
**)

let chemin_elem filename =
  let ic = open_in filename in
  let lines = read_lines ic in
  let nodes_1 = (remove_at 0 lines) in
  let nodes_2 = (remove_at 0 nodes_1) in
  let nodes = (remove_at 0 nodes_2) in
  close_in ic;
  (nodes);;



(**
@requires une liste des arc (donné par la fonction précédante) et un graphe g
@ensures la construction d'un graphe.
**)

let rec build_graph l g = 
  match l with 
  [] -> g
  |t::t1::q ->  let g' = Flux.add_edge t t1 g in build_graph q g'
  |[t] -> let g' = Flux.add_vertex t g in g';;


(**
@requires une chaine de caractères représentant le nom du fichier (input.txt)
@ensures lire extraire les deux premières ligne représentant la source et le puit.
**)

let source_puit filename =
  let ic = open_in filename in
  let lines = read_lines ic in 
  let s,p = (List.nth lines 0, List.nth lines 1) in
  close_in ic;
  (s,p);;


(************************************************************************** PHASE II ******************************************************************************)

(* fonction qui retourne une liste de couples où chaque couple contient les deux noeud formant un edge et la capacité de l'adge entre eux*)

(**
@requires une liste l
@ensures chaque trois éléments succéssive reprsente un chemin élémentaire avec sa capacité maximale
**)

let rec nodes l = match l with 
		 [] -> []
		|t::t1::t2::q -> (t,t1,0,t2) :: nodes q
		|_::[] -> []
		|_::_::[] -> [];;

(**
@requires une liste nodes et un graphe g
@ensures construire le graphe g à partir de la liste nodes
**)

let rec build_graph2 nodes g = 
  match nodes with 
  [] -> g
  |(t,t1,_,_)::q ->  let g' = Flux.add_edge t t1 g in build_graph2 q g';;


(*Pour une liste des plus court chemin entre la source et le puit on cherche à trouver les capacités des chemins élémentaires de chaque chemin entre s et p
  tous les chemins seront initialiser à zero (troisiéme élément du couple *)

(**
@requires une liste des noeuds, les noeud non encores visité,path un plus court chemin entre une source et un puit, un compteur i 
@ensures retourne le chemins sous forme d'une liste de couples, chaque couple est un chemin élémentaire initialisé par 0 (3ème élément du couple) et sa capacité c (4ème élément du couple) 
**)

let rec c_path nodes left_nodes path i= match left_nodes with
			    [] -> []
			   |(s,p,_,c)::q -> if List.length path > (i+1) then
						if  List.nth path (i+1) = p && List.nth path i  = s then  
					        (s,p,0,int_of_string(c)) :: c_path nodes nodes path (i+1)
					        else c_path nodes q path i
					  else [];;

(**
@requires une liste des noeuds, les noeud non encores visité,path un plus court chemin entre une source et un puit, un compteur i 
@ensures une liste de liste où chaque liste représente un chemin sous forme des couple des chemins élémentaires et leurs capacités initiaux et maximaux  
**)

let rec c_paths nodes paths = match paths with
			[] -> []
		       |t::q -> c_path nodes nodes t 0::c_paths nodes q;;



(**
@requires une liste l
@ensures une liste de des capacités courantes  
**) 

let rec l_cap l = match l with 
		 [] -> []
		|(_,_,c_i,c) :: q -> (c-c_i):: l_cap q;;





(**
@requires une liste de chemin élémentaire (un plus court chemin)
@ensures trouver le flux bloquant (la capacité minimal)  
**)

let rec min_cap_ch l min_c =  match l with 
				[] -> failwith "liste vide"
				|(s,p,c_i,c) :: q -> if min_c = c then (s,p,c_i,c) else min_cap_ch q min_c ;; 

(**
@requires une liste d'éléments elm 
@ensures la liste l sans l'élement elm 
**) 

let rec  remove_elm elm l = match l with
	[] -> []
	|t::q -> if t=elm then q else t:: remove_elm elm q ;;


(**
@requires liste de chemins et la longueur du plus court chemin
@ensures le premier plus court chemin 
**)

let rec shortest_path1  paths  min_l =  match paths with 
				        [] -> []
				       |t:: q -> if min_l = List.length t then t
						  else shortest_path1  q min_l;;


(**
@requiresune liste l et une capacité minimal
@ensures mettre à jour les capacités des arcs d'un chemin en ajoutant la capacité du flux bloquant 
**)

let rec update_current_path l  min_c = match l with
		[] -> [] 
		|(s',p',c_i',c') :: q' -> (s',p',(c_i'+min_c),c') :: (update_current_path q' min_c );;

(*mettre à jour le chemin élémentaire dans tous les autre chemins où il existe*)

(**
@requires une un chemin élémentaire ch, la capacité de son flux bloquant , un chemin de s vers p
@ensures mettre à jour la capacité de ch s'il existe dans le chemin path 
**)

let rec update_path ch min path   = match path with
		[] -> []
	       |(s,p,c_i,c)::q -> if (s,p,c_i,c) = ch then (s,p,(c_i+min),c)::q else (s,p,c_i,c)::update_path ch min q;;

(**
@requires une un chemin élémentaire ch, la capacité de son flux bloquant ,une listes des chemins entre s et p
@ensures mettre à jour la capacité de ch s'il existe dans les autres chemins 
**)

let rec update_all_paths ch min paths = match paths with
			[] -> []
			|t::q -> (update_path ch min t )::(update_all_paths ch min q);;











