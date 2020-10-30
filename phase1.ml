
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

(* création d'un graphe avec le type String *)
module Flux = Make_g(String);;

(************************************************************************** PHASE I ******************************************************************************)


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
@requires une liste des arcs (donné par la fonction précédante) et un graphe g
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




(** LE PLUS COURT CHEMIN **)



(**
@requires une liste des couples (k : la clé dans map = un noeud ; b = bindigns) 
@ensures une liste des noeuds du graphe (sans redondance)
**)

let rec vertex_list l = match l with 
			   [] -> []
			   |(k,b)::q -> k:: vertex_list q;;


(**
@requires un graphe g, un noeud a, la liste des noeuds eet une condition  
@ensures trouver les prédécesseurs d'un noeud a
**)

let rec neighbors g a nodes cond  = 
		    match nodes with
		    [] -> []
		   |t::q -> let succ_t = Flux.NodeSet.elements (Flux.succs t g) in
			    if List.mem a succ_t && cond t then t:: neighbors g a q cond   else neighbors g a q cond ;;




(**
@requires un graphe g, un noeud a et une liste [to_b] qui contient un path vers b
@ensures tous les chemins possible entre a et b (sans prendre en compte les boucles)
@raises erreur si [to_be] est vide: contient au moins b (le puit)
**)

let rec list_path g a to_b = match to_b with
    | [] -> assert false (* [to_b] contains the path to [b]. *)
    | a' :: _ ->
       if a' = a then [to_b]
       else
         let n = neighbors g a' (vertex_list (Flux.NodeMap.bindings g)) (fun c ->not(List.mem c to_b) )in
         List.concat(List.map (fun c -> list_path g a (c :: to_b)) n);;			            


(**
@requires une liste de listes
@ensures une liste contenant la longeurs de chaque liste des listes
**)

let rec lenght_paths l = match l with
	 [] -> []
	|t::q -> (List.length t) :: lenght_paths q;;


(**
@requires une liste d'entiers (les longueurs) 
@ensures la longeur minimal
@raises l'exception "liste vide" si la liste est vide
**)

let rec l_min l = match l with  
			|[x] -> x 
			|t::q -> min t (l_min q) 
			|[] -> failwith "Liste vide!";;



(**
@requires une liste de liste paths (tous les chemins possibles), min_l un entier qui représente la longueur de la plus pelite liste (le chemin le plus court)
@ensures une liste des chemins les plus courts (ils ont la même longueur)
**)

let rec shortest_path  paths  min_l =  match paths with 
				        [] -> []
				       |t:: q -> if min_l = List.length t then t::shortest_path  q min_l
						  else shortest_path  q min_l;;


(**MAIN**)


(**
@requires () pas de paramétres 
@ensures vérifier le nombre de paramétres passer à l'exécutable.
@raises l'exception "liste vide" si la liste est vide
**)

let verify_args  = 
  if Array.length Sys.argv != 2  then false else true;;


(**
@requires une liste de chaine de caractères
@ensures transformer une liste de chaine de caractères en une seul chaine de caractères
**)

let rec list_to_string l = match l with 
	[] -> ""
	|t::q -> t ^ " " ^ list_to_string q;;

(**
@requires une liste de listes de chaine de caractères
@ensures transformer une liste de listes de chaines de caractères en un ensembre de ligne (chaque liste représente une chaine)
**)

let rec l_list_to_string l = match l with
	[] -> ""
	|t::q -> list_to_string t ^" \n" ^l_list_to_string q ;; 



(**
@requires oc : chaine de caractère représentant le nom du fichier
@ensures écritures des plus courts chemins sur le fichier oc
**)

let rec output_lines oc l =
  let oc' = open_out oc in
  Printf.fprintf oc' "%s\n" (l_list_to_string l); close_out oc';;

(**
@requires un graphe g
@ensures rassemble tous les fonctions précédentes pour écrire dans le fichier output.txt les plus courts chemins correspendant au fichier passé en paramétres
@raises l'exception "no arguments" si on a pas passé un nom de fichier
**)

let main g = if verify_args then 	
		let chemin_elmen= chemin_elem (Sys.argv.(1)) in 
		let graphe =build_graph chemin_elmen g in 
                let s,p = source_puit (Sys.argv.(1)) in 
		let paths = list_path graphe s [p] in
		  if List.length paths !=0 then
			let minl_paths = l_min (lenght_paths paths) in
		        let short_paths = shortest_path  paths  minl_paths in 
			output_lines "output.txt" short_paths
		  else let message = [[" Aucun chemin entre la source et la puit"]] in
		     output_lines "output.txt" message
                else failwith "no arguments";;

(** création d'un graphe g vide **)

let g = Flux.empty;;

(**exécution du programme **)

let () = main g;;











