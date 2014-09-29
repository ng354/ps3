open Parser
open Quadtree

let load_city_data (s:string) : string quadtree = 
  let tree = new_tree ((-90.0,-180.0), (90.0,180.0)) in
  let city_lst = parse s in
  List.fold_left  (fun acc x -> let (lat,long,name) = x in insert acc (lat, long) name) tree city_lst


let city_search (q: string quadtree) (r : region) : string list = 
  fold_region (fun acc x -> (snd x) :: acc) [] q r
