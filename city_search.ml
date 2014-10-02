open Parser
open Quadtree

(*This function loads all of the cities from the given file in the string argument, 
stores them in a quadtree, and returns that string quadtree. City coordiantes are given
in terms of longtitude and latitude. Latitude ranges from -90.0 to 90.0 
and longtitude ranges from -180.0 to 180.0.*)
let load_city_data (s:string) : string quadtree = 
  let tree = new_tree ((-90.0,-180.0), (90.0,180.0)) in
  let city_lst = parse s in
  List.fold_left  (fun acc x -> let (lat,long,name) = x in insert acc (lat, long) name) tree city_lst

(*Requires a string quadtree. Returns: all of the cities within the 
given region, which is specified by latitude and longtitude. Cities have a format
of Latitude, Longitude, Name. *)
let city_search (q: string quadtree) (r : region) : string list = 
  fold_region (fun acc x -> (snd x) :: acc) [] q r
