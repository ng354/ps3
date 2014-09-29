type coord = float * float
type region = coord * coord
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds

let new_tree (r:region) : 'a quadtree = 
  Leaf (r,[])



module Region = struct
  let x1 r = fst (fst r)
  let y1 r = snd (fst r)
  let x2 r = fst (snd r)
  let y2 r = snd (snd r)
  let midpoint_x r = ((x1 r) +. (x2 r))/.2.0
  let midpoint_y r = ((y1 r) +. (y2 r))/.2.0
  let min_diagonal_check r = sqrt((x2 r  -. x1 r)**2.0 +. (y2 r  -. y1 r)**2.0) >= min_diagonal
  let contains r c = fst c >= x1 r && fst c <= x2 r && snd c >= y1 r && snd c <= y2 r
end

module NE = struct 
  let region r =  ( (Region.midpoint_x r, Region.midpoint_y r), snd r) 
  let contains1 r c = fst c >= Region.x1 (region r) && fst c <= Region.x2 (region r) && snd c > Region.y1 (region r) && snd c <= Region.y2 (region r) (*if r is not NE quadrant *) 
  let contains2 r c = fst c >= Region.x1 r && fst c <= Region.x2 r && snd c > Region.y1 r && snd c <= Region.y2 r (*if r is NE quadrant*)
  let leaf_insert r c h s insert = match (contains1 r c , contains1 r (fst h) ) with
                                             | (true,true) -> insert (Leaf (region r,[h]) ) c s
                                             | (true,false) -> insert (new_tree (region r)) c s
                                             | (false, true) -> Leaf (region r,[h])
                                             | (false, false) -> new_tree (region r)  
end

module NW = struct
  let region r = ( ( Region.x1 r , Region.midpoint_y r ) , ( Region.midpoint_x r, Region.y2 r ) )
  let contains1 r c = fst c >= Region.x1 (region r) && fst c < Region.x2 (region r) && snd c >= Region.y1 (region r) && snd c <= Region.y2 (region r)
  let contains2 r c = fst c >= Region.x1 r && fst c < Region.x2 r && snd c >= Region.y1 r && snd c <= Region.y2 r 
  let leaf_insert r c h s insert = match (contains1 r c , contains1 r (fst h) ) with
                                             | (true,true) -> insert (Leaf (region r,[h]) ) c s
                                             | (true,false) -> insert (new_tree (region r)) c s
                                             | (false, true) -> Leaf (region r,[h])
                                             | (false, false) -> new_tree (region r)  
end

module SW = struct
  let region r = ( fst r, ( Region.midpoint_x r, Region.midpoint_y r ) )
  let contains1 r c = fst c >= Region.x1 (region r) && fst c <= Region.x2 (region r) && snd c >= Region.y1 (region r) && (snd c < Region.y2 (region r) || c = snd (region r) ) (*check for if c is in the center of the region*)
  let contains2 r c = fst c >= Region.x1 r && fst c <= Region.x2 r && snd c >= Region.y1 r && (snd c < Region.y2 r || c = snd r)
  let leaf_insert r c h s insert = match (contains1 r c , contains1 r (fst h) ) with
                                             | (true,true) -> insert (Leaf (region r,[h]) ) c s
                                             | (true,false) -> insert (new_tree (region r)) c s
                                             | (false, true) -> Leaf (region r,[h])
                                             | (false, false) -> new_tree (region r)  
end

module SE = struct 
  let region r = ( ( Region.midpoint_x r, Region.y1 r) , ( Region.x2 r, Region.midpoint_y r) )
  let contains1 r c = fst c > Region.x1 (region r) && fst c <= Region.x2 (region r) && snd c >= Region.y1 (region r) && snd c <= Region.y2 (region r)
  let contains2 r c = fst c > Region.x1 r && fst c <= Region.x2 r && snd c >= Region.y1 r && snd c <= Region.y2 r 
  let leaf_insert r c h s insert = match (contains1 r c , contains1 r (fst h) ) with
                                             | (true,true) -> insert (Leaf (region r,[h]) ) c s
                                             | (true,false) -> insert (new_tree (region r)) c s
                                             | (false, true) -> Leaf (region r,[h])
                                             | (false, false) -> new_tree (region r)  
end 


let contains_c (q: 'a quadtree) (c: coord) : bool = 
  match q with 
  | Leaf (r,_) | Node (r,_,_,_,_) -> fst c >= Region.x1 r && fst c <= Region.x2 r && snd c >= Region.y1 r && snd c <= Region.y2 r


let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree  =
  let check_leaf = fun y z h -> match z with
                                | 1 -> NE.leaf_insert y c h s insert
                                | 2 -> NW.leaf_insert y c h s insert
                                | 3 -> SW.leaf_insert y c h s insert
                                | 4 -> SE.leaf_insert y c h s insert
                                | _ -> new_tree y 
    in
  let check_node = fun q z -> match q with
                              Leaf (r,_) | Node (r,_,_,_,_) -> match z with 
                                                               | 1 -> if NW.contains2 r c then insert q c s else q
                                                               | 2 -> if NE.contains2 r c then insert q c s else q
                                                               | 3 -> if SW.contains2 r c then insert q c s else q
                                                               | 4 -> if SE.contains2 r c then insert q c s else q
                                                               | _ -> q                       
    in
  if contains_c q c then 
    match q with 
    | Leaf (x,[]) -> Leaf (x,[(c,s)])
    | Leaf (x,h::t) -> if Region.min_diagonal_check (NW.region x) then  Node (x, check_leaf x 1 h, check_leaf x 2 h, check_leaf x 3 h, check_leaf x 4 h) 
                                            else Leaf (x, (c,s) :: h :: t)
    | Node (x, qt1, qt2, qt3, qt4) -> Node (x, check_node qt1 1, check_node qt2 2, check_node qt3 3, check_node qt4 4)
  else 
    raise OutOfBounds
  
							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a 
  =
  match t with 
  | Leaf (r,lst) -> List.fold_left f a lst
  | Node (r, qt1, qt2, qt3, qt4) -> fold_quad f (fold_quad f (fold_quad f (fold_quad f a qt1) qt2) qt3) qt4
	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
  =
  match t with 
  | Leaf (y,lst) -> List.fold_left (fun acc x -> if Region.contains r (fst x) then f acc x else acc) a lst
  | Node (y, qt1, qt2, qt3, qt4) -> fold_region f (fold_region f (fold_region f (fold_region f a qt1 r) qt2 r) qt3 r) qt4 r

