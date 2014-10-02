type coord = float * float
type region = coord * coord
(*A quadtree is a structure that provides a representation of
2 dimensional space. This function implements a quadtree when a leaf node
represents a set of objects, and a non-leaf node represents a 
rectangular region of space with values of x0, x1, y0, and y1. The recursive calls to 
the quadtree, which creates the quadtrants separated by regions of space are type 
'a quadtree. A coord is a tuple of floats that has a pair of an x coordinate and 
a y coordinate. The first coord of a region represents the lower-left coordinate 
of that space and the second coord the upper-right. A region is a type that contains 
the two coords and defines the space. The type 'a quadtree represents the nodes of the 
NE, NW, SW, and SE quadrants of the region. *)
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)

(*If a region already has a diagonal length from lower left coordinate
to upper right coordinate, that is less than the constant min_diagonal, 
then the leaf remains a leaf and grows in length with each new object
added to the lead. We have set the constant of min_diagonal. *)		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds

(*Requires a region which is a tuple of two coordinates and returns a new
quadtree that contains the points within that given region. The quadtree is 
traversed starting at the root, until a leaf node is reached. The set of
objects at the leaf node will be examined.*)
let new_tree (r:region) : 'a quadtree = 
  Leaf (r,[])


module Region = struct
  let x1 r = fst (fst r)
  let y1 r = snd (fst r)
  let x2 r = fst (snd r)
  let y2 r = snd (snd r)
  (*takes the midpoint of the space parameterized by the x1 and
  x2 values.*)
  let midpoint_x r = ((x1 r) +. (x2 r))/.2.0
  (*takes the midpoint of the space parameterized by the y1 and
  y2 values.*)
  let midpoint_y r = ((y1 r) +. (y2 r))/.2.0
  (*This function checks if the region's length from the bottom-left coordinate
  to the upper right coordinate is less than the constant min_diagonal. Returns
  true if the length is greater than the min_diagonal otherwise false.*)
  let min_diagonal_check r = sqrt((x2 r  -. x1 r)**2.0 +. (y2 r  -. y1 r)**2.0) >= min_diagonal
  (*This function checks if the space parameterized by the four values
  is contained in the region*)
  let contains r c = fst c >= x1 r && fst c <= x2 r && snd c >= y1 r && snd c <= y2 r
end

module NE = struct 
(*repesents the first quadtree in the Node*)
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
(*repesents the second quadtree in the Node*)
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
(*repesents the third quadtree in the Node*)
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
(*repesents the fourth quadtree in the Node*)
  let region r = ( ( Region.midpoint_x r, Region.y1 r) , ( Region.x2 r, Region.midpoint_y r) )
  let contains1 r c = fst c > Region.x1 (region r) && fst c <= Region.x2 (region r) && snd c >= Region.y1 (region r) && snd c <= Region.y2 (region r)
  let contains2 r c = fst c > Region.x1 r && fst c <= Region.x2 r && snd c >= Region.y1 r && snd c <= Region.y2 r 
  let leaf_insert r c h s insert = match (contains1 r c , contains1 r (fst h) ) with
                                             | (true,true) -> insert (Leaf (region r,[h]) ) c s
                                             | (true,false) -> insert (new_tree (region r)) c s
                                             | (false, true) -> Leaf (region r,[h])
                                             | (false, false) -> new_tree (region r)  
end 

(*This function checks if the coordinates are present
in the regions specified within the quadtree. Returns true
if contained in the region, otherwise false. *)
let contains_c (q: 'a quadtree) (c: coord) : bool = 
  match q with 
  | Leaf (r,_) | Node (r,_,_,_,_) -> fst c >= Region.x1 r && fst c <= Region.x2 r && snd c >= Region.y1 r && snd c <= Region.y2 r

(*Requires type 'a quadtree, coordinate, and object 'a. This function inserts
object at the given coordinate and returns the quadtree, with that object mapped
to the coordinate. The function will raise OutOfBounds if the coordinates 
given are outside the region represented by the 'a quadtree.*)
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree  =
  (*helper function to match the leaf node with a set of objects.*)
  let check_leaf = fun y z h -> match z with
                                | 1 -> NE.leaf_insert y c h s insert
                                | 2 -> NW.leaf_insert y c h s insert
                                | 3 -> SW.leaf_insert y c h s insert
                                | 4 -> SE.leaf_insert y c h s insert
                                | _ -> new_tree y 
    in
  (*this function matches the nodes and see if it is contained within
  the given region *)
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
  
(*Requires object 'a and applies it to the coordinate and 'b tuple. The 
function type 'a is folded over the given 'b quadtree, starting with the given
accumulator 'a. The function argument is applied to every object in the 
'b quadtree, and returns an 'a, a new accumulator that will be used for every
recursive call in the fold. *)			      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a =
  match t with 
  | Leaf (r,lst) -> List.fold_left f a lst
  | Node (r, qt1, qt2, qt3, qt4) -> fold_quad f (fold_quad f (fold_quad f (fold_quad f a qt1) qt2) qt3) qt4

(*This function folds the function argument to every object the quadtree
that are within the region argument. Precondition: The region may not be 
entirely contained within the same subtree. Returns: accumulator 'a that will
be used in the recursive call for folding over the quadtree.*)	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
  =
  match t with 
  | Leaf (y,lst) -> List.fold_left (fun acc x -> if Region.contains r (fst x) then f acc x else acc) a lst
  | Node (y, qt1, qt2, qt3, qt4) -> fold_region f (fold_region f (fold_region f (fold_region f a qt1 r) qt2 r) qt3 r) qt4 r

