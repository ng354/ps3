open Quadtree
open Assertions


TEST_UNIT "new_tree" = assert_true( new_tree ((0.,0.),(100.,100.)) = Leaf (((0., 0.), (100., 100.)), []) )
TEST_UNIT "new_tree" = assert_true( new_tree ((0.,0.),(0.,0.)) = Leaf (((0., 0.), (0.,0.)), []) )

let tree0 = new_tree ((0.,0.),(100.,100.))
let tree1 = insert tree0 (50.,50.) "first" 
let tree2 = insert tree1 (75.,75.) "second"
let tree3 = insert tree2 (20.,50.) "third" 
let tree4 = insert tree3 (75.,50.) "fourth"
let tree5 = insert tree1 (50.,75.) "fifth"
let tree6 = insert tree4 (25.,25.) "sixth"

let small0 = new_tree ((0.,0.),(10.**(-14.) /. 2. , 10.**(-14.) /. 2. ) ) 
let small1 = insert small0 (10.**(-14.) /. 3., 10.**(-14.) /. 3.) "first"
let small2 = insert small1 (10.**(-14.) /. 4., 10.**(-14.) /. 4.) "second"



(*checks if an excpetion is thrown if the coordinate is outside the region*)
TEST_UNIT "insert" = assert_raises (Some OutOfBounds) (fun () -> insert tree0 (-1.,-1.) "fail") ()
(*Insert in middle of graph*)
TEST_UNIT "insert" = assert_true( tree1 = Leaf (((0., 0.), (100., 100.)), [((50., 50.), "first")] ) ) 
(*"first" is in middle so it gets pushed to SW and "second" is in the NE region*)
TEST_UNIT "insert" = assert_true( tree2 = Node (((0., 0.), (100., 100.)),
	                                           Leaf (((50., 50.), (100., 100.)), [((75., 75.), "second")]),                   
                                               Leaf (((0., 50.), (50., 100.)), []),
                                               Leaf (((0., 0.), (50., 50.)), [((50., 50.), "first")]),
                                               Leaf (((50., 0.), (100., 50.)), [])) )

(*"third" is on the border of NW and SW so it should be mapped to NW*)
TEST_UNIT "insert" = assert_true( tree3 =  Node (((0., 0.), (100., 100.)),
                                               Leaf (((50., 50.), (100., 100.)), [((75., 75.), "second")]),
                                               Leaf (((0., 50.), (50., 100.)), [((20., 50.), "third")]),
                                               Leaf (((0., 0.), (50., 50.)), [((50., 50.), "first")]),
                                               Leaf (((50., 0.), (100., 50.)), [])) )

(*"fourth" is on the border of NE and SE so it should be mapped to SE*)
TEST_UNIT "insert" = assert_true( tree4 = Node (((0., 0.), (100., 100.)),
                                               Leaf (((50., 50.), (100., 100.)), [((75., 75.), "second")]),                 
                                               Leaf (((0., 50.), (50., 100.)), [((20., 50.), "third")]),
                                               Leaf (((0., 0.), (50., 50.)), [((50., 50.), "first")]),
                                               Leaf (((50., 0.), (100., 50.)), [((75., 50.), "fourth")])) )

(*"fifth" is on the border of NE and NW and should map to NE *)
TEST_UNIT "insert" = assert_true( tree5 = Node (((0., 0.), (100., 100.)),
                                               Leaf (((50., 50.), (100., 100.)), [((50., 75.), "fifth")]),                  
                                               Leaf (((0., 50.), (50., 100.)), []),
                                               Leaf (((0., 0.), (50., 50.)), [((50., 50.), "first")]),
                                               Leaf (((50., 0.), (100., 50.)), [])) )

(*"sixth" is on the border ot the SW and NW regions of the new Node created. 
* It should map to SW. This tested whether a node is created correctly *)
TEST_UNIT "insert" = assert_true( tree6 = Node (((0., 0.), (100., 100.)),
                                               Leaf (((50., 50.), (100., 100.)), [((75., 75.), "second")]),
                                               Leaf (((0., 50.), (50., 100.)), [((20., 50.), "third")]),
                                               Node (((0., 0.), (50., 50.)),
                                                    Leaf (((25., 25.), (50., 50.)), [((50., 50.), "first")]),
                                                    Leaf (((0., 25.), (25., 50.)), []),
                                                    Leaf (((0., 0.), (25., 25.)), [((25., 25.), "sixth")]),
                                                    Leaf (((25., 0.), (50., 25.)), [])),
                                               Leaf (((50., 0.), (100., 50.)), [((75., 50.), "fourth")])) )

(*checks that when the region of a leaf has a diagonal less than min_diagonal, 
* insert will append the list instead of creating a new node *)
TEST_UNIT "insert" = assert_true( let helper a = match a with 
	                                             | Leaf (r,x) -> List.length x 
	                                             | _ -> 0
	                              in (helper small2) = 2 )

(*checking for trees with nodes*)
TEST_UNIT "fold_quad" = assert_true( fold_quad (fun a x -> a + 1) 0 tree6 = 5 ) 
TEST_UNIT "fold_quad" = assert_true( fold_quad (fun a x -> a + String.length (snd x)) 0 tree6 = 27 ) 
(*checking for a tree with just a leaf*)
TEST_UNIT "fold_quad" = assert_true( fold_quad (fun a x -> a + String.length (snd x)) 0 tree1 = 5 ) 
(*check if fold_quad works when the list of a leaf has more than one element*)
TEST_UNIT "fold_quad" = assert_true( fold_quad (fun a x -> a + String.length (snd x)) 0 small2 = 11)



(*fold_region will include coordinates on the border of regions*)
TEST_UNIT "fold_region" = assert_true( fold_region (fun a x -> a + 1) 0 tree6 ((50.,0.),(100.,100.)) = 3 )
TEST_UNIT "fold_region" = assert_true( fold_region (fun a x -> a + String.length (snd x)) 0 tree6 ((0.,0.),(50.,100.)) = 15 )

(*checking if fold_region works when the list of a leaf has more than one element*)
TEST_UNIT "fold_region" = assert_true( fold_region (fun a x -> a + String.length (snd x)) 0 small2 ((0.,0.),(1.,1.)) = 11)
(*checks that fold region can restrict the region and work for a list of more than one element*)
TEST_UNIT "fold_region" = assert_true( fold_region (fun a x -> a + String.length (snd x)) 0 small2 ((0.,0.),(10.**(-14.) /. 4., 10.**(-14.) /. 4.)) = 6)





let () = Pa_ounit_lib.Runtime.summarize()