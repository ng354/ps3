open Nats
open Assertions
open Array



TEST_UNIT "mult_overflows" = assert_true(mult_overflows max_int 2)
TEST_UNIT "mult_overflows" = assert_true(mult_overflows max_int 3)
TEST_UNIT "mult_overflows" = assert_false(mult_overflows 150 350)

TEST_UNIT "IntNat" = assert_true(IntNat.zero = 0)
TEST_UNIT "IntNat" = assert_true(IntNat.one = 1)
TEST_UNIT "IntNat" = assert_true(IntNat.( + ) IntNat.one IntNat.one = 2)
TEST_UNIT "IntNat" = assert_raises (Some IntNat.Unrepresentable) (fun () -> IntNat.( + ) max_int IntNat.one) ()


TEST_UNIT "IntNat" = assert_true(IntNat.( * ) IntNat.one IntNat.one = 1)
TEST_UNIT "IntNat" = assert_true(IntNat.( * ) IntNat.one IntNat.zero = 0)
TEST_UNIT "IntNat" = assert_raises (Some IntNat.Unrepresentable) (fun () -> IntNat. ( * ) max_int 2) ()
TEST_UNIT "IntNat" = assert_true(IntNat.( < ) IntNat.zero IntNat.one )
TEST_UNIT "IntNat" = assert_false(IntNat.( < ) IntNat.one IntNat.zero)
TEST_UNIT "IntNat" = assert_true(IntNat.( < ) IntNat.zero max_int)
TEST_UNIT "IntNat" = assert_false(IntNat.( === ) IntNat.zero IntNat.one )
TEST_UNIT "IntNat" = assert_true(IntNat.( === ) 4 4 )
TEST_UNIT "IntNat" = assert_true(IntNat.int_of_nat IntNat.zero = 0 )
TEST_UNIT "IntNat" = assert_true(IntNat.int_of_nat IntNat.one = 1 )
TEST_UNIT "IntNat" = assert_true(IntNat.int_of_nat 4 = 4 )
TEST_UNIT "IntNat" = assert_true(IntNat.nat_of_int 1 = 1 )
TEST_UNIT "IntNat" = assert_true(IntNat.nat_of_int 10 = 10 )

(*creates list of length i * 2 in order to create lists of length greater than max_int*)


let big_list1 = Array.to_list (Array.make 1000000 1)
let big_list2 = Array.to_list (Array.make 32768 1)

TEST_UNIT "ListNat" = assert_true(ListNat.zero = [])
TEST_UNIT "ListNat" = assert_true(ListNat.one = [1])
TEST_UNIT "ListNat" = assert_true(ListNat.( + ) [1;2;3] [5;6;7] = [7;6;5;1;2;3]  )
TEST_UNIT "ListNat" = assert_true(ListNat.( + ) ListNat.one ListNat.one = [1;1] )
TEST_UNIT "ListNat" = assert_true(ListNat.( * ) [2;4;5] [6;7;8;9;0] = [5; 4; 2; 5; 4; 2; 5; 4; 2; 5; 4; 2; 5; 4; 2])
TEST_UNIT "ListNat" = assert_true(ListNat.( * ) ListNat.one ListNat.zero = [] )
TEST_UNIT "ListNat" = assert_false(ListNat.( < ) ListNat.one ListNat.zero)
TEST_UNIT "ListNat" = assert_true(ListNat.( === ) [1;1;1;1] [2;3;4;5])
TEST_UNIT "ListNat" = assert_true(ListNat.( === ) ListNat.zero ListNat.zero )
TEST_UNIT "ListNat" = assert_true(ListNat.int_of_nat big_list1 = 1000000)
TEST_UNIT "ListNat" = assert_true(ListNat.nat_of_int 1000000 = big_list1)
TEST_UNIT "ListNat" = assert_true(ListNat.nat_of_int 10 = [1;1;1;1;1;1;1;1;1;1])
TEST_UNIT "ListNat" = assert_true(ListNat.nat_of_int 0 = [])


module X = NatConversionFn(ListNat)

TEST_UNIT "NatConversionFn_test" = assert_true(X.int_of_nat [] = 0)
TEST_UNIT "NatConversionFn_test" = assert_true(X.int_of_nat [1;2;3;4] = 4)
TEST_UNIT "NatConversionFn_test" = assert_true(X.nat_of_int 10 = [1;1;1;1;1;1;1;1;1;1])
TEST_UNIT "NatConversionFn_test" = assert_true(X.nat_of_int 0 = [])
TEST_UNIT "NatConversionFn_test" = assert_true(X.int_of_nat [2;3;4;5;2;5] = 6)


module AlienTest : AlienMapping with type aliensym = int*string = struct
  type aliensym = int*string
  let int_of_aliensym a = fst a 
  let zero =  (0,"zero")
  let one = (1,"one")
  let ten = (10,"ten")
end

module Y = AlienNatFn(AlienTest)

TEST_UNIT "AlienNatFn_test" = assert_true(Y.zero = [(0,"zero")])
TEST_UNIT "AlienNatFn_test" = assert_true(Y.one = [(1,"one")])
TEST_UNIT "AlienNatFn_test" = assert_true(Y.( + ) (Y.one@Y.one) (Y.one@Y.zero) = [(1, "one"); (1, "one"); (1, "one"); (0, "zero")] )
TEST_UNIT "AlienNatFn_test" = assert_true(Y.( * ) (Y.one@Y.one@Y.one) (Y.one@Y.zero)=
             [(0, "zero"); (0, "zero"); (1, "one"); (1, "one"); (0, "zero"); (1, "one"); (0, "zero")] )
TEST_UNIT "AlienNatFn_test" = assert_true(Y.( * ) [(5,"five");(2,"two")] (Y.one@[(9,"nine")]) = 
	[(0, "zero"); (9, "nine"); (9, "nine"); (9, "nine"); (9, "nine"); (9, "nine");   (1, "one"); (1, "one"); (1, "one"); (1, "one"); (1, "one"); (1, "one");        
	 (1, "one"); (9, "nine"); (9, "nine")])
TEST_UNIT "AlienNatFn_test" = assert_true(Y.( * ) [(2,"");(3,"");(9,"")] [(10,""); (2,"")] =
	[(0, "zero"); (2, ""); (2, ""); (2, ""); (10, ""); (10, ""); (10, "");           (10, ""); (10, ""); (2, ""); (2, ""); (10, ""); (10, ""); (10, "");            
	 (10, ""); (10, ""); (10, ""); (10, ""); (10, ""); (10, ""); (2, ""); 
	 (2, ""); (2, ""); (2, ""); (2, ""); (2, ""); (2, ""); (2, ""); (2, "")] )

TEST_UNIT "AlienNatFn_test" = assert_false(Y.( < ) (Y.one@Y.one@Y.one) (Y.one@Y.zero@Y.one) )
TEST_UNIT "AlienNatFn_test" = assert_true(Y.( < ) [(3,"");(10,"")] ([(2,"");(3,"");(11,"")]@Y.one) )
TEST_UNIT "AlienNatFn_test" = assert_true(Y.( === ) (Y.one@Y.one@Y.one) (Y.one@Y.zero@Y.one@Y.one) )
TEST_UNIT "AlienNatFn_test" = assert_true(Y.int_of_nat [(2,"");(3,"");(9,"")]  = 14)
TEST_UNIT "AlienNatFn_test" = assert_true(Y.int_of_nat (Y.one@Y.zero@Y.one@Y.one) = 3)
TEST_UNIT "AlienNatFn_test" = assert_true(Y.nat_of_int 10 = [(0,"zero");(1,"one");(1,"one");(1,"one");(1,"one");(1,"one");(1,"one");(1,"one");(1,"one");(1,"one");(1,"one")])
TEST_UNIT "AlienNatFn_test" = assert_raises (Some Y.Unrepresentable) (fun () -> Y.nat_of_int (-1)) ()




let () = Pa_ounit_lib.Runtime.summarize()