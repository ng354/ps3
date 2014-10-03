open City_search
open Assertions

let ithaca = load_city_data "ithaca.csv"

TEST_UNIT "city_search_test1" = assert_true( city_search ithaca ((42.44,-76.5),(42.5,-76.4)) = ["First Presbyterian Church"; "Fall Creek School"] )
TEST_UNIT "city_search_test2" = assert_true( city_search ithaca ((42.43,-76.55),(42.5,-76.48)) = 
	["Oldport Harbour"; "Greater Ithaca Activities Center"; "First Presbyterian Church"; "0alvary Baptist Church"; "Fall Creek School";    
 "South Hill School"])
(*Checking a region with a diagonal less than the min_diagonal constant*)
TEST_UNIT "city_search_test3" = assert_true( city_search ithaca ((42.43673949,-76.47660641),(42.43673951,-76.47660639))  = ["Belle Sherman School"] )





let () = Pa_ounit_lib.Runtime.summarize()