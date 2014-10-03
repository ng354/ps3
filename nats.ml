(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
  type t

  (*Multiplication ( * ) identity of zero : a * zero === zero
  * Addition ( + ) identity of zero : a + zero === a *)
  val zero : t

  (*Multiplication ( * ) identity of one : a * one === a *)
  val one : t

  (* ( + ) is associative : (a+b)+c === a+(b+c) 
  * ( + ) is communitive : a+b === b+a 
  * Precondition: type t can be int
  Postcondition: return the sum of type t *)
  val ( + ) : t -> t -> t
  (* ( * ) is associative : (a * b) * c === a * (b * c) 
  * ( * ) is communitive : a * b === b * a 
  * ( * ) is distributive over ( + ) : 
  * (a * (b + c)) === (a * b) + (a * c) *)
  val ( * ) : t -> t -> t 
  val ( < ) : t -> t -> bool
  val ( === ) : t -> t -> bool
			    
  exception Unrepresentable

  val int_of_nat: t -> int
  val nat_of_int: int -> t
end

module type AlienMapping = sig
  type aliensym

  val int_of_aliensym: aliensym -> int
  val one: aliensym
  val zero: aliensym
end


(* END: DO NOT CHANGE THIS CODE *)

(* Add your solution here for IntNat, ListNat, NatConvertFn, 
   and AlienNatFn, being careful to use the declarations and
   types specified in the problem set. *)

type sign = Positive | Negative
let sign_int ( n : int ) : sign =
  if n >= 0 then Positive else Negative
let sum_overflows ( i1 : int ) ( i2 : int ) : bool =
  sign_int i1 = sign_int i2 && sign_int ( i1 + i2 ) <> sign_int i1
let mult_overflows (i1:int) (i2:int) : bool =
  if i1 = 0 || i2 = 0 then false else
  if sign_int i1 = sign_int i2 then sign_int (i1*i2) <> Positive || i1*i2 < max i1 i2 else
    sign_int (i1*i2) <> Negative ||  i1*i2 > (~-) (max i1 i2)

module IntNat : NATN with  type t = int = struct
  type t = int
  exception Unrepresentable
  let zero = 0
  let one = 1
  let ( + ) a b = if sum_overflows a b then raise Unrepresentable else Pervasives.(+) a b
  let ( * ) a b = if mult_overflows a b then raise Unrepresentable else Pervasives.( * ) a b
  let ( < ) a b = a < b
  let ( === ) a b = a = b
  let int_of_nat a = a
  let nat_of_int a = if a < 0 then raise Unrepresentable else a 
end 


(* This helper function checks overflow. Returns boolean if there are max_int elements
int lst 1 and the remaining elements exist in lst 2. If lst 2 exists, 
then we know that there is an "overflow" in lst 1 *)
let check_overflow (lst : int list) : bool = 
	let x = List.fold_left (fun acc x -> let (fst, snd, trd) = acc in if trd = max_int then (fst,x::snd, trd+1) 
				else (x::fst,snd,trd+1)) ([], [], 0) lst in let (fst, snd, trd) = x in 
	 			match snd with 
	 			[] -> false
	 			| _ -> true

module ListNat : NATN with type t = int list = struct
(*The list [a1;...;an] represents the natural number n. This is, 
the list lst represents length(lst). The empty list represents 0. 
The values of the list elements are irrelevant. *)
	type t = int list 
	exception Unrepresentable
	let zero = []
	let one = [1]
	let (+) a b = List.fold_left (fun acc x -> x::acc) a b 
                
	let ( * ) a b = List.fold_left (fun acc x -> List.rev_append a acc) [] b  
                  
	let ( < ) a b =  let less_than = List.fold_left (fun acc x -> begin match acc with
                                                                      | (false, []) -> (true, [])
                                                                      | (false, h::t) -> (false, t)
                                                                      | (true, _ ) -> (true, [])   
                                                                end ) (false, a) b
                   in fst less_than

	let ( === ) a b = let equals = List.fold_left (fun acc x -> begin match acc with
                                                      | (true, []) -> (false, [])
                                                      | (true, h::t) -> (true, t)
                                                      | (false, _ ) -> (false, []) 
                                                 end ) (true, a) b
                    in fst equals
                    
	let int_of_nat a = let l = List.fold_left (fun acc x -> Pervasives.(+) acc 1) 0 a
                     in if check_overflow a then raise Unrepresentable else l
	let nat_of_int a = let rec concat (f: int list -> int list) (acc: int list) (x : int) : int list = 
		                    match x with 
		                    | 0 -> acc
		                    | _ -> concat f (f acc) (x-1)
		                 in if Pervasives.( < ) a 0 then raise Unrepresentable else concat (fun acc -> 1::acc) [] a
end




(*This module will take t of any type and will convert it 
to a natrual number. Here, there is no previous knowledge of the 
representation type t. *)
module NatConversionFn (N: NATN) = struct
	let int_of_nat (n: N.t) : int = N.int_of_nat n
  let nat_of_int (n: int) : N.t = N.nat_of_int n
end





(* binary representation of naturals ex 10011 is = 19 because
2^4 + 0 + 0 + 2^1 + 2^0 = 19. In a nested way, 10011 with base 2
(((1*2+0)*2+0)*2+1)*2+1 = 19
(for n bit number, (b(n-1) * 2 + b(n-2)*2 + b(n-3)*2..)*2+b(0)*)








module AlienNatFn ( M : AlienMapping ): (NATN with type t = M.aliensym list) = struct
  type t = M.aliensym list
  let zero = [M.zero]
  let one = [M.one]
  let ( + ) a b = List.rev_append a b 
  let ( * ) a b = let rec mult_aliensym (f: M.aliensym list -> M.aliensym -> M.aliensym list ) (acc: M.aliensym list) (i:int) (sym: M.aliensym) : M.aliensym list =
                    match i with 
                    | 0 -> acc
                    | _ -> mult_aliensym f (f acc sym) (i-1) sym
                  in
                  let mult_aliensym_list (c: M.aliensym) (d: M.aliensym list) : M.aliensym list =
                    List.fold_left (fun acc x -> List.rev_append acc (mult_aliensym (fun acc1 x1 -> x1::acc1) [] (M.int_of_aliensym c) x )  ) [] d
                  in 
                  M.zero :: (List.fold_left (fun acc x -> List.rev_append acc (mult_aliensym_list x b) ) [] a )

  let less_than_helper (lst : M.aliensym list) : M.aliensym list =
    List.fold_left (fun acc x -> let rec helper (f: t -> t) (acc: t) (i : int) : t =
                                   match i with
                                   | 0 -> acc
                                   | _ -> helper f (f acc) (i-1)
                                 in List.rev_append (helper (fun acc1 -> M.one::acc1) [] (M.int_of_aliensym x) ) acc ) [] lst


  let ( < ) a b = let less_than = List.fold_left (fun acc x -> match acc with
                                                               | (false,[]) -> (true,[]) 
                                                               | (false, h::t) -> (false,t)
                                                               | (true, _ ) -> (true,[])
                                                                                            ) (false,less_than_helper a) (less_than_helper b) 
                  in fst less_than
  

  let ( === ) a b = (less_than_helper a) = (less_than_helper b)

  exception Unrepresentable

  let int_of_nat a = List.fold_left (fun acc x -> if sum_overflows (M.int_of_aliensym x) acc then raise Unrepresentable else Pervasives.( + ) (M.int_of_aliensym x) acc) 0 a

  let nat_of_int a = let rec helper (f: t -> t) (acc: t) (i: int) : t =
                       match i with
                       | 0 -> M.zero::acc
                       | _ -> helper f (f acc) (i-1)
                     in if Pervasives.( < ) a 0 then raise Unrepresentable else helper (fun b -> M.one::b) [] a
end
