
------------------- MODULE Jugs -------------
(* this is die hard *)

EXTENDS Integers

(*******************************************)
(* this is a comment *)

(* careful with <= as => is implies eek*)

(*   =<   means less than or equal to  *)
(*   /=   means not equal to  *)
(*   # alternative means also not equal to same as /=  *)


(* two jugs , one is three gallon , other is five gallon *)

(* write proof informally first *)
(* let variable big represent big gallon jug  *)
(* let variable small represent small gallon jug *)

VARIABLES big , small


(* type variables *)
TypeOK == /\ small \in 0 .. 3
          /\ big \in 0 .. 5
	  

(* initially big jug empty and small jug empty  *)
Init == /\ big = 0
        /\ small = 0
	

(* fill small jug from tap *)
FillSmall == /\ small' = 3
             /\ big' = big

(* fill big jug from tap *)
FillBig == /\ big' = 5
           /\ small' = small

(* pour small jug onto floor *)
EmptySmall == /\ small' = 0
              /\ big' = big

(* pour big jug onto floor*)
EmptyBig == /\ big' = 0
            /\ small' = small


(* pour small jug into big jug stopping when big jug is full stop*)
(*  *)
(* liquid in small = small *)
(* free space in big = 5 - big *)
(* transfer amount =  min ( 5 - big , small ) *)
(*  *)
(* small = 0 then transfer = 0 *)
(* small = 1 /\ big = 5 then transfer = 0 *)
(* small = 1 /\ big = 4 then transfer = 1 *)
(* small = 3 /\ big = 0 then transfer = 3 *)
(*  *)
(*                                                                  free space in big *)
(* small       big                      big-small                       5-big          *)
(* 0           5 , 4, 3 , 2, 1 , 0      5 , 4, 3 , 2, 1 , 0              *)
(* 1	    5 , 4, 3 , 2, 1 , 0      4, 3 , 2, 1 , 0 , -1 *)
(* 2	    5 , 4, 3 , 2, 1 , 0      3 , 2, 1 , 0 , -1 , -2   *)
(* 3 	    5 , 4, 3 , 2, 1 , 0      2, 1 , 0 , -1 , -2 , -3   *)
(*  *)
(* case 2  big + small > 5   *)
(*  *)



LessOrEq(m,n) == \/ m < n
                 \/ m = n



SmallToBig == \/   ( /\ big + small > 5
	             /\ big' = 5
	             /\ small' = (big + small) - 5     (* small - (5 - big)  *)
		   )
	       \/  ( /\ LessOrEq(big + small , 5)
                     /\ big' = big + small
	             /\ small' = 0
		   )
              
(* pour big jug into small jug stopping when small jug is full *)
(* big jug 5 gallon *)
(* small jug 3 gallon  *)
(*  *)

(* shown big + small - 3 equivalent to small - (3 - big)  *)
(*  *)
(*  *)
(*  *)



BigToSmall == \/ ( /\ LessOrEq(big + small , 3)
                   /\ big' = 0
	           /\ small' =  big + small
		 )
              \/ ( /\ big + small > 3
	           /\ big' = (big + small) - 3   (* big' = small - (3 - big)  *)
	           /\ small' = 3
		 )


(* transitions that can do  *)
(* fill small jug *)
(* fill big jug *)
(* empty small jug *)
(* empty big jug *)
(* pour big jug into small jug *)
(* pour small jug into big jug *)

Next == \/ FillSmall
        \/ EmptySmall
        \/ FillBig
	\/ EmptyBig
	\/ SmallToBig
	\/ BigToSmall

(*  *)
(*  *)
(*  *)
(*  *)
(*  *)
(*  *)
(*  *)
(*  *)
(*  *)
(*  *)



===============================================


