(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Nathanial Tranel
* njtranel@gmail.com
*
***************************************************************)
Control.Print.printDepth := 1024;

(* Datatype for set - either Empty or Set of an element and another Set *)
datatype 'element set = Empty | Set of 'element * 'element set;

(* Function to determine if e is contained in set *)
fun isMember e set =
	case set of
		Empty => false |					(* if set is Empty, then e is not a member of set *)
		Set(h,t) =>
			if e=h then true				(* if set is Set, compare the first element to e and return true, *)
			else isMember e t;			(* but if not do isMember with the Set in set *)

(* Function to convert a list type to type set *)
fun list2Set [] = Empty												(* if passed the empty list, return set with value Empty *)
|   list2Set (x::xs) =												(* otherwise, proceed with recursive case *)
			let																																														(* define a function to remove duplicates from list *)
				fun remove_duplicates [] = []																																(* if list is empty, return empty list *)
				| remove_duplicates (y::ys) = y::remove_duplicates(List.filter (fn z => z <> y) ys)					(* otherwise filter the list, returning only unique elements *)
			in
				let
					val s = list2Set(xs);																					(* create the set for all elements but the head *)
				in																															(* before putting on the head, check to make sure there are not duplicates *)
					if isMember x s then list2Set(remove_duplicates(x::xs))				(* if there are, then build a set from the list of unique elements *)
					else Set(x,s)																									(* otherwise tack on the head of the list and return the new set *)
				end
			end;

(* Function that takes two sets and returns the mathematical union of the sets *)
fun union set1 set2 =
		case set1 of																			(* check set2 against set1 *)
			Empty => set2	|																	(* if set1 is empty, return set2 *)
			Set(h,t) =>																			(* otherwise, if set1 contains values, separate them into the head and tail *)
				let
					val s = Set(h, set2)												(* define a variable s to represent the created set of the head of set1 and set2 *)
				in
					if isMember h set2 then union t set2				(* if the head value of set1 is a member of set2, then retry the union function with the tail of set1 and set2 *)
					else union t s 															(* otherwise, repeat union with the tail of set1 and the set created earlier *)
				end;

(* Function that takes two sets and returns the matematical intersection of the sets *)
fun intersect set1 set2 =
	case set1 of																									(* as in union, check set2 against set1 *)
		Empty => Empty |																						(* if set1 is empty, return an Empty set *)
		Set(h,t) =>																									(* otherwise, divide set1 into a head and tail *)
			if isMember h set2 then Set(h, (intersect t set2))				(* if the head of set1 is a member of set2, then create a set with the head of set1 and the intersection of the rest of set1 and set2 *)
			else intersect t set2;																		(* otherwise, repeat intersect with the tail of set1 and set2 *)

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

fun f [] = [] (* a *)
|   f (x::xs) = (x + 1) :: (f xs); (* b *)

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
