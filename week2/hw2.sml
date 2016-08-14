(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, lst) =
  case lst of
      [] => NONE
    | x::xs => if x=s
	       then SOME xs
	       else let val res = all_except_option (s, xs)
		    in
			case res of
			    NONE => NONE
			  | SOME l => SOME (x::l)
		    end

fun get_substitutions1 (xss, s) =
  case xss of
      [] => []
    | xs::xss' => let val res = all_except_option(s, xs)
		  in
		      case res of
			  NONE => get_substitutions1(xss', s)
			| SOME l => l @ get_substitutions1(xss', s)
		  end
		      
fun get_substitutions2 (xss, s) =
  let fun helper (xss, s, acc) =
	case xss of
	    [] => acc
	  | xs::xss' => let val res = all_except_option(s, xs)
			in
			    case res of
				NONE => helper(xss', s, acc)
			      | SOME l => helper(xss', s, l@acc)
			end
  in
      helper(xss,s,[])
  end

fun similar_names (xss, full_name) =
  let fun helper (fnames, m, l) =
	case fnames of
	    [] => []
	  | x::xs => {first=x,middle=m,last=l} :: helper(xs, m, l)
  in
      case full_name of
	  {first=x,middle=y,last=z} => let val names = get_substitutions2(xss, x)
				       in
					   helper(x::names, y, z)
				       end
  end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color c =
  case c of
      (Clubs, _) => Black
    | (Spades, _) => Black
    | (Diamonds, _) => Red
    | (Hearts, _) => Red

fun card_value c =
  case c of
      (_, Num x) => x
    | (_, Ace) => 11
    | _ => 10

fun remove_card (cs, c, e) =
  case cs of
      [] => raise e
    | x::xs => if x=c
	       then xs
	       else x::remove_card(xs,c,e)

fun all_same_color cs =
  case cs of
      [] => true
    | x::[] => true
    | x1::x2::xs => if card_color x1 <> card_color x2
		    then false
		    else all_same_color(x2::xs)

fun sum_cards cs =
  let fun helper (cs, acc) =
	case cs of
	    [] => acc
	  | x::xs => helper(xs, card_value x + acc)
  in
      helper(cs,0)
  end
	      
fun score (cs, goal) =
  let val raw_score = sum_cards(cs)
      val preliminary_score = 
	  if raw_score > goal
	  then 3*(raw_score - goal)
	  else goal - raw_score
  in
      if all_same_color(cs)
      then preliminary_score div 2
      else preliminary_score
  end

fun officiate (cs, ms, goal) = 
  let fun held_cards (cs, ms, hc, goal) =
	case cs of
	    [] => score(hc, goal)
	  | c::cs' => case ms of
			  [] => score(hc, goal)
			| m::ms' => case m of
					Draw => let val raw_score = sum_cards(c::hc)
						in
						    if raw_score > goal
						    then score(c::hc, goal)
						    else held_cards(cs', ms', c::hc, goal)
						end
				      | Discard x => let val new_hd = remove_card(hc, x, IllegalMove)
						     in
							 held_cards(cs, ms', new_hd, goal)
						     end
  in
      held_cards(cs,ms,[],goal)
  end
