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
	case ms of
	    [] => score(hc, goal)
	  | m::ms' => case m of
			  Discard x => let val removed = remove_card(hc, x, IllegalMove)
				       in
					   held_cards(cs, ms', removed, goal)
				       end
			| Draw => case cs of
				      [] => score(hc, goal)
				    | c::cs' => let val card_sum = sum_cards(c::hc)
						in
						    if card_sum > goal
						    then score(c::hc, goal)
						    else held_cards(cs', ms', c::hc, goal)
						end
  in
      held_cards(cs, ms, [], goal)
  end

(*fun officiate2 (cs, ms, goal) = 
  let fun held_cards (cs, ms, hc, goal) =
	case ms of
	    [] => hc
	  | m::ms' => case m of
			  Discard x => let val new_hd = remove_card(hc, x, IllegalMove)
				       in
					   held_cards(cs, ms', new_hd, goal)
				       end
			| Draw => case cs of
				      [] => hc
				    | c::cs' => let val raw_score = sum_cards(c::hc)
						in
						    if raw_score > goal
						    then c::hc
						    else held_cards(cs', ms', c::hc, goal)
						end
  in
      held_cards(cs,ms,[],goal)
  end
 *)

fun score_challenge (cs, goal) =
  let fun smallest_sum_card (cs,acc,ace) =
	case cs of
	    [] => (acc,ace)
	  | (_, Num x)::cs' => smallest_sum_card(cs',x+acc,ace)
	  | (_, Ace)::cs' => smallest_sum_card(cs',1+acc,1+ace)
	  | _::cs' => smallest_sum_card(cs',10+acc,ace)

      val (smallest,ace) = smallest_sum_card(cs,0,0)
      val preliminary = if goal <= smallest
			then 3*(smallest - goal)
			else let val n = (goal - smallest) div 10
			     in
				 if ace <= n
				 then goal - (smallest + ace*10)
				 else Int.min(goal-smallest-n*10, 3*(smallest+10*n+10-goal))
			     end
  in
      if all_same_color(cs)
      then preliminary div 2
      else preliminary
  end

fun officiate_challenge (cs, ms, goal) = 
  let fun smallest_sum_cards cs =
	case cs of
	    [] => 0
	  | c::cs' => case c of
			  (_,Num x) => x + smallest_sum_cards cs'
			| (_,Ace) => 1 + smallest_sum_cards cs'
			| _ => 10 + smallest_sum_cards cs'
						       
      fun held_cards (cs, ms, hc, goal) =
	case ms of
	    [] => score_challenge(hc, goal)
	  | m::ms' => case m of
			  Discard x => let val removed = remove_card(hc, x, IllegalMove)
				       in
					   held_cards(cs, ms', removed, goal)
				       end
			| Draw => case cs of
				      [] => score_challenge(hc, goal)
				    | c::cs' => let val card_sum = smallest_sum_cards(c::hc)
						in
						    if card_sum >= goal
						    then score_challenge(c::hc, goal)
						    else held_cards(cs', ms', c::hc, goal)
						end
  in
      held_cards(cs, ms, [], goal)
  end

      
fun careful_player (cs, goal) =
  let fun find_card_by_value (cs, n) =
	if n <= 0
	then NONE
	else
	    case cs of
		[] => NONE
	      | c::cs' => if card_value c = n
			  then SOME c
			  else find_card_by_value(cs', n)

      fun helper(cs, moves, hc, goal) =
	case cs of
	    [] => moves
	  | c::cs' => let val current = sum_cards(hc)
		      in
			  if goal= current
			  then moves
			  else if goal > current + 10
			  then helper(cs', Draw::moves, c::hc, goal)
			  else (* check discard and draw to achive 0 *)
			      let val diff = current + card_value c - goal
				  val card_option = find_card_by_value(hc, diff)
			      in
				  case card_option of
				      NONE => (* my strategy *)
				      if diff <= 0
				      then helper(cs', Draw::moves, c::hc, goal)
				      else moves
				    | SOME cx => Draw::Discard cx::moves
			      end
		      end
      val ms = helper(cs, [], [], goal)
  in
      case ms of
	  [] => []
	| _ => rev ms
  end
      

val c2 = [(Hearts, Num 4), (Hearts, Ace)]
val c3 = [(Hearts, Num 4), (Hearts, Ace), (Spades, Num 1)]
val c4 = [(Hearts, Num 4), (Spades, Num 1), (Hearts, Ace)]
	     
