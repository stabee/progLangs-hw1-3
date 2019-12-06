(* DG, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, str_list) =
    let fun helper_function (xs) =
        case xs of
            [] => []
            | x::xs' => if same_string(x, str)
                        then helper_function(xs')
                        else x::helper_function(xs')
        val final_list = helper_function(str_list)
    in if (final_list = str_list) then NONE else SOME final_list end

fun get_substitutions1(list, s) =
    case list of
         [] => []
         | xss::xss' => case all_except_option(s, xss) of
                            NONE => get_substitutions1(xss', s)
                            | SOME x => x@get_substitutions1(xss',s)

fun get_substitutions2(list, s) =
    let fun helper_fun(xs, acc) =
        case xs of
            [] => acc
            | xss::xss' => case all_except_option(s, xss) of
                                NONE => helper_fun(xss', acc)
                                | SOME z => helper_fun(xss', acc@z)

    in helper_fun(list, []) end

fun similar_names(list, {first=first, middle=middle, last=last}) = 
    let fun helper_fun(list2) =
        case list2 of
            [] => [{first=first, middle=middle, last=last}]
            | x::xs' => {first=x, middle=middle, last=last}::helper_fun(xs')
    in
        helper_fun(get_substitutions1(list, first))
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

fun card_color(suit, rank) =
    case suit of
        Clubs => "black"
        | Diamonds => "red"
        | Hearts => "red"
        | Spades => "black"

fun card_value(suit, rank) =
    case rank of
        Jack => 10
        | Queen => 10
        | King => 10
        | Ace => 11
        | Num i => i

fun remove_card(cs, c, e) =
    case cs of
        [] => raise e
        | head::tail => if head = c then tail else head::remove_card(tail, c, e)

fun all_same_color(cards) =
    case cards of
        [] => true
        | x::xs' => case xs' of
            [] => true
            | y::ys' => if card_color(x) = card_color(y) then all_same_color(xs') else false

fun sum_cards(cards) =
    let fun helper_fun(cards_list, acc) =
        case cards_list of
            [] => acc
            | head::tail => helper_fun(tail, card_value(head) + acc)
    in
        helper_fun(cards, 0)
    end

fun score(cards, goal) =
    let val sum = sum_cards(cards)
        val pre_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
        if all_same_color(cards) then pre_score div 2 else pre_score
    end

fun officiate(cards, moves, goal) =
    let fun helper_fun(held_cards, deck) =
        case moves of
        [] => score(held_cards, goal)
        | move::moves' =>
            case move of
                Discard card => helper_fun(remove_card(held_cards, card, IllegalMove), deck)
                | Draw =>
                    case deck of
                    [] => score(cards, goal)
                    | head::tail => if card_value(head) + sum_cards(held_cards) > goal then score(head::held_cards,goal) else helper_fun(head::held_cards, tail)                        
    in
        helper_fun([], cards)
    end

fun pass_or_fail({grade = grade, id = id}) =
    case grade of
        NONE => raise IllegalMove
        | SOME i => if i > 75 then "pass" else "fail"

fun has_passed({grade = grade, id=id}) =
    case grade of
        NONE => raise IllegalMove
        | SOME i => if pass_or_fail({grade=grade, id=id}) = "pass" then true else false