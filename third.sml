exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(strings) =
    List.filter (fn string => Char.isUpper(String.sub(string, 0))) strings

fun longest_string(strings) =
    List.foldl (fn (a, b) => if String.size(a) > String.size(b) then a else b) "" strings

fun longest_string2(strings) =
    List.foldl (fn (a, b) => if String.size(b) > String.size(a) then b else a) "" strings

fun longest_string_helper f = fn strings =>
    List.foldl f "" strings

val longest_string3 = longest_string_helper (fn(a,b) => if String.size(a)>String.size(b) then a else b)

val longest_string4 = longest_string_helper (fn(a,b) => if String.size(b) > String.size(a) then b else a)

val longest_capitalized = longest_string4 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f = fn list =>
    case list of
        [] => raise NoAnswer
        | x::xs' => case f(x) of
            NONE => first_answer f xs'
            | SOME x => x

fun all_answers f = fn list =>
    let fun helper_fun(lst, acc) =
        case lst of
            [] => SOME acc
            | x::xs' => case f(x) of
                NONE => NONE
                | SOME x => helper_fun(xs', x@acc)
    in
        helper_fun(list, [])
    end

fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn x => String.size(x)) p

fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let fun helper_fun1 p' =
            case p' of
                Variable x => [x]
                | TupleP ps => List.foldl (fn (x, y) => y @ (helper_fun1 x)) [] ps
                | ConstructorP (string, pattern) => helper_fun1 pattern
                | _ => []

        fun helper_fun2 strings =
            case strings of
                [] => false
                | head::tail => if List.exists(fn x => x = head) tail then true else helper_fun2 tail
    in
        (helper_fun2 o helper_fun1) p
    end

fun match (valu, pattern) =
    case (valu, pattern) of
        (_, Wildcard) => SOME []
        | (v, Variable s) => SOME [(s, v)]
        | (Unit, UnitP) => SOME []
        | (Const i, ConstP i') => if i = i' then SOME [] else NONE
        | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                   then all_answers match (ListPair.zip(vs, ps))
                                   else NONE
        | (Constructor(s2, v), ConstructorP(s1, p)) => if s2 = s1 then match(v, p) else NONE

fun first_match (value, patterns) =
    SOME (first_answer (fn pattern => match(value, pattern)) patterns)
    handle NoAnswer => NONE