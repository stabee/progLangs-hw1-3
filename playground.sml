fun fold (f, acc, xs) =
    case xs of
        [] => acc
        | x::xs' => fold(f, f(acc, x), xs')

fun f1 xs = fold((fn(x,y) => x + y), 0, xs)

fun has_duplicate strings =
    case strings of
        [] => false
        | head::tail => if List.exists(fn x => x = head) tail then true else has_duplicate tail