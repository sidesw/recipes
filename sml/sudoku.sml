val a ="c:\\documents and settings\\Administrator\\My documents\\sid\\sud.sml";

open List;

exception invalid;

datatype sudoku = E 
                | N of int
                | P of int list
                ;

val row = fn n => fn l => nth(l,n);
val col = fn n => fn l => (map (row n) l);
val value = fn i => fn j => fn l => (row j (row i l));
val box = let 
                val rows = fn n => fn i => fn l => take(drop(l,i*n),n)
          in
                fn n => fn i => fn j => fn (l:sudoku list list)=> (map (rows n j) (rows n i l))
          end;

val o2n = [1,2,3,4,5,6,7,8,9];

val sudtoString = map (map (fn E => "_" | P l => "_" | N n => Int.toString(n)));

val sudtoList = fn E => []
                 | (N n) => [n]
                 | (P l) => l
                 ;

fun belongs x [] = false
   |belongs x (y::ys) = if(x=y) then true else (belongs x ys)
   ;

fun union([],l) = l
   |union(l,[]) = l
   |union(x::xs,l) = if (belongs x l) then union(xs,l) else x::(union(xs,l))
   ;

fun sub([],l) = []
   |sub(l,[]) = l
   |sub(x::xs,l) = if (belongs x l) then sub(xs,l) else x::(sub(xs,l))
   ;

fun intersect([],l) = []
   |intersect(l,[]) = []
   |intersect(x::xs,l) = if(belongs x l) then x::(intersect(xs,l)) 
                         else intersect(xs,l)
   ;

fun empty [] = []
   |empty ((N n)::l) = (N n)::(empty l)
   |empty (_::l) = empty l
   ;

fun replace 0 0 y ((x::xs)::l) = (y::xs)::l
   |replace 0 j y ((x::xs)::l) = let
                                       val (b::bs) = replace 0 (j-1) y (xs::l)
                                 in
                                       (x::b)::bs
                                 end
   |replace i j y (l::ls) = l::(replace (i-1) j y ls)
   ;

val sudsearch = fn i => fn j => fn l => 
if((fn (N _) => true | _ => false) (value i j l)) then l else
let
        val r = empty(row i l)
        val c = empty(col j l)
        val b = foldr union [] (map empty (box 3 (i div 3) (j div 3) l))
        val x = foldr List.@ [] (map sudtoList (union(union(r,c),b)))
        val v = sudtoList(value i j l)
        val v1 = sub(v,x)
        val v2 = if (length(v1)=1) then N(hd v1) else P(v1)
in
        replace i j v2 l
end;

val condP = fn P s => s | _ => [];

val sudsearchbox = fn col => fn b => fn k => 
let
        val c = foldr union [] (map condP (col k b))
in
        if(null c) then []
        else
        let
                val c1 = sub(o2n,foldr union [] (map condP (col ((k+1)mod 3) b)))
                val c2 = sub(o2n,foldr union [] (map condP (col ((k+2)mod 3) b)))
                val bp = foldr List.@ [] (map sudtoList (foldr List.@ [] (map empty b)))
        in
                intersect(c,sub(intersect(c1,c2),bp))
        end
end;

val fill = map (map (fn E => (P o2n) | x => x));
val n = 9;

local
fun help i j l = if (i=n) then l else if(j=n) then help (i+1) 0 l 
                                      else help i (j+1) (sudsearch i j l)
in
val apply = help 0 0
end;


local
fun colrephelp [] bi j n s = s
   |colrephelp (y::ys) bi j n s = if(n >= bi*3 andalso n <= bi*3+2) then colrephelp ys bi j (n+1) s 
                                  else colrephelp ys bi j (n+1) (replace n j y s)
fun rowrephelp [] bj i n s = s
   |rowrephelp (y::ys) bj i n s = if(n >= bj*3 andalso n <= bj*3+2) then rowrephelp ys bj i (n+1) s 
                                  else rowrephelp ys bj i (n+1) (replace i n y s)
val subhelp = fn a => fn (P b) => let 
                                        val ans = sub(b,a) 
                                        in 
                                                if((length ans)=1) then N(hd ans) else P(ans)
                                        end
                        | b => b
fun unionex 0 l = foldr union [] (tl l)
   |unionex n (x::xs) = union(x,unionex (n-1) xs)

fun boxrephelp rc i j v c 0 l = l
   |boxrephelp rc i j v c n l = if(null (nth(c,n-1))) then boxrephelp rc (i-1) j v c (n-1) l
                                     else 
                                     let
                                        val u = unionex (n-1) c
                                        val d = intersect(v,sub(nth(c,n-1),u))
                                     in 
                                        if(null d) then boxrephelp rc (i-1) j v c (n-1) l
                                        else boxrephelp rc (i-1) j v c (n-1) (replace (if rc then i else j) (if rc then j else i) (N(hd d)) l)
                                     end

fun help i j n l = if(i=3) then l else if(j=3) then help (i+1) 0 0 l else if (n=3) then help i (j+1) 0 l
                                     else let
                                                val b1 = box 3 i j l
                                                val v1 = sudsearchbox col b1 n
                                                val l1 = if (null v1) then l
                                                         else colrephelp (map (subhelp v1) (col (j*3+n) l)) i (j*3+n) 0 l
                                                val l11 = if (null v1) then l1
                                                          else boxrephelp true (i*3+2) (j*3+n) v1 (map condP (col n b1)) 3 l1

                                                val b2 = box 3 i j l11
                                                val v2 = sudsearchbox row b2 n
                                                val l2 = if (null v2) then l11
                                                         else rowrephelp (map (subhelp v2) (row (i*3+n) l11)) j (i*3+n) 0 l11
                                                val l22 = if (null v2) then l2
                                                          else boxrephelp false (j*3+2) (i*3+n) v2 (map condP (row n b2)) 3 l2
                                          in
                                                help i j (n+1) l22
                                          end

                                                
in
val applybox = help 0 0 0
end;

val puzzle1 = [
                [N 5,N 9,E,N 3,E,N 1,N 2,N 7,E],
                [E,E,E,E,N 6,N 7,E,N 5,E],
                [E,E,E,N 5,E,E,N 1,N 4,E],
                [E,N 7,E,E,N 2,N 6,E,E,N 5],
                [N 2,E,E,E,N 9,E,E,E,N 4],
                [N 8,E,E,N 4,N 3,E,E,N 1,E],
                [E,N 3,N 7,E,E,N 2,E,E,E],
                [E,N 4,E,N 6,N 1,E,E,E,E],
                [E,N 5,N 2,N 8,E,N 4,E,N 6,N 3]
             ];
val puzzle2 = [
                [E,N 2,E,N 7,E,E,E,E,N 1],
                [E,E,E,N 5,E,E,N 3,E,E],
                [N 9,E,E,E,E,N 8,E,E,N 4],
                [E,N 7,N 4,E,E,E,E,N 6,E],
                [E,E,N 6,E,N 3,E,N 7,E,E],
                [E,N 5,E,E,E,E,N 8,N 1,E],
                [N 4,E,E,N 6,E,E,E,E,N 7],
                [E,E,N 1,E,E,N 4,E,E,E],
                [N 2,E,E,E,E,N 5,E,N 3,E]
             ];


(*
val sudsearchboxcol = fn i => fn j => fn k => fn l => 
let
        val b = box 3 i j l
        val c = empty(col k b)
        val c1 = foldr List.@ [] (map sudtoList (empty(col (j*3+((k+1)mod 3)) l)))
        val c2 = foldr List.@ [] (map sudtoList (empty(col (j*3+((k+2)mod 3)) l)))
        val x = foldr union [] (map (fn E => [] | N _ => [] | P l => l) c)
        val bp = foldr List.@ [] (map sudtoList (foldr union [] (map empty b)))
        val v = sub(intersect(x,intersect(c1,c2)),bp)
        fun rephelp [] n s = s
           |rephelp (y::ys) n s = if(n >= i*3 orelse n <= i*3+2) then rephelp ys (n+1) s 
                                  else rephelp ys (n+1) (replace n (j*3+k) y s)
        val subhelp = fn a => fn (P b) => let 
                                                val ans = sub(b,a) 
                                          in 
                                                if((length ans)=1) then N(hd ans) else P(ans)
                                          end
                                | b => b
in
        rephelp (map (subhelp v) (col (j*3+k) l)) 0 l
end;
*)
(*val sudsearchboxrow = fn i => fn j => fn k => fn l => 
let
        val b = box 3 i j l
        val f = fn E => [] | N _ => [] | P l => l
        val c = foldr union [] (map f (row k b))
        val c1 = sub(o2n,foldr union [] (map f (row ((k+1)mod 3) b)))
        val c2 = sub(o2n,foldr union [] (map f (row ((k+2)mod 3) b)))
        val bp = foldr List.@ [] (map sudtoList (foldr union [] (map empty b))) 
        val v = intersect(c,sub(intersect(c1,c2),bp))
        fun rephelp [] n s = s
           |rephelp (y::ys) n s = if(n >= j*3 orelse n <= j*3+2) then rephelp ys (n+1) s 
                                  else rephelp ys (n+1) (replace (i*3+k) n y s)
        val subhelp = fn a => fn (P b) => let 
                                                val ans = sub(b,a) 
                                          in 
                                                if((length ans)=1) then N(hd ans) else P(ans)
                                          end
                                | b => b
in
        if (null v) then l else rephelp (map (subhelp v) (row (i*3+k) l)) 0 l
end;*)
val newpuzz = fill puzzle2;

