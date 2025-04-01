(* TEST
 expect;
*)

let poly1 (id : 'a. 'a -> 'a) = id 3, id "three"
[%%expect {|
val poly1 : ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly1 (fun x -> x)
[%%expect {|
Line 1, characters 14-26:
1 | let _ = poly1 (fun x -> x)
                  ^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       "'a. 'a -> 'a"
|}];;

let _ = poly1 (fun x -> x + 1)
[%%expect {|
Line 1, characters 14-30:
1 | let _ = poly1 (fun x -> x + 1)
                  ^^^^^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       "'a. 'a -> 'a"
|}];;

let id x = x
let _ = poly1 id
[%%expect {|
val id : 'a -> 'a = <fun>
Line 2, characters 14-16:
2 | let _ = poly1 id
                  ^^
Error: The value "id" has type "'b -> 'b" but an expression was expected of type
         "'a. 'a -> 'a"
|}];;

let _ = poly1 (id (fun x -> x))
[%%expect {|
Line 1, characters 14-31:
1 | let _ = poly1 (id (fun x -> x))
                  ^^^^^^^^^^^^^^^^^
Error: This expression has type "'b -> 'b"
       but an expression was expected of type "'a. 'a -> 'a"
Hint: This function application is partial, maybe some arguments are missing.
|}];;

let _ = poly1 (let r = ref None in fun x -> r := Some x; x)
[%%expect {|
Line 1, characters 35-58:
1 | let _ = poly1 (let r = ref None in fun x -> r := Some x; x)
                                       ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       "'a. 'a -> 'a"
|}];;

let escape f = poly1 (fun x -> f x; x)
[%%expect {|
Line 1, characters 21-38:
1 | let escape f = poly1 (fun x -> f x; x)
                         ^^^^^^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       "'a. 'a -> 'a"
|}];;

let poly2 : ('a. 'a -> 'a) -> int * string =
  fun id -> id 3, id "three"
[%%expect {|
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;

let _ = poly2 (fun x -> x)
[%%expect {|
Line 1, characters 8-13:
1 | let _ = poly2 (fun x -> x)
            ^^^^^
Error: Unbound value "poly2"
Hint:   Did you mean "poly1"?
|}];;

let _ = poly2 (fun x -> x + 1)
[%%expect {|
Line 1, characters 8-13:
1 | let _ = poly2 (fun x -> x + 1)
            ^^^^^
Error: Unbound value "poly2"
Hint:   Did you mean "poly1"?
|}];;

let poly3 : 'b. ('a. 'a -> 'a) -> 'b -> 'b * 'b option =
  fun id x -> id x, id (Some x)
[%%expect {|
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;

let _ = poly3 (fun x -> x) 8
[%%expect {|
Line 1, characters 8-13:
1 | let _ = poly3 (fun x -> x) 8
            ^^^^^
Error: Unbound value "poly3"
Hint:   Did you mean "poly1"?
|}];;

let _ = poly3 (fun x -> x + 1) 8
[%%expect {|
Line 1, characters 8-13:
1 | let _ = poly3 (fun x -> x + 1) 8
            ^^^^^
Error: Unbound value "poly3"
Hint:   Did you mean "poly1"?
|}];;

let rec poly4 p (id : 'a. 'a -> 'a) =
  if p then poly4 false id else id 4, id "four"
[%%expect {|
Line 2, characters 24-26:
2 |   if p then poly4 false id else id 4, id "four"
                            ^^
Error: The value "id" has type "'b -> 'b" but an expression was expected of type
         "'a. 'a -> 'a"
|}];;

let _ = poly4 true (fun x -> x)
[%%expect {|
Line 1, characters 8-13:
1 | let _ = poly4 true (fun x -> x)
            ^^^^^
Error: Unbound value "poly4"
Hint:   Did you mean "poly1"?
|}];;

let _ = poly4 true (fun x -> x + 1)
[%%expect {|
Line 1, characters 8-13:
1 | let _ = poly4 true (fun x -> x + 1)
            ^^^^^
Error: Unbound value "poly4"
Hint:   Did you mean "poly1"?
|}];;

let rec poly5 : bool -> ('a. 'a -> 'a) -> int * string =
  fun p id ->
    if p then poly5 false id else id 5, id "five"
[%%expect {|
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;

let _ = poly5 true (fun x -> x)
[%%expect {|
Line 1, characters 8-13:
1 | let _ = poly5 true (fun x -> x)
            ^^^^^
Error: Unbound value "poly5"
Hint:   Did you mean "poly1"?
|}];;

let _ = poly5 true (fun x -> x + 1)
[%%expect {|
Line 1, characters 8-13:
1 | let _ = poly5 true (fun x -> x + 1)
            ^^^^^
Error: Unbound value "poly5"
Hint:   Did you mean "poly1"?
|}];;


let rec poly6 : 'b. bool -> ('a. 'a -> 'a) -> 'b -> 'b * 'b option =
  fun p id x ->
    if p then poly6 false id x else id x, id (Some x)
[%%expect {|
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;

let _ = poly6 true (fun x -> x) 8
[%%expect {|
Line 1, characters 8-13:
1 | let _ = poly6 true (fun x -> x) 8
            ^^^^^
Error: Unbound value "poly6"
Hint:   Did you mean "poly1"?
|}];;

let _ = poly6 true (fun x -> x + 1) 8
[%%expect {|
Line 1, characters 8-13:
1 | let _ = poly6 true (fun x -> x + 1) 8
            ^^^^^
Error: Unbound value "poly6"
Hint:   Did you mean "poly1"?
|}];;

let needs_magic (magic : 'a 'b. 'a -> 'b) = (magic 5 : string)
let _ = needs_magic (fun x -> x)
[%%expect {|
val needs_magic : ('a 'b. 'a -> 'b) -> string = <fun>
Line 2, characters 20-32:
2 | let _ = needs_magic (fun x -> x)
                        ^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       "'a 'b. 'a -> 'b"
|}];;

let with_id (f : ('a. 'a -> 'a) -> 'b) = f (fun x -> x)
[%%expect {|
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;

let _ = with_id (fun id -> id 4, id "four")
[%%expect {|
Line 1, characters 8-15:
1 | let _ = with_id (fun id -> id 4, id "four")
            ^^^^^^^
Error: Unbound value "with_id"
|}];;

let non_principal1 p f =
  if p then with_id f
  else f (fun x -> x)
[%%expect {|
Line 2, characters 12-19:
2 |   if p then with_id f
                ^^^^^^^
Error: Unbound value "with_id"
|}];;

let non_principal2 p f =
  if p then f (fun x -> x)
  else with_id f
[%%expect {|
Line 3, characters 7-14:
3 |   else with_id f
           ^^^^^^^
Error: Unbound value "with_id"
|}];;

let principal1 p (f : ('a. 'a -> 'a) -> 'b) =
  if p then f (fun x -> x)
  else with_id f
[%%expect {|
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;

let principal2 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b =
  fun p f ->
    if p then f (fun x -> x)
    else with_id f
[%%expect {|
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;

type poly = ('a. 'a -> 'a) -> int * string

let principal3 : poly option list = [ None; Some (fun x -> x 5, x "hello") ]
[%%expect {|
type poly = ('a. 'a -> 'a) -> int * string
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;

let non_principal3 =
  [ (Some (fun x -> x 5, x "hello") : poly option);
    Some (fun y -> y 6, y "goodbye") ]
[%%expect {|
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;

let non_principal4 =
  [ Some (fun y -> y 6, y "goodbye");
    (Some (fun x -> x 5, x "hello") : poly option) ]
[%%expect {|
Line 2, characters 26-35:
2 |   [ Some (fun y -> y 6, y "goodbye");
                              ^^^^^^^^^
Error: This constant has type "string" but an expression was expected of type
         "int"
|}];;

(* Functions with polymorphic parameters are separate from other functions *)
type 'a arg = 'b
  constraint 'a = 'b -> 'c
type really_poly = (('a. 'a -> 'a) -> string) arg
[%%expect {|
type 'a arg = 'b constraint 'a = 'b -> 'c
Line 3, characters 20-44:
3 | type really_poly = (('a. 'a -> 'a) -> string) arg
                        ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type "('a. 'a -> 'a) -> string" should be an instance of type
         "'b -> 'c"
       The universal variable "'a" would escape its scope
|}];;

(* Polymorphic parameters are (mostly) treated as invariant *)
type p1 = ('a. 'a -> 'a) -> int
type p2 = ('a 'b. 'a -> 'b) -> int
[%%expect {|
type p1 = ('a. 'a -> 'a) -> int
type p2 = ('a 'b. 'a -> 'b) -> int
|}];;

let foo (f : p1) : p2 = f
[%%expect {|
Line 1, characters 24-25:
1 | let foo (f : p1) : p2 = f
                            ^
Error: The value "f" has type "p1" = "('a. 'a -> 'a) -> int"
       but an expression was expected of type "p2" = "('a 'b. 'a -> 'b) -> int"
       Type "'a" is not compatible with type "'b"
|}];;

let foo f = (f : p1 :> p2)
[%%expect {|
Line 1, characters 12-26:
1 | let foo f = (f : p1 :> p2)
                ^^^^^^^^^^^^^^
Error: Type "p1" = "('a. 'a -> 'a) -> int" is not a subtype of
         "p2" = "('a 'b. 'a -> 'b) -> int"
       Type "'b" is not a subtype of "'a"
|}];;

module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
[%%expect {|
Line 1, characters 59-60:
1 | module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
                                                               ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : p1 end
       is not included in
         sig val f : p2 end
       Values do not match: val f : p1 is not included in val f : p2
       The type "p1" = "('a. 'a -> 'a) -> int" is not compatible with the type
         "p2" = "('a 'b. 'a -> 'b) -> int"
       Type "'a" is not compatible with type "'b"
|}];;

let foo (f : p1) : p2 = (fun id -> f id)
[%%expect {|
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;

(* Following the existing behaviour for polymorphic methods, you can
   subtype from a polymorphic parameter to a monomorphic
   parameter. Elsewhere it still behaves as invariant. *)
type p1 = (bool -> bool) -> int
type p2 = ('a. 'a -> 'a) -> int

let foo (x : p1) : p2 = x
[%%expect {|
type p1 = (bool -> bool) -> int
type p2 = ('a. 'a -> 'a) -> int
Line 4, characters 24-25:
4 | let foo (x : p1) : p2 = x
                            ^
Error: The value "x" has type "p1" = "(bool -> bool) -> int"
       but an expression was expected of type "p2" = "('a. 'a -> 'a) -> int"
       Type "bool" is not compatible with type "'a"
|}];;

let foo x = (x : p1 :> p2)
[%%expect {|
val foo : p1 -> p2 = <fun>
|}];;

module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
[%%expect {|
Line 1, characters 59-60:
1 | module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
                                                               ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : p1 end
       is not included in
         sig val f : p2 end
       Values do not match: val f : p1 is not included in val f : p2
       The type "p1" = "(bool -> bool) -> int" is not compatible with the type
         "p2" = "('a. 'a -> 'a) -> int"
       Type "bool" is not compatible with type "'a"
|}];;

let foo (f : p1) : p2 = (fun id -> f id)
[%%expect {|
Uncaught exception: File "typing/btype.ml", line 798, characters 9-15: Assertion failed

|}];;
