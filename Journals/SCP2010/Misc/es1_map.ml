exception Failure

module Subst = 
  Map.Make(struct type t = string let compare = compare end)

let add x v m =
  try
    if v = Subst.find x m then m else raise Failure
  with Not_found -> Subst.add x v m

let rec mat p f = match (p,f) with
  ([],[]) -> Subst.empty                             (* END     *)
| (`Lex(s1)::p,`Lex(s2)::f) when s1 = s2
    -> mat p f                                       (* ELIM    *)
| (`Meta(x)::`Lex(s1)::p,`Node(c,f1)::`Lex(s2)::f2) when s1 = s2
    -> add x (`Node(c,f1)) (mat p f2)                (* BIND_1  *)
| (`Meta(x)::p,`Node(c,f1)::(`Node(_,_)::_ as f2))
    -> add x (`Node(c,f1)) (mat p f2)                (* BIND_2  *)
| ([`Meta(x)],[`Node(c,f)]) 
    -> Subst.add x (`Node(c,f)) Subst.empty          (* BIND_3  *)
| (`Pat(p1)::p2,`Node(c,f1)::f2)
    -> Subst.fold add (mat p1 f1) (mat p2 f2)        (* UNPAR_1 *)
| (p,`Node(c,f1)::f2)
    -> mat p (f1 @ f2)                               (* UNPAR_2 *)
| _ -> raise Failure

