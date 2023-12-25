exception Failure

let rec add s b = match (s,b) with
  ([],b) -> [b]
| ((x,v)::s1 as s,(y,w)) when x = y -> 
    if v = w then s else raise Failure
| (b1::s1,b) -> b1 :: add s1 b

let rec mat p f = match (p,f) with
  ([],[]) -> []                                   (* END    *)
| (`Lex(l1)::p,`Lex(l2)::f) when l1 = l2
    -> mat p f                                    (* ELIM   *)
| (`Meta(x,None)::`Lex(l1)::p,`Node(c,f1)::`Lex(l2)::f2)
    when l1 = l2
    -> add (mat p f2) (x,`Node(c,f1))             (* BIND1  *)
| (`Meta(x,Some c1)::`Lex(l1)::p,`Node(c2,f1)::`Lex(l2)::f2)
    when l1 = l2 && c1 = c2
    -> add (mat p f2) (x,`Node(c2,f1))       (* BIND1 typed *)
| (`Meta(x,None)::p,`Node(c,f1)::(`Node(_,_)::_ as f2))
    -> add (mat p f2) (x,`Node(c,f1))             (* BIND2  *)
| (`Meta(x,Some c)::p,`Node(c1,f1)::(`Node(_,_)::_ as f2))
    when c = c1
    -> add (mat p f2) (x,`Node(c1,f1))       (* BIND2 typed *)
| ([`Meta(x,None)],[`Node(c,f)])
    -> [(x,`Node(c,f))]                           (* BIND3  *)
| ([`Meta(x,Some c1)],[`Node(c,f)]) when c1 = c
    -> [(x,`Node(c,f))]                      (* BIND3 typed *)
| (`Pat(p1)::p2,`Node(c,f1)::f2)
    -> List.fold_left add (mat p1 f1) (mat p2 f2) (* UNPAR1 *)
| (p,`Node(c,f1)::f2)
    -> mat p (f1 @ f2)                            (* UNPAR2 *)
| _ -> raise Failure
