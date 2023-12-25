match([],[],[]).                                       % END
match([lex(L)|P],[lex(L)|F],S) :- match(P,F,S).        % ELIM
match([meta(X)|P],[node(C,F1)|F2],S2) :-               % BIND
  match(P,F2,S1), add(S1,{X,node(C,F1)},S2).
match(P,[node(_,F1)|F2],S) :-                          % UNPAR
  append(F1,F2,F), match(P,F,S).

add([],B,[B]).
add(S=[{X,T1}|_],{X,T2},S) :- !, T1 = T2.
add([B1|S1],B,[B1|S]) :- add(S1,B,S).
