% import necessary libs
:- use_module([library(lists),library(clpfd)]).

lookup(A, [], [(A,VarA)], VarA).
% exist case 
lookup(A, [(A,VarA)|Tail], [(A,VarA)|Tail], VarA).
% if not in case 
lookup(A, [(B,VarB)|Tail1], [(B,VarB)|Tail2], VarA) :-
    \+ =(A,B),
    lookup(A,Tail1,Tail2,VarA).

%% between 

% The variable x lies between 0 and 10.
% Variable x varies from 1 to 20.
% A variable x is in the range 100 to 14.
% y is between -2 and 25.

between --> [lies],[between]. 
between --> [varies],[from]. 
between --> [is], [in] ,[the] ,[range].
between --> [between]. 
between --> [is], [between]. 
and --> [and].
and --> [to].

equals --> [is].

equals --> [equals].
equals --> [equal].
equals --> [greater],[than],[or],[equal],[to].
equals --> [contains] .
equals --> [holds].


subject(OldVars, NewVars, Var) --> variable ,[X], {lookup(X, OldVars, NewVars, Var)}.

variable -->  [the], [variable].
variable --> [a], [variable].
variable --> [variable].
variable --> [].



sentence(OldVars, NewVars) --> subject(OldVars, NewVars, Var), between, [Number1], and, [Number2], [fullstop],
{(Number1 #> Number2) #==> (Var #>= Number2 #/\ Var #=< Number1), (Number1 #=< Number2) #==> (Var #>= Number1 #/\ Var #=< Number2), integer(Number1), integer(Number2)}.
% Var (which is x) equals Result (which is A + B)
sentence(OldVars, NewVars) --> subject(OldVars, InterimVars, Var), equals, math_calculation(InterimVars, NewVars, Result),[fullstop],
{Var #= Result}.

sentence(OldVars, NewVars) --> subject(OldVars, InterimVars, Var), equals, math_calculation(InterimVars, NewVars, Result),[fullstop],
{Var #>= Result}.



math_calculation(OldVars, NewVars, VarA + VarB) --> [A], [plus], [B], {lookup(A, OldVars, InterimVars, VarA), 
lookup(B, InterimVars, NewVars, VarB)
}.
math_calculation(OldVars, NewVars, VarA * VarB) --> [the],[product],[of],[A], [and], [B], {
lookup(A, OldVars, InterimVars, VarA), 
lookup(B, InterimVars, NewVars, VarB)
}.
math_calculation(OldVars, NewVars, VarC * Number) --> [C], [times], [Number],{
lookup(C, OldVars, InterimVars, VarC)
}.

math_calculation(OldVars, NewVars, VarZ / Number) --> division,[Z], [and], [Number],{
lookup(Z, OldVars, InterimVars, VarZ)
}.

division --> [quotient],[of].
division --> [the],[dividend],[of].


lpsolve( Data, Answers ) :-
    lpsolve( [], Answers, Data, [] ).

lpsolve(Vars,Vars,[],[]).

lpsolve(OldVars,FutureVars,Data,FutureRemain) :-
     phrase(sentence( OldVars,NewVars),Data,Remain),
     lpsolve(NewVars,FutureVars,Remain,FutureRemain).

% lpsolve([the,variable,x,lies,between,0,and,10,fullstop,variable,x,varies,from,1,to,20,fullstop],Answers).
% phrase(sentence(OldVars, NewVars),[the,variable,x,lies,between,0,and,10,fullstop]).
% phrase(sentence(OldVars, NewVars),[variable,x,varies,from,1,to,20,fullstop]).
% phrase(sentence(OldVars, NewVars),[a,variable,x,is, in,the,range,100,to,14,fullstop]).


% The variable x lies between 0 and 10.
% Variable x varies from 1 to 20.
% A variable x is in the range 100 to 14.
% y is between -2 and 25.
% x equals a plus b.
% x is c times 2.
% The variable z contains the product of b and c.
% y is less than 5 + 2 * q.
% All these variables are greater than 5.
% Variable q is greater than or equal to the quotient of z and 2.
% Variable w holds the dividend of z and 2.
% It is greater than q.


