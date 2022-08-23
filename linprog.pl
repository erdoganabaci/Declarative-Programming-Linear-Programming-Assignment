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

subject(Vars, Vars,Var) --> [it], {append(_,[(_,Var)],Vars)}. 

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

sentence(OldVars, NewVars) --> subject(OldVars, InterimVars, Var), less, math_calculation(InterimVars, NewVars, Result),[fullstop],
{Var #< Result}.

less --> [is], [less], [than].
less --> [is], [greater], [than].

math_calculation(Vars,Vars,Number) --> [Number].

% y is less than 5 + 2 * q. This recursion will look at all possible cases if it's making cases that are incorrect according to add some additional checks inside {}
math_calculation(OldVars, NewVars, Number + Result) --> [Number] , [+], math_calculation(OldVars, NewVars, Result).
% phrase(sentence(OldVars, NewVars),[y,is,less,than,5,+,2,-,1,fullstop]).
math_calculation(OldVars, NewVars, Number - Result) --> [Number] , [-], math_calculation(OldVars, NewVars, Result).

math_calculation(OldVars, NewVars, Number - Result) --> [Number] , [/], math_calculation(OldVars, NewVars, Result).


math_calculation(OldVars, NewVars, VarA + VarB) --> [A], [plus], [B], {lookup(A, OldVars, InterimVars, VarA),
lookup(B, InterimVars, NewVars, VarB)
}.

math_calculation(OldVars, NewVars,VarB * A) --> [A], [*], [B], {lookup(B, OldVars, NewVars, VarB)
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

    
searchDomainAndWrite([]).
searchDomainAndWrite([(VarName, Var)|Rest]) :- fd_dom(Var, Dom), write(VarName), write(":"), write(Dom), searchDomainAndWrite(Rest).
% [the,variable,x,equals,3,fullstop,the,variable,q,equals,3,fullstop,z,is,less,than,10,fullstop,it,is,greater,than,5,fullstop]
analyse(List) :- lpsolve(List,Answers),searchDomainAndWrite(Answers).
% analyse([the,variable,x,equals,3,fullstop,the,variable,q,equals,3,fullstop,z,is,less,than,10,fullstop,it,is,greater,than,5,fullstop]).
% phrase(sentence(OldVars, NewVars),[the,variable,x,lies,between,0,and,10,fullstop,variable,x,varies,from,1,to,20,fullstop,it,is,greater,than,q,fullstop]).
% lpsolve([the,variable,x,lies,between,0,and,10,fullstop,variable,x,varies,from,1,to,20,fullstop],Answers).
% lpsolve([the,variable,x,equals,3,fullstop,variable,y,equals,2,fullstop,variable,z,is,less,than,10,fullstop,it,equals,x,plus,y,fullstop],Answers).
% lpsolve([x,is,greater,than,q,fullstop],Answers).
% lpsolve([the,variable,x,equals,3,fullstop,the,variable,q,equals,3,fullstop,z,is,less,than,10,fullstop,it,is,greater,than,5,fullstop],Answers).

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


