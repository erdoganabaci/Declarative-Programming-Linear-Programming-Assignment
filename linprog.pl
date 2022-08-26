% import necessary libs
:- use_module([library(lists),library(clpfd)]).

% if we didnt find variable we added
lookup(A, [], [(A,VarA)], VarA).
% exist case
lookup(A, [(A,VarA)|Tail], [(A,VarA)|Tail], VarA).
% if not in case
lookup(A, [(B,VarB)|Tail1], [(B,VarB)|Tail2], VarA) :-
    \+ =(A,B),
    lookup(A,Tail1,Tail2,VarA).

% dcg helper to parse sentences
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
equals --> [contains] .
equals --> [holds].

greaterOrEqual --> [is],[greater],[than],[or],[equal],[to].
less --> [is], [less], [than].
greater --> [is], [greater], [than].

% we parsed subject of sentence it always it or variable
subject(Vars, Vars,Var) --> [it], {append(_,[(_,Var)],Vars)}. 
% variable names always one according to specification
subject(OldVars, NewVars, Var) --> variable ,[X], {lookup(X, OldVars, NewVars, Var),atom_length(X,Length),Length is 1}.


variable -->  [the], [variable].
variable --> [a], [variable].
variable --> [variable].
variable --> [].

division --> [the],[quotient],[of].
division --> [the],[dividend],[of].


% we parse one complete sentences with subject and constraint
sentence(OldVars, NewVars) --> subject(OldVars, NewVars, Var), between, [Number1], and, [Number2], [fullstop],
{(Number1 #> Number2) #==> (Var #>= Number2 #/\ Var #=< Number1), (Number1 #=< Number2) #==> (Var #>= Number1 #/\ Var #=< Number2), integer(Number1), integer(Number2)}.
% Var (which is x) equals Result (which is A + B)
sentence(OldVars, NewVars) --> subject(OldVars, InterimVars, Var), equals, math_calculation(InterimVars, NewVars, Result),[fullstop],
{Var #= Result}.

sentence(OldVars, NewVars) --> subject(OldVars, InterimVars, Var), greaterOrEqual, math_calculation(InterimVars, NewVars, Result),[fullstop],
{Var #>= Result}.

sentence(OldVars, NewVars) --> subject(OldVars, InterimVars, Var), less, math_calculation(InterimVars, NewVars, Result),[fullstop],
{Var #< Result}.

sentence(OldVars, NewVars) --> subject(OldVars, InterimVars, Var), greater, math_calculation(InterimVars, NewVars, Result),[fullstop],
{Var #> Result}.


% basically math operation of base case
math_calculation(Vars,Vars,Number) --> [Number].
% basically math operation resursive step
% y is less than 5 + 2 * q. This recursion will look at all possible cases if it's making cases that are incorrect according to add some additional checks inside {}
math_calculation(OldVars, NewVars, Number + Result) --> [Number] , [+], math_calculation(OldVars, NewVars, Result).
% phrase(sentence(OldVars, NewVars),[y,is,less,than,5,+,2,-,1,fullstop]).
math_calculation(OldVars, NewVars, Number - Result) --> [Number] , [-], math_calculation(OldVars, NewVars, Result).

math_calculation(OldVars, NewVars, Number - Result) --> [Number] , [/], math_calculation(OldVars, NewVars, Result).


math_calculation(OldVars, NewVars, VarA + VarB) --> [A], [plus], [B], {
lookup(A, OldVars, InterimVars, VarA),
lookup(B, InterimVars, NewVars, VarB)
}.

math_calculation(OldVars, NewVars,VarB * A) --> [A], [*], [B], {lookup(B, OldVars, NewVars, VarB)
}.

math_calculation(OldVars, NewVars, VarA * VarB) --> [the],[product],[of],[A], [and], [B], {
lookup(A, OldVars, InterimVars, VarA),
lookup(B, InterimVars, NewVars, VarB)
}.
math_calculation(OldVars, NewVars, VarC * Number) --> [C], [times], [Number],{
lookup(C, OldVars, NewVars, VarC)
}.

math_calculation(OldVars, NewVars, (VarZ // Number)) --> division,[Z], [and], [Number],{
lookup(Z, OldVars, NewVars, VarZ)
}.


% this is parses all sentences and add spesify constraint
lpsolve( Data, Answers ) :-
    lpsolve( [], Answers, Data, [] ).

lpsolve(Vars,Vars,[],[]).

lpsolve(OldVars,FutureVars,Data,FutureRemain) :-
     phrase(sentence( OldVars,NewVars),Data,Remain),
     lpsolve(NewVars,FutureVars,Remain,FutureRemain).

% well format of output and printouts following sideeffects
searchDomainAndWrite([],[]).
% analyse([the,variable,x,is,greater,than,3,fullstop],Residue).
searchDomainAndWrite([(VarName, Var)|Rest],[[VarName]-Dom|RestResidue]) :- fd_dom(Var, Dom), fd_size(Var,sup),write(VarName), write(":"),write(" "),write("this var name is infite number of solutions\n"),searchDomainAndWrite(Rest,RestResidue).
% analyse([the,variable,x,equals,3,fullstop],Residue).
searchDomainAndWrite([(VarName, Var)|Rest],[[VarName]-Dom|RestResidue]) :- fd_dom(Var, Dom), fd_size(Var,Size),Size is 1,fd_sup(Var,Value),write(VarName), write("="),write(Value),write("\n"),searchDomainAndWrite(Rest,RestResidue).
% analyse([the,variable,x,lies,between,0,and,10,fullstop],Residue).
searchDomainAndWrite([(VarName, Var)|Rest],[[VarName]-Dom|RestResidue]) :- fd_dom(Var, Dom),fd_size(Var,Size),Size \= sup,Size > 1,write(VarName), write(" in "),write(Dom),write("\n"),searchDomainAndWrite(Rest,RestResidue).
analyse(List,Residue) :- lpsolve(List,Answers),searchDomainAndWrite(Answers,Residue).
% analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is, in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop,x,is,greater,than,y,times,2,fullstop,variable,y,is,greater,than,or,equal,to,the,quotient,of,z,and,4,fullstop],Residue).
% analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is, in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop,x,is,greater,than,y,times,2,fullstop,variable,y,is,greater,than,or,equal,to,the,quotient,of,z,and,4,fullstop,w,equals,3,fullstop,v,is,less,than,2,fullstop],Residue).

% analyse([the,variable,x,is,greater,than,3,fullstop],Residue).
% first sentences test
test1(OldVars,NewVars) :- phrase(sentence(OldVars,NewVars),[the,variable,x,lies,between,0,and,10,fullstop]).
% usage
% test1(A,B).
test2(Residue) :- analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is, in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop,x,is,greater,than,y,times,2,fullstop,variable,y,is,greater,than,or,equal,to,the,quotient,of,z,and,4,fullstop],Residue).

% analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is, in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop,x,is,greater,than,y,times,2,fullstop,variable,y,is,greater,than,or,equal,to,the,quotient,of,z,and,4,fullstop]).% analyse([the,variable,x,equals,3,fullstop,the,variable,q,equals,3,fullstop,z,is,less,than,10,fullstop,it,is,greater,than,5,fullstop]).
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
% Variable y varies from 10 to -10.
% A variable z is in the range 0 to 15.
% It equals x plus y.
% All these variables are greater than -20
% y is less than 5 + 2 * x.
% x is greater than y times 2.
% Variable y is greater than or equal to the quotient of z and 4.

% [the,variable,x,lies,between,0,and,10,fullstop]
% [variable,y,varies,from,10,to,-10,fullstop]
% [a,variable,z,is, in,the,range,0,to,15,fullstop]
% [it,equals,x,plus,y,fullstop]
% [y,is,less,than,5,+,2,*,x,fullstop]
% [x,is,greater,than,y,times,2,fullstop]
% [variable,y,is,greater,than,or,equal,to,the,quotient,of,z,and,4,fullstop]

% phrase(sentence(OldVars, NewVars),[the,variable,x,lies,between,0,and,10,fullstop]).
% phrase(sentence(OldVars, NewVars),[variable,y,varies,from,10,to,-10,fullstop]).
% phrase(sentence(OldVars, NewVars),[a,variable,z,is, in,the,range,0,to,15,fullstop]).
% phrase(sentence(OldVars, NewVars),[it,equals,x,plus,y,fullstop]).
% phrase(sentence(OldVars, NewVars),[y,is,less,than,5,+,2,*,x,fullstop]).
% phrase(sentence(OldVars, NewVars),[x,is,greater,than,y,times,2,fullstop]).
% phrase(sentence(OldVars, NewVars),[variable,y,is,greater,than,or,equal,to,the,quotient,of,z,and,4,fullstop]).


% lpsolve([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is, in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop],Answers).

% return true;
% lpsolve([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is, in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop,x,is,greater,than,y,times,2,fullstop],Answers).

% lpsolve([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is, in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop,x,is,greater,than,y,times,2,fullstop,variable,y,is,greater,than,or,equal,to,the,quotient,of,z,and,4,fullstop],Answers).


% lpsolve([x,is,greater,than,y,times,2,fullstop],Answers).
% lpsolve([variable,y,is,greater,than,the,dividend,of,z,and,4,fullstop],Answer).
% variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is,in,the,range,0,to,15,fullstop,