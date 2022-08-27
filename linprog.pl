% Erdogan ABACI 0584957

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

% well format of output and print outs following side-effects
searchDomainAndWrite([],[]).
% if domain is not finite,print out that there may be an in nite number of solutions
searchDomainAndWrite([(VarName, Var)|Rest],[[VarName]-Dom|RestResidue]) :- fd_dom(Var, Dom), fd_size(Var,sup),write(VarName), write(":"),write(" "),write("this var name is infite number of solutions\n"),searchDomainAndWrite(Rest,RestResidue).
% if there is a unique solution, print out form a = N
searchDomainAndWrite([(VarName, Var)|Rest],[[VarName]-Dom|RestResidue]) :- fd_dom(Var, Dom), fd_size(Var,Size),Size is 1,fd_sup(Var,Value),write(VarName), write("="),write(Value),write("\n"),searchDomainAndWrite(Rest,RestResidue).
% print out succeed case
searchDomainAndWrite([(VarName, Var)|Rest],[[VarName]-Dom|RestResidue]) :- fd_dom(Var, Dom),fd_size(Var,Size),Size \= sup,Size > 1,write(VarName), write(" in "),write(Dom),write("\n"),searchDomainAndWrite(Rest,RestResidue).
analyse(List,Residue) :- lpsolve(List,Answers),searchDomainAndWrite(Answers,Residue).

% Tested following sentences

% The variable x lies between 0 and 10.
% Variable y varies from 10 to -10.
% A variable z is in the range 0 to 15.
% It equals x plus y.
% y is less than 5 + 2 * x.
% x is greater than y times 2.
% Variable y is greater than or equal to the quotient of z and 4.

% due to the limitation of phrase we can only test 1 sentence at the same time.
test(OldVars,NewVars) :- phrase(sentence(OldVars,NewVars),[the,variable,x,lies,between,0,and,10,fullstop]).
test1(OldVars,NewVars) :- phrase(sentence(OldVars,NewVars),[variable,y,varies,from,10,to,-10,fullstop]).
test2(OldVars,NewVars) :- phrase(sentence(OldVars,NewVars),[a,variable,z,is,in,the,range,0,to,15,fullstop]).
test3(OldVars,NewVars) :- phrase(sentence(OldVars,NewVars),[it,equals,x,plus,y,fullstop]).
test4(OldVars,NewVars) :- phrase(sentence(OldVars,NewVars),[y,is,less,than,5,+,2,*,x,fullstop]).
test5(OldVars,NewVars) :- phrase(sentence(OldVars,NewVars),[x,is,greater,than,y,times,2,fullstop]).
test6(OldVars,NewVars) :- phrase(sentence(OldVars,NewVars),[variable,y,is,greater,than,or,equal,to,the,quotient,of,z,and,4,fullstop]).

% cumulative adding test
testSentence(Residue) :- analyse([the,variable,x,lies,between,0,and,10,fullstop],Residue).
testSentence1(Residue) :- analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop],Residue).
testSentence2(Residue) :- analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is,in,the,range,0,to,15,fullstop],Residue).
testSentence3(Residue) :- analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is,in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop],Residue).
testSentence4(Residue) :- analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is,in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop],Residue).
testSentence5(Residue) :- analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is,in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop,x,is,greater,than,y,times,2,fullstop],Residue).
testSentence6(Residue) :- analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is,in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop,x,is,greater,than,y,times,2,fullstop,variable,y,is,greater,than,or,equal,to,the,quotient,of,z,and,4,fullstop],Residue).

% side effects test
% infinite doamin test
testFailCase(Residue) :-  analyse([the,variable,x,is,greater,than,3,fullstop],Residue).
% unique solution test
testFailCase1(Residue) :- analyse([the,variable,x,equals,3,fullstop],Residue).
% succeed test
testSuccess(Residue) :-  analyse([the,variable,x,lies,between,0,and,10,fullstop],Residue).

% all sentences test
testAllSentences(Residue) :- analyse([the,variable,x,lies,between,0,and,10,fullstop,variable,y,varies,from,10,to,-10,fullstop,a,variable,z,is,in,the,range,0,to,15,fullstop,it,equals,x,plus,y,fullstop,y,is,less,than,5,+,2,*,x,fullstop,x,is,greater,than,y,times,2,fullstop,variable,y,is,greater,than,or,equal,to,the,quotient,of,z,and,4,fullstop],Residue).

