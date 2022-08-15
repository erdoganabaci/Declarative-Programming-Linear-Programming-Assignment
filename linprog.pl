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
equals --> [greater],[than],[or].
equals --> [contains] .
equals -> [holds].


subject(OldVars, NewVars, Var) --> variable ,[X], {lookup(X, OldVars, NewVars, Var)}.

variable -->  [the], [variable].
variable --> [a], [variable].
variable --> [variable].
variable --> [].

% sentences(OldVars, NewVars) --> sentence(OldVars, NewVars).
% sentences(OldVars, NewVars) --> sentence(OldVars, NewVars), sentences(OldVars, NewVars).

sentence(OldVars, NewVars) --> subject(OldVars, NewVars, Var), between, [Number1], and, [Number2], [fullstop],
{(Number1 #> Number2) #==> (Var #>= Number2 #/\ Var #=< Number1), (Number1 #=< Number2) #==> (Var #>= Number1 #/\ Var #=< Number2), integer(Number1), integer(Number2)}.
% Var (which is x) equals Result (which is A + B)
sentence(OldVars, NewVars) --> subject(OldVars, InterimVars, Var), equals, math_calculation(InterimVars, NewVars, Result),[fullstop],
{Var #= Result}.

sentence(OldVars, NewVars) --> subject(OldVars, InterimVars, Var), equals, math_calculation(InterimVars, NewVars, Result),[fullstop],
{Var #>= Result}.
% All these variables are greater than 5.
% sentence(OldVars, NewVars, Var) --> subject(OldVars, InterimVars, Var), equals, math_calculation(InterimVars, NewVars, Result),
% {Var #= Result}.



math_calculation(OldVars, NewVars, Result) --> [A], [plus], [B], {lookup(A, OldVars, InterimVars, VarA), 
lookup(B, InterimVars, NewVars, VarB),
Result is VarA + VarB
}.
math_calculation(OldVars, NewVars, Result) --> [the],[product],[of],[A], [and], [B], {
lookup(A, OldVars, InterimVars, VarA), 
lookup(B, InterimVars, NewVars, VarB),
Result is VarA * VarB
}.
math_calculation(OldVars, NewVars, Result) --> [C], [times], [Number],{
lookup(C, OldVars, InterimVars, VarC),
Result is VarC * Number
}.

math_calculation(OldVars, NewVars, Result) --> [quotient],[of],[Z], [and], [Number],{
lookup(Z, OldVars, InterimVars, VarZ),
Result is VarZ / Number
}.

% front end: lpsolve/2 succeeds when its first
% argument is a list of well formed sentences
% and its second is a list of constrained variables
% lpsolve([the,variable,x,lies,between,0,and,10,fullstop,variable,x,varies,from,1,to,20,fullstop],Answers).

lpsolve( Data, Answers ) :-
    lpsolve( [], Answers, Data, [] ).

lpsolve(Vars,Vars,[],[]).

lpsolve(OldVars,FutureVars,Data,FutureRemain) :-
     phrase(sentence( OldVars,NewVars),Data,Remain),
     lpsolve(NewVars,FutureVars,Remain,FutureRemain).

% lpsolve/4 succeeds when its first argument is a
% list of variable name/Prolog variable pairs, its
% second is the same list modified with any new
% variables introduced in the list of sentences
% in argument 3 which is parsed with the remainder
% given in argument 4


% phrase(start(OldVars, NewVars),[the,variable,x,lies,between,0,and,10]).

% phrase(sentence(OldVars, NewVars),[the,variable,x,lies,between,0,and,10]).
% Variable x varies from 1 to 20.
% phrase(start(OldVars, NewVars),[the,variable,x,lies,between,0,and,10,fullstop,variable,x,varies,from,1,to,20,fullstop]).
% phrase(start(OldVars, NewVars),[the,variable,x,lies,between,0,and,10,variable,y,varies,from,1,to,20]).
% The variable x lies between 0 and 10.

% phrase(sentence(OldVars, NewVars, Var),[the,variable,x,lies,between,0,and,10]).
%% Remain sentences 

% y is less than 5 + 2 * q.
% All these variables are greater than 5.
% Variable q is greater than or equal to the quotient of z and 2.
% Variable w holds the dividend of z and 2.
% It is greater than q.


% Here are some notes on the sentences:
% • The scope of the variable names is the entire set of input sentences { that is, once a variable
% is named, any use of its name refers to the same variable.

% • The referring expression It refers to the variable which was the subject of the previous
% sentence, unless the previous sentence began with All, in which case an exception is generated
% (see the manual for details).

% • The referring expression All these variables refers to the set of variables which have been
% named up until its occurrence in the input string. It does not refer to any variables named
% after it in the input string.
% 2

% • The noun phrase A variable x always introduces a new variable called x; using it twice with
% the same variable name should generate an exception, because this would cause a scoping
% error.

% Do not forget that mathematical expressions are not simply evaluated in left to right order { you
% have to allow for the fact that +/- and  X and  / bind their arguments differently.
% Make any assumptions which you suppose in designing your grammar clear in your comments.



% lookup/4 predicate will create