

/*
Mustafa Bingül - 141044077
Programming Lang.

hocam part3 ve part4 ü yapabildim,
part1 ve part2 yarım çalışıyor.

part2 de gidebilecekleri buluyorum.
ama en ucuz olanını bulduramadım.
*/

flight(istanbul,izmir,3).
flight(istanbul,ankara,5).
flight(istanbul,trabzon,3).

flight(izmir,ankara,6).
flight(izmir,istanbul,3).
flight(izmir,antalya,1).

flight(trabzon,istanbul,3).
flight(trabzon,ankara,2).

flight(ankara,istanbul,5).
flight(ankara,izmir,6).
flight(ankara,trabzon,2).
flight(ankara,konya,8).

flight(konya,diyarbakir,1).
flight(konya,kars,5).
flight(konya,ankara,8).

flight(diyarbakir,konya,1).
flight(diyarbakir,antalya,5).

flight(antalya,diyarbakir,5).
flight(antalya,izmir,1).

flight(gaziantep,kars,3).

flight(kars,konya,5).
flight(kars,gaziantep,3).

flight(edirne,edremit,5).

flight(edremit,edirne,5).
flight(edremit,erzincan,7).

flight(erzincan,edremit,7).

/*PART 1*/

%route(X,Y,C) :- route(Y,X,C).
route(X,Y,C) :- flight(X,Y,C),flight(Y,Z,C).
route(X,Y,C) :- flight(X,O,C),flight(O,Y,C),O\==Y.



/*PART 2*/

mem(X,[X|_]).
mem(X,[_|END]):- mem(X,END).

croute(A,B,D):- path(A,B,[A],P,D),write(P).
path(A,B,P,[B|P],D):- flight(A,B,D),!.
path(A,B,LISTED,PATH,D):- flight(A,C,Tt), 
						C\==B, \+mem(C,LISTED), 
						flight(A,C,T),
						path(C,B,[C|LISTED],PATH,D2),
						%write(PATH),
						D is T + D2.


/*PART 3*/
when(a,10).
when(b,12).
when(c,11).
when(d,16).
when(e,17).

when(f,10).
	
where(a,101).
where(b,104).
where(c,102).
where(d,103).
where(e,103).

where(f,101).

enrollment(1,a).
enrollment(1,b).
enrollment(2,a).
enrollment(3,b).
enrollment(4,c).
enrollment(5,d).
enrollment(6,d).
enrollment(6,a).


/*3.1*/
schedule(S,P,T):- enrollment(S,K),where(K,P),when(K,T).

/*3.2*/
usage(P,T):- when(K,T),where(K,P).

/*3.3*/
conflict(X,Y):-not((where(X,K),where(Y,L),K\==L,
			when(X,M),when(Y,N),M\==N)).

/*3.4*/
meet(X,Y):- enrollment(X,K),where(K,L),enrollment(Y,A),where(A,B),
			K==A,L==B,!.



/*PART 4*/

/*4.1*/

member(X,[X|_]).
member(X,[_|END]):- member(X,END).

union([X|A],L1,L2) :- member(X,L1),union(A,L1,L2).
union([X|A],L1,[X|L2]):-union(A,L1,L2).
union([],X,X).
union(L,S,U):-write(U),union(L1,U,L2).

/*4.2*/

intersect([X|L1],ALL,[X|L2]):- member(X,ALL),
							intersect(L1,ALL,L2).
intersect([X|L1],ALL,L2):-intersect(L1,ALL,L2).
intersect([],ALL,[]).
intersect(L1,L2,I):-write(I),intersect(L1,I,L2).

/*4.3*/

addList([],L,L).
addList([X|A],ALL,[X|B]):-addList(A,ALL,B).

flatten([],[]):-!.
flatten([X|A],ALL):-!,flatten(X,L1),flatten(A,L2),
	addList(L1,L2,ALL).
flatten(X,[X]).
flatten(L,F):-write(F),flatten(L,F).
