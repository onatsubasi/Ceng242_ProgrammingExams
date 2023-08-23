
:- module(main, [is_vote_wasted/2, is_candidate_elected/2, candidate_count_from_city/3, all_parties/1, all_candidates_from_party/2, all_elected_from_party/2, election_rate/2, council_percentage/2, alternative_debate_setups/2]).
:- [kb].

% DO NOT CHANGE THE UPPER CONTENT, WRITE YOUR CODE AFTER THIS LINE
is_vote_wasted(City , PoliticalParty) :- not(elected(City, PoliticalParty, N)).
is_candidate_elected(Name, PoliticalParty) :- candidate(Name, PoliticalParty, City, Row), elected(City, PoliticalParty, Erc), Row =< Erc.

candidate_count_from_city( [] , GivenCity ,0 ).
candidate_count_from_city( [Candidate | Rest] , GivenCity, Count) :- candidate(Candidate, PolticalParty, GivenCity, Row),
                                                                     candidate_count_from_city(Rest, GivenCity, RestCount), Count is RestCount + 1, !. 

candidate_count_from_city([Candidate| Rest], GivenCity, Count) :- candidate_count_from_city(Rest, GivenCity, Count).



all_parties(ListOfPoliticalParties) :- findall(PoliticalParty, party(PoliticalParty, _), ListOfPoliticalParties).

all_candidates_from_party(PoliticalParty , ListOfCandidates) :- findall(Name, candidate(Name, PoliticalParty,_,_), ListOfCandidates) .

all_elected_from_party( PoliticalParty , ListOfCandidates ) :- findall(Name, is_candidate_elected(Name, PoliticalParty), ListOfCandidates).


election_rate(PoliticalParty, Percentage) :-
    all_elected_from_party(PoliticalParty, ElectedList),
    all_candidates_from_party(PoliticalParty, CandidateList),
    length(ElectedList, ElectedCount),
    length(CandidateList, CandidateCount),
    Percentage is (ElectedCount / CandidateCount).

all_count(List) :- findall(Count, elected(_,_,Count), List) .
sum([], 0).
sum([Head|Tail], Sum) :-
   sum(Tail, Rest),
   Sum is Head + Rest.

party_count(PoliticalParty, List) :- findall(Count, elected(_,PoliticalParty,Count), List).

council_percentage( PoliticalParty , Percentage ) :-
    party_count(PoliticalParty, ElectedList),
    sum(ElectedList, Sum1),
    to_elect(X),
    Percentage is (Sum1/X) .



findParty(PartyInitial, PoliticalParty) :-
    party(PoliticalParty, PartyInitial).


doIt([], Result, Result).
doIt([Item | Rest],Acc, Result) :-
    candidate(Name,Item,_,_),
    not(member(Name, Acc)),
    append(Acc, [Name], UpdatedAcc),
    doIt(Rest, UpdatedAcc, Result).


alternative_debate_setups(DescriptionString, OrderedListOfCandidates) :-
    atom_chars(DescriptionString, PartyInitials),
    maplist(findParty, PartyInitials, Desired),
    doIt(Desired, [], OrderedListOfCandidates).
    
    






    
    
    