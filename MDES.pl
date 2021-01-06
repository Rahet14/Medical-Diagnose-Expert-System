
symptom(Patient,fever) :- 
verify(Patient," have a fever (y/n) ?").

symptom(Patient,eye_irritation) :- 
verify(Patient," have a eye irritation (y/n) ?").

symptom(Patient,runny_nose) :- 
verify(Patient," have a runny nose (y/n) ?").

symptom(Patient,stuffy_nose) :- 
verify(Patient," have a stuffy_nose (y/n) ?").

symptom(Patient,sneezing) :- 
verify(Patient," have a sneezing (y/n) ?").

symptom(Patient,headache) :- 
verify(Patient," have a headache (y/n) ?").

symptom(Patient,fatigue) :- 
verify(Patient," have a fatigue (y/n) ?").

symptom(Patient,cough) :- 
verify(Patient," have a cough (y/n) ?").

symptom(Patient,sore_through) :- 
verify(Patient," have a sore through (y/n) ?").

symptom(Patient,nausea) :- 
verify(Patient," have a nausea (y/n) ?").

symptom(Patient,rash) :- 
verify(Patient," have a rash (y/n) ?").

symptom(Patient,body_ache) :- 
verify(Patient," have a body_ache (y/n) ?").

symptom(Patient,blisters) :- 
verify(Patient," have a blisters (y/n) ?").

symptom(Patient,chills) :- 
verify(Patient," have a chills (y/n) ?").


ask(Patient,Question) :-
	write(Patient),write(', do you'),write(Question),
	read(N),
	( (N == yes ; N == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail).


:- dynamic yes/1,no/1.

verify(P,S) :-
   (yes(S) -> true ;
    (no(S) -> fail ;
     ask(P,S))).


diagnose(X, allergies):- symptom(X,eye_irritation),symptom(X,runny_nose),symptom(X,stuffy_nose),symptom(X,sneezing).
diagnose(X, flu):- symptom(X,fever),symptom(X,headache),symptom(X,fatigue),symptom(X,cough).
diagnose(X, cold):- symptom(X,cough),symptom(X,sneezing),symptom(X,stuffy_nose),symptom(X,sore_through).
diagnose(X, dengue):- symptom(X,nausea),symptom(X,rash),symptom(X,body_ache),symptom(X,fever).
diagnose(X, malaria):- symptom(X,headache),symptom(X,nausea),symptom(X,body_ache),symptom(X,fever),symptom(X,fatigue).
diagnose(X,chicken_pox):-symptom(X,fever),symptom(X,cough),symptom(X,blisters),symptom(X,rash).
diagnose(X,influenza):-symptom(X,fever),symptom(X,chills),symptom(X,headache),symptom(X,body_ache),symptom(X,nausea).


write_list([]).
write_list([Term| Terms]) :-
write(Term),
write_list(Terms).


check :-
write('What is the your name? '),
read(Patient),
diagnose(Patient,Disease),
write_list([Patient,', probably has ',Disease,'.']),nl.

check :-
write('Sorry, I couldn''t diagnose the disease. '),nl.






