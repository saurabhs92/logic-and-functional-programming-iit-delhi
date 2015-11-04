/* Prolog Program which creates a database of characters from Mahabharata
in the form male(X), female(X), married(X,Y), child(X,Y)
It then defines other relationships like son, anscestor, grandchildren, etc, from these */

/*male(X) if X is a male*/
male(shantanu).
male(bhishma).
male(chitrangada).
male(vichitravira).
male(shalva).
male(drupada).
male(shikhandi).
male(vyasa).  
male(dhritrashtra).
male(pandu).
male(vidur).
male(krishna).
male(shakuni).
male(dharma).
male(vayu).
male(indra).
male(yudishthira).
male(bhima).
male(arjun).
male(nakul).
male(sahdev).
male(karna).
male(duryodhan).
male(dushasan).
male(vikarna).
male(sukarna).
male(jayadratha).
male(balram).
male(sudama).
male(nanda).
male(drona).
male(kripa).
male(ashwatthama).
male(drishtadyumna).
male(abhimanyu).
male(parikshit).
male(janamejaya).
male(parashara).
male(surya).
male(ghatotkacha).
male(hidimba).
male(adiratha).
male(prativindhya).
male(shatanika).
male(sutasoma).
male(shrutsena).
male(shrutkarma).
male(vasudeva).
male(jarasanta).
male(kansa).
male(jarasanta).
male(adhirata).
male(laxmankumara).
male(subala).
male(ugrasena).

/*female(X) if X is a female*/
female(ganga).
female(satyavati).
female(amba).
female(ambika).
female(ambalika).
female(aunti).
female(madri).
female(gandhari).
female(draupadi).
female(rukmini).
female(yashoda).
female(dushala).
female(subhadra).
female(kripi).
female(uttara).
female(madravati).
female(hidimbi).
female(bhanumati).
female(devaki).
female(rohini).
female(radha).
female(lakshmana).
female(sudarma).
female(padmavati).
female(parishrami).

/*married(X,Y) if X(male) is married to Y(female)*/
married(shantanu, ganga).
married(shantanu, satyavati).
married(parashara, satyavati).
married(dhritrashtra, gandhari).
married(pandu, kunti).
married(pandu, madri).
married(vasudeva,devaki).
married(vasudeva,rohini).
married(nanda, yashoda).
married(krishna,rukmini).
married(vichitravira, ambika).
married(vichitravira, ambalika).
married(drona, kripi).
married(yudishthira, draupadi).
married(bhima, draupadi).
married(bhima, hidimbi).
married(arjun, draupadi).
married(nakul, draupadi).
married(sahdev, draupadi).
married(arjun, subhadra).
married(duryodhan, bhanumati).
married(jayadratha, dushala).
married(abhimanyu, uttara).
married(parikshit, madravati).
married(adiratha, radha).
married(subala, sudarma).
married(ugrasena, padmavati).

/*child(X,Y) if X is the child of Y*/
child(bhishma,shantanu).
child(bhishma,ganga).
child(vyasa, satyavati).
child(vyasa, parashara).
child(chitrangada, shantanu).
child(chitrangada, satyavati).
child(vichitravira, shantanu).
child(vichitravira, satyavati).
child(dhritrashtra, ambika).
child(dhritrashtra, vyasa).
child(pandu, ambalika).
child(pandu, vyasa).
child(gandhari, subala).
child(gandhari, sudarma).
child(shakuni, subala).
child(shakuni, sudarma).
child(ashwatthama, drona).
child(ashwatthama, kripi).
child(yudishthira, kunti).
child(yudishthira, dharma).
child(bhima, kunti).
child(bhima, vayu).
child(arjun, kunti).
child(arjun, indra).
child(nakul, madri).
child(nakul, ashwin).
child(sahdev, madri).
child(sahdev, ashwin).
child(karna, kunti).
child(karna, surya).
child(draupadi, drupada).
child(drishtadyumna, drupada).
child(shikhandi, drupada).
child(duryodhan, gandhari).
child(duryodhan, dhritrashtra).
child(dushasan, gandhari).
child(dushasan, dhritrashtra).
child(sukarna, gandhari).
child(sukarna, dhritrashtra).
child(vikarna, gandhari).
child(vikarna, dhritrashtra).
child(krishna, devaki).
child(krishna, vasudev).
child(balram, devaki).
child(balram, vasudev).
child(subhadra, vasudev).
child(subhadra, rohini).
child(kansa, ugrasena).
child(kansa, padmavati).
child(devaki, ugrasena).
child(devaki, padmavati).
child(ghatotkacha, bhima).
child(ghatotkacha, hidimbi).
child(abhimanyu, subhadra).
child(abhimanyu, arjun).
child(parikshit, abhimanyu).
child(parikshit, uttara).
child(janamejaya, parikshit).
child(janamejaya, madravati).
child(laxmankumara, duryodhan).
child(laxmankumara, bhanumati).
child(lakshmana, duryodhan).
child(lakshmana, bhanumati).
child(prativindhya, yudishthira).
child(prativindhya, draupadi).
child(sutasoma, bhima).
child(sutasoma, draupadi).
child(shrutkarma, arjun).
child(shrutkarma, draupadi).
child(shatanika, nakul).
child(shatanika, draupadi).
child(shrutsena,sahdev).
child(shrutsena, draupadi).
child(ghatotkacha,bhima).
child(ghatotkacha,hidimbi).
child(vidur,parishrami).
child(vidur,vyasa).

spouse(X,Y) :- married(X,Y).
spouse(X,Y) :- married(Y,X).
husband(X,Y) :- married(X,Y).
wife(X,Y) :- female(X),married(Y,X).

son(X,Y) :- male(X),child(X,Y).
daughter(X,Y) :- female(X),child(X,Y).

parent(X,Y) :- child(Y,X).
father(X,Y) :- male(X),child(Y,X).
mother(X,Y) :- female(X),child(Y,X).

grandchild(X,Y) :- child(X,Z),child(Z,Y).
grandson(X,Y) :- son(X,Z),child(Z,Y).
granddaughter(X,Y) :- daughter(X,Z),child(Z,Y).

anscestor(X,Y) :- parent(X,Y).
anscestor(X,Y) :- parent(X,Z),anscestor(Z,Y).
descendant(X,Y) :- child(X,Y).
descendant(X,Y) :- child(Z,Y),descendant(X,Z).
descendant(X,Y) :- spouse(Z,Y),descendant(X,Z).


sibling(X,Y) :- child(X,Z),child(Y,Z).
brother(X,Y) :- male(X),sibling(X,Y).
sister(X,Y) :- female(X),sibling(X,Y).

aunt(X,Y) :- sister(X,Z),child(Y,Z).
aunt(X,Y) :- spouse(X,W),male(W),sibling(W,Z),child(Y,Z).

parent-in-law(X,Y) :- spouse(Y,Z),parent(X,Z).
father-in-law(X,Y) :- spouse(Y,Z),father(X,Z).
mother-in-law(X,Y) :- spouse(Y,Z),mother(X,Z).

sibling-in-law(X,Y) :- spouse(Y,Z),sibling(X,Z).
brother-in-law(X,Y) :- spouse(Y,Z),brother(X,Z).
sister-in-law(X,Y) :- spouse(Y,Z),sister(X,Z).

children-in-law(X,Y) :- child(Y,Z),spouse(X,Z).
son-in-law(X,Y) :- daughter(Z,Y),spouse(Z,X).
daughter-in-law(X,Y) :- son(Z,Y),spouse(Z,X).

step-parent(X,Y) :- child(Y,Z),spouse(Z,X),not(child(Y,X)).
step-father(X,Y) :- mother(Z,Y),spouse(Z,X),not(child(Y,X)).
step-mother(X,Y) :- father(Z,Y),spouse(Z,X),not(child(Y,X)).

step-sibling(X,Y) :- step-parent(Z,Y),child(X,Z).
step-brother(X,Y) :- step-parent(Z,Y),son(X,Z).
step-sister(X,Y) :- step-parent(Z,Y),daughter(X,Z).


