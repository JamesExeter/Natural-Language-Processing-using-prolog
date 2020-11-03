% Work of James Brock Student number: 953238

% Notes:
% Every sentence passes and has the output requested in the CW document except for sentence 5, see sentence 5 for an explanation
% In the parse tree, np won't be listed on the verb phrase side since we have to explicitly define the noun phrase behaivour in usage
% so np on the RHS of the tree will simply be all of the rules the corresponding noun phrase uses instead

% queries can be formed using phrase(s(Tree), [words to be put in here]).
% or without the parse tree involved, use s([words to be put in here], []).

% CW test queries and outputs
% 1. ?- phrase(s(Tree), [the,woman,sees,the,apples]).
% Tree = s(np(det(p_lex(the)), nbar(n(p_lex(woman)))), vp(v(p_lex(sees)), np(det(p_lex(the)), nbar(n(p_lex(apples)))))) .

% 2. ?- phrase(s(Tree), [a,woman,knows,him]).
% Tree = s(np(det(p_lex(a)), nbar(n(p_lex(woman)))), vp(v(p_lex(knows)), pro(p_lex(him)))) .

% 3. ?- phrase(s(Tree), [two,woman,sees,a,man]).
% phrase(s(Tree), [two,woman,sees,a,man]).

% 4. ?- phrase(s(Tree), [two,women,see,a,man]).
% Tree = s(np(det(p_lex(two)), nbar(n(p_lex(women)))), vp(v(p_lex(see)), np(det(p_lex(a)), nbar(n(p_lex(man)))))) .
 
% 5. ?- phrase(s(Tree), [the,man,see,the,apples]). --> INCORRECT, SHOULD SAY false. Problem with the verb being used when 
% Tree = s(np(det(p_lex(the)), nbar(n(p_lex(man)))), vp(v(p_lex(see)), np(det(p_lex(the)), nbar(n(p_lex(apples)))))) .
% the noun it is used in context to is singular, the verb used for singular nouns should be singular and in the third person
% but since the lexicon for nouns does not include a grammatical person, that can't be matched unless the nouns include GPs.


% 6. ?- phrase(s(Tree), [the,men,see,the,apples]).
% Tree = s(np(det(p_lex(the)), nbar(n(p_lex(men)))), vp(v(p_lex(see)), np(det(p_lex(the)), nbar(n(p_lex(apples)))))) .

% 7. ?- phrase(s(Tree), [the,men,sees,the,apples]).
% false.

% 8. ?- phrase(s(Tree), [she,knows,the,man]).
% Tree = s(np(pro(p_lex(she))), vp(v(p_lex(knows)), np(det(p_lex(the)), nbar(n(p_lex(man)))))) .

% 9. ?- phrase(s(Tree), [she,know,the,man]).
% false.

% 10. ?- phrase(s(Tree), [us,sees,the,apple]).
% false.

% 11. ?- phrase(s(Tree), [i,know,a,short,man]).
% Tree = s(np(pro(p_lex(i))), vp(v(p_lex(know)), np(det(p_lex(a)), nbar(jp(adj(p_lex(short))), n(p_lex(man)))))) .

% 12. ?- phrase(s(Tree), [the,tall,woman,sees,the,red]).
% false.

% 13. ?- phrase(s(Tree), [the,young,tall,man,knows,the,old,short,woman]).
% Tree = s(np(det(p_lex(the)), nbar(jp(adj(p_lex(young)), jp(adj(p_lex(tall)))), n(p_lex(man)))), vp(v(p_lex(knows)), 
%np(det(p_lex(the)), nbar(jp(adj(p_lex(old)), jp(adj(p_lex(short)))), n(p_lex(woman)))))) .

% 14. ?- phrase(s(Tree), [a,man,tall,knows,the,short,woman]).
% false.

% 15. ?- phrase(s(Tree), [a,man,on,a,chair,sees,a,woman,in,a,room]).
% Tree = s(np(det(p_lex(a)), nbar(n(p_lex(man))), pp(prep(p_lex(on)), np(det(p_lex(a)), nbar(n(p_lex(chair)))))), 
% vp(v(p_lex(sees)), np(det(p_lex(a)), nbar(n(p_lex(woman))), pp(prep(p_lex(in)), np(det(p_lex(a)), nbar(n(p_lex(room)))))))) .

% 16. ?- phrase(s(Tree), [a,man,on,a,chair,sees,a,woman,a,room,in]).
% false.

% 17. ?- phrase(s(Tree), [a,tall,young,woman,in,a,room,sees,the,red,apples,under,the,chair]).
% Tree = s(np(det(p_lex(a)), nbar(jp(adj(p_lex(tall)), jp(adj(p_lex(young)))), n(p_lex(woman))), pp(prep(p_lex(in)), 
% np(det(p_lex(a)), nbar(n(p_lex(room)))))), vp(v(p_lex(sees)), np(det(p_lex(the)), nbar(jp(adj(p_lex(red))), n(p_lex(apples))), 
% pp(prep(p_lex(under)), np(det(p_lex(the)), nbar(n(p_lex(chair)))))))) .

% 18. ?- phrase(s(Tree), [the,woman,in,a,room,on,the,chair,in,a,room,in,the,room,sees,the,man]).
% Tree = s(np(det(p_lex(the)), nbar(n(p_lex(woman))), pp(prep(p_lex(in)), np(det(p_lex(a)), nbar(n(p_lex(room))), pp(prep(p_lex(on)), 
% np(det(p_lex(the)), nbar(n(p_lex(chair))), pp(prep(p_lex(in)), np(det(p_lex(...)), nbar(n(...)), pp(prep(...), np(..., ...))))))))), 
% vp(v(p_lex(sees)), np(det(p_lex(the)), nbar(n(p_lex(man)))))) .
% Guessing the parse tree added in ... in the p_lex since it had seen that order of words before and wanted to save space. But I beleive this is the correct output

%rules for the grammar

% a sentence / language in this DCG starts with a noun phrase followed by a verb phrase
% each takes two variables, the plurarity on the noun being used and the grammatical person such
% that they can be passed into the respective rules when needed

% the variables used throughout the rules are NP and GP which stand for Noun Plurality and Grammatical Person respectively
% when a 2 is added onto the end of them, that means that we're using a second word as two are required but they are still of
% the same type
s --> np(NP, GP), vp(NP, GP).

% list of noun phrase definitions

% in order for question 5 to pass, singular nouns can only be used with verbs in the third grammatical person
% for this to work, all singular nouns have been labelled as being in the third grammatical person as was confirmed
% by Adam over email.
% This alters the information in the original lexicon given to us but it needed to be done for full correctness


% allows us to add adjectives to subject nouns in the grammar as we would be able to for object nouns
% hence we can describe a man or woman more aptly such as young or short etc...
np(NP, _) --> det(NP), nbar(NP).

% an extension of the first np rule, allowing us to add a preoposition rule after a noun.
% we need to keep track of the Noun Plurality for the use in the determinant and the noun used as they need to be
% consistent with each other for grammatical correctness 
np(NP, _) --> det(NP), nbar(NP), pp.

% not given on the example parse tree in the CW outline but by considering the list of example sentences
% we need to be able to place a pronoun such as "I" which has to then be followed by the verb phrase and another
% noun phrase. An incredibly simple example phrase this rule could produce would be: "I see the man".
% this rule is the only one in the noun phrase rules that requires us to keep track of the grammatical person being used
% as well as the noun pluraility and ensure the pronoun is in respect to the subject rather than object.
% It would be ungrammatical therefore for the sentence: "You sees the man" as the grammatical person is incorrect 
np(NP, GP) --> pro(NP, GP, subject).

%list of verb phrase definitions

% verb phrase where we only need to ensure consistency with the noun pluralisation 
% consists of a verb followed by a determinant and noun, the determinant and noun
% can have a different noun plurality to the verb but they themselves must be consistent
% it could also be the case that all three have the same noun plurality 
vp(NP, GP) --> v(NP, GP), np(_, _).

% similar to the first noun phrase rule but here we can place a propositional phrase after the noun
% yet, again we can use two different noun pluralities for the verb and determinant and noun or use the same for
% all for 3, the propositional phrase doesn't need to regard this pluraility since it comes after the noun.
vp(NP, GP) --> v(NP, GP), np(_, _), pp.

% the final verb phrase rule where we place a verb with respect to the pluraility given to us,
% this verb can then be followed up by a pronoun where the pluraility or person does not matter
% but the pronoun is now the object in regards to the verb.
vp(NP, GP) --> v(NP, GP), pro(_, _, object).

% a noun is always placed after the last adjective as depicted in the parse tree
% an nbar is effectively a noun or a call to adjective noun so it can be replaced as such
nbar(NP) --> n(NP).
nbar(NP) --> jp, n(NP).

% rule for recursive adjective placing, either can place an adjective or place an adjective followed by the rule again
jp --> adj.
jp --> adj, jp.

% a prepositional phrase, a noun phrase that is preceeded by a propositional such as i, you etc...
% the noun phrase here does not need to be selective with the pluraility used or the grammatical person
% since it is only placed after a noun, omitting the need for considerations to prior grammatical rules 
pp --> prep, np(_, _).

% wrapping the lexicon
% lexicon wrapping for pronouns, take the arguments of the noun plurality, the grammatical person and whether
% it is a subject or object.
% Matches to any words corresponding to those arguments in the lexicon
pro(singular, 1, subject) --> [Word], {lex(Word, pro, singular, 1, subject)}.
pro(singular, 2, subject) --> [Word], {lex(Word, pro, singular, 2, subject)}.
pro(singular, 3, subject) --> [Word], {lex(Word, pro, singular, 3, subject)}.
pro(plural, 1, subject) --> [Word], {lex(Word, pro, plural, 1, subject)}.
pro(plural, 2, subject) --> [Word], {lex(Word, pro, plural, 2, subject)}.
pro(plural, 3, subject) --> [Word], {lex(Word, pro, plural, 3, subject)}.
pro(singular, 1, object) --> [Word], {lex(Word, pro, singular, 1, object)}.
pro(singular, 2, object) --> [Word], {lex(Word, pro, singular, 2, object)}.
pro(singular, 3, object) --> [Word], {lex(Word, pro, singular, 3, object)}.
pro(plural, 1, object) --> [Word], {lex(Word, pro, plural, 1, object)}.
pro(plural, 2, object) --> [Word], {lex(Word, pro, plural, 2, object)}.
pro(plural, 3, object) --> [Word], {lex(Word, pro, plural, 3, object)}.

% lexicon wrapping for verbs, taking the arugments of noun plurality and grammatical person
% matches any words that correlate to the provided arguments
% for plural verbs, the grammatical person does not matter
v(singular, 1) --> [Word], {lex(Word, v, singular, 1)}.
v(singular, 2) --> [Word], {lex(Word, v, singular, 2)}.
v(singular, 3) --> [Word], {lex(Word, v, singular, 3)}.
v(plural, _) --> [Word], {lex(Word, v, plural, _)}.

% lexicon wrapping for determinants, only taking the argument of the noun plurality
% the word "the" corresponds to both singular and plural noun plurality and so can be 
% returned from det being called with either.
det(singular) --> [Word], {lex(Word, det, singular)}.
det(plural) --> [Word], {lex(Word, det, plural)}.

% lexicon wrapping for nouns, nouns take the argument of noun plurality
n(singular) --> [Word], {lex(Word, n, singular)}.
n(plural) --> [Word], {lex(Word, n, plural)}.

% lexicon wrappings for adjectives and prepositions, neither of them take arguments
% meaning any adjective or prepositions can be used when inserted into a sentence
adj --> [Word], {lex(Word, adj)}.
prep --> [Word], {lex(Word, prep)}.

% word, grammatical category (pronoun), number (singular/plural), grammatical person (1st,2nd,or 3rd), and grammatical role (subject or object)
lex(i, pro, singular, 1, subject).
lex(you, pro, singular, 2, subject).
lex(he, pro, singular, 3, subject).
lex(she, pro, singular, 3, subject).
lex(it, pro, singular, 3, subject).
lex(we, pro, plural, 1, subject).
lex(you, pro, plural, 2, subject).
lex(they, pro, plural, 3, subject).
lex(me, pro, singular, 1,  object).
lex(you, pro, singular, 2, object).
lex(him, pro, singular, 3, object).
lex(her, pro, singular, 3, object).
lex(it, pro, singular, 3, object).
lex(us, pro, plural, 1, object).
lex(you, pro, plural, 2, object).
lex(them, pro, plural, 3, object).

% word, grammatical category (verb), number (singular/plural), grammatical person (1st, 2nd, 3rd)
lex(know, v, singular, 1).
lex(know, v, singular, 2).
lex(knows, v, singular, 3).
lex(know, v, plural, _).
lex(see, v, singular, 1).
lex(see, v, singular, 2).
lex(sees, v, singular, 3).
lex(see, v, plural, _).

% word, grammatical category (noun), number
lex(man, n, singular).
lex(woman, n, singular).
lex(apple, n, singular).
lex(chair, n, singular).
lex(room, n, singular).
lex(men, n, plural).
lex(women, n, plural).
lex(apples, n, plural).
lex(chairs, n, plural).
lex(rooms, n, plural).

% word, grammatical category (determiner), number
lex(a, det, singular).
lex(two, det, plural).
lex(the, det, _).

% word, grammatical category (preposition)
lex(on, prep).
lex(in, prep).
lex(under, prep).

% word, grammatical category (adjective)
lex(old, adj).
lex(young, adj).
lex(red, adj).
lex(short, adj).
lex(tall, adj).

% parse tree for the DCG
% since NP is already a used variable name, we make NP -> PNP to stand for Parse NP
s(s(PNP, VP)) --> np(NP,GP, PNP), vp(NP, GP, VP).

np(NP, _, np(DET, NBAR)) --> det(NP, DET), nbar(NP, NBAR).
np(NP, _, np(DET, NBAR, PP)) --> det(NP, DET), nbar(NP, NBAR), pp(PP).
np(NP, GP, np(PRO)) --> pro(NP, GP, subject, PRO).

vp(NP, GP, vp(V, PNP)) --> v(NP, GP, V), np(_,_,PNP).
vp(NP, GP, vp(V, PNP, PP)) --> v(NP, GP, V), np(_,_,PNP), pp(PP).
vp(NP, GP, vp(V, PRO)) --> v(NP, GP, V), pro(_, _, object, PRO).

nbar(NP, nbar(N)) --> n(NP, N).
nbar(NP, nbar(JP, N)) --> jp(JP), n(NP, N).

jp(jp(ADJ)) --> adj(ADJ).
jp(jp(ADJ, JP)) --> adj(ADJ), jp(JP).

pp(pp(PREP, PNP)) --> prep(PREP), np(_, _, PNP).

% adding the lexicon to the parse tree, rename the lexicon so it doesn't get confused with the above lexicon

pro(singular, 1, subject, pro(PRO)) --> [Word], {p_lex(Word, pro, singular, 1, subject, PRO)}.
pro(singular, 2, subject, pro(PRO)) --> [Word], {p_lex(Word, pro, singular, 2, subject, PRO)}.
pro(singular, 3, subject, pro(PRO)) --> [Word], {p_lex(Word, pro, singular, 3, subject, PRO)}.
pro(plural, 1, subject, pro(PRO)) --> [Word], {p_lex(Word, pro, plural, 1, subject, PRO)}.
pro(plural, 2, subject, pro(PRO)) --> [Word], {p_lex(Word, pro, plural, 2, subject, PRO)}.
pro(plural, 3, subject, pro(PRO)) --> [Word], {p_lex(Word, pro, plural, 3, subject, PRO)}.
pro(singular, 1, object, pro(PRO)) --> [Word], {p_lex(Word, pro, singular, 1, object, PRO)}.
pro(singular, 2, object, pro(PRO)) --> [Word], {p_lex(Word, pro, singular, 2, object, PRO)}.
pro(singular, 3, object, pro(PRO)) --> [Word], {p_lex(Word, pro, singular, 3, object, PRO)}.
pro(plural, 1, object, pro(PRO)) --> [Word], {p_lex(Word, pro, plural, 1, object, PRO)}.
pro(plural, 2, object, pro(PRO)) --> [Word], {p_lex(Word, pro, plural, 2, object, PRO)}.
pro(plural, 3, object, pro(PRO)) --> [Word], {p_lex(Word, pro, plural, 3, object, PRO)}.

v(singular, 1, v(V)) --> [Word], {p_lex(Word, v, singular, 1, V)}.
v(singular, 2, v(V)) --> [Word], {p_lex(Word, v, singular, 2, V)}.
v(singular, 3, v(V)) --> [Word], {p_lex(Word, v, singular, 3, V)}.
v(plural, _, v(V)) --> [Word], {p_lex(Word, v, plural, _, V)}.

det(singular, det(DET)) --> [Word], {p_lex(Word, det, singular, DET)}.
det(plural, det(DET)) --> [Word], {p_lex(Word, det, plural, DET)}.

n(singular, n(N)) --> [Word], {p_lex(Word, n, singular, N)}.
n(plural, n(N)) --> [Word], {p_lex(Word, n, plural, N)}.

adj(adj(ADJ)) --> [Word], {p_lex(Word, adj, ADJ)}.
prep(prep(PREP)) --> [Word], {p_lex(Word, prep, PREP)}.

% parse tree lexicon
% word, grammatical category (pronoun), number (singular/plural), grammatical person (1st,2nd,or 3rd), and grammatical role (subject or object)
p_lex(i, pro, singular, 1, subject, p_lex(i)).
p_lex(you, pro, singular, 2, subject, p_lex(you)).
p_lex(he, pro, singular, 3, subject, p_lex(he)).
p_lex(she, pro, singular, 3, subject, p_lex(she)).
p_lex(it, pro, singular, 3, subject, p_lex(it)).
p_lex(we, pro, plural, 1, subject, p_lex(we)).
p_lex(you, pro, plural, 2, subject, p_lex(you)).
p_lex(they, pro, plural, 3, subject, p_lex(they)).
p_lex(me, pro, singular, 1,  object, p_lex(me)).
p_lex(you, pro, singular, 2, object, p_lex(you)).
p_lex(him, pro, singular, 3, object, p_lex(him)).
p_lex(her, pro, singular, 3, object, p_lex(her)).
p_lex(it, pro, singular, 3, object, p_lex(it)).
p_lex(us, pro, plural, 1, object, p_lex(us)).
p_lex(you, pro, plural, 2, object, p_lex(you)).
p_lex(them, pro, plural, 3, object, p_lex(them)).

% word, grammatical category (verb), number (singular/plural), grammatical person (1st, 2nd, 3rd)
p_lex(know, v, singular, 1, p_lex(know)).
p_lex(know, v, singular, 2, p_lex(know)).
p_lex(knows, v, singular, 3, p_lex(knows)).
p_lex(know, v, plural, _, p_lex(know)).
p_lex(see, v, singular, 1, p_lex(see)).
p_lex(see, v, singular, 2, p_lex(see)).
p_lex(sees, v, singular, 3, p_lex(sees)).
p_lex(see, v, plural, _, p_lex(see)).

% word, grammatical category (noun), number
p_lex(man, n, singular,p_lex(man)).
p_lex(woman, n, singular, p_lex(woman)).
p_lex(apple, n, singular, p_lex(apple)).
p_lex(chair, n, singular, p_lex(chair)).
p_lex(room, n, singular, p_lex(room)).
p_lex(men, n, plural, p_lex(men)).
p_lex(women, n, plural, p_lex(women)).
p_lex(apples, n, plural, p_lex(apples)).
p_lex(chairs, n, plural, p_lex(chairs)).
p_lex(rooms, n, plural, p_lex(rooms)).

% word, grammatical category (determiner), number
p_lex(a, det, singular, p_lex(a)).
p_lex(two, det, plural, p_lex(two)).
p_lex(the, det, _, p_lex(the)).

% word, grammatical category (preposition)
p_lex(on, prep, p_lex(on)).
p_lex(in, prep, p_lex(in)).
p_lex(under, prep, p_lex(under)).

% word, grammatical category (adjective)
p_lex(old, adj, p_lex(old)).
p_lex(young, adj, p_lex(young)).
p_lex(red, adj, p_lex(red)).
p_lex(short, adj, p_lex(short)).
p_lex(tall, adj, p_lex(tall)).
