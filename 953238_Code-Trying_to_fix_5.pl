% Work of James Brock Student number: 953238

% Notes:
% In this grammar, nbar would have been used to place a noun or a jp, nbar was removed and instead replaced with either a direct call to a noun
% or uses the adjective_noun rule which allows the recursive placing of adjectives. 
% In the parse tree, np won't be listed on the verb phrase side since we have to explicitly define the noun phrase behaivour in usage
% so np on the RHS of the tree will simply be all of the rules the corresponding noun phrase uses instead

% queries can be formed using phrase(s(Tree), [words to be put in here]).
% or without the parse tree involved, use s([words to be put in here], []).

% CW test queries and outputs
% 1. ?- phrase(s(Tree), [the,woman,sees,the,apples]).
% Tree = s(np(det(parse_lex(the)), n(parse_lex(woman))), vp(v(parse_lex(sees)), det(parse_lex(the)), n(parse_lex(apples)))) .

% 2. ?- phrase(s(Tree), [a,woman,knows,him]).
%Tree = s(np(det(parse_lex(a)), n(parse_lex(woman))), vp(v(parse_lex(knows)), pro(parse_lex(him)))) .

% 3. ?- phrase(s(Tree), [two,woman,sees,a,man]).
% false.

% 4. ?- phrase(s(Tree), [two,women,see,a,man]).
% Tree = s(np(det(parse_lex(two)), n(parse_lex(women))), vp(v(parse_lex(see)), det(parse_lex(a)), n(parse_lex(man)))) .
 
% 5. ?- phrase(s(Tree), [the,man,see,the,apples]).
% false.

% 6. ?- phrase(s(Tree), [the,men,see,the,apples]).
% Tree = s(np(det(parse_lex(the)), n(parse_lex(men))), vp(v(parse_lex(see)), det(parse_lex(the)), n(parse_lex(apples)))) .

% 7. ?- phrase(s(Tree), [the,men,sees,the,apples]).
% false.

% 8. ?- phrase(s(Tree), [she,knows,the,man]).
% Tree = s(np(pro(parse_lex(she))), vp(v(parse_lex(knows)), det(parse_lex(the)), n(parse_lex(man)))) .

% 9. ?- phrase(s(Tree), [she,know,the,man]).
% false.

% 10. ?- phrase(s(Tree), [us,eats,the,apple]).
% false.

% 11. ?- phrase(s(Tree), [i,know,a,short,man]).
% Tree = s(np(pro(parse_lex(i))), vp(v(parse_lex(know)), det(parse_lex(a)), adjective_noun(adj(parse_lex(short))), n(parse_lex(man)))) .

% 12. ?- phrase(s(Tree), [the,tall,woman,sees,the,red]).
% false.

% 13. ?- phrase(s(Tree), [the,young,tall,man,knows,the,old,short,woman]).
% Tree = s(np(det(parse_lex(the)), n(parse_lex(man)), adjective_noun(adj(parse_lex(young)), adjective_noun(adj(parse_lex(tall))))), vp(v(parse_lex(knows)), 
% det(parse_lex(the)), adjective_noun(adj(parse_lex(old)), adjective_noun(adj(parse_lex(short)))), n(parse_lex(woman)))) .

% 14. ?- phrase(s(Tree), [a,man,tall,knows,the,short,woman]).
% false.

% 15. ?- phrase(s(Tree), [a,man,on,a,chair,sees,a,woman,in,a,room]).
% Tree = s(np(det(parse_lex(a)), n(parse_lex(man)), pp(prep(parse_lex(on)), np(det(parse_lex(a)), n(parse_lex(chair))))), vp(v(parse_lex(sees)), 
% det(parse_lex(a)), n(parse_lex(woman)), pp(prep(parse_lex(in)), np(det(parse_lex(a)), n(parse_lex(room)))))) .

% 16. ?- phrase(s(Tree), [a,man,on,a,chair,sees,a,woman,a,room,in]).
% false.

% 17. ?- phrase(s(Tree), [a,tall,young,woman,in,a,room,sees,the,red,apples,under,the,chair]).
% Tree = s(np(det(parse_lex(a)), n(parse_lex(woman)), adjective_noun(adj(parse_lex(tall)), adjective_noun(adj(parse_lex(young)))), pp(prep(parse_lex(in)), 
% np(det(parse_lex(a)), n(parse_lex(room))))), vp(v(parse_lex(sees)), det(parse_lex(the)), adjective_noun(adj(parse_lex(red))), n(parse_lex(apples)), 
% pp(prep(parse_lex(under)), np(det(parse_lex(the)), n(parse_lex(chair)))))) .

% 18. ?- phrase(s(Tree), [the,woman,in,a,room,on,the,chair,in,a,room,in,the,room,sees,the,man]).
% Tree = s(np(det(parse_lex(the)), n(parse_lex(woman)), pp(prep(parse_lex(in)), np(det(parse_lex(a)), n(parse_lex(room)), pp(prep(parse_lex(on)), np(det(parse_lex(the)), 
% n(parse_lex(chair)), pp(prep(parse_lex(in)), np(det(parse_lex(...)), n(parse_lex(...)), pp(prep(...), np(..., ...))))))))), vp(v(parse_lex(sees)), det(parse_lex(the)), 
% n(parse_lex(man)))) 
% Guessing the parse tree added in ... in the parse_lex since it had seen that order of words before and wanted to save space. But I beleive this is the correct output

%rules for the grammar

% a sentence / language in this DCG starts with a noun phrase followed by a verb phrase
% each takes two variables, the plurarity on the noun being used and the grammatical person such
% that they can be passed into the respective rules when needed

% the variables used throughout the rules are NP and GP which stand for Noun Plurality and Grammatical Person respectively
% when a 2 is added onto the end of them, that means that we're using a second word as two are required but they are still of
% the same type
s --> np(NP, GP), vp(NP, GP).
s --> pro(NP, _, subject), vp(NP, _).

% list of noun phrase definitions

% in order for question 5 to pass, singular nouns can only be used with verbs in the third grammatical person
% for this to work, all singular nouns have been labelled as being in the third grammatical person as was confirmed
% by Adam over email. This does not affect any correctness in the rest of the sentences.
% This alters the information in the original lexicon given to us but it needed to be done for full correctness

% the simplest case of a noun phrase, it is simply the above rule but without the need to place a prepositional phrase
% after the noun, we simply place a determinant and a noun, ensuring that the noun pluraility of the two are the same
np(NP, GP) --> det(NP), n(NP, GP).

% allows us to add adjectives to subject nouns in the grammar as we would be able to for object nouns
% hence we can describe a man or woman more aptly such as young or short etc...
np(NP, GP) --> det(NP), adjective_noun, n(NP, GP).

% an extension of the first np rule, allowing us to add a preoposition rule after a noun.
% we need to keep track of the Noun Plurality for the use in the determinant and the noun used as they need to be
% consistent with each other for grammatical correctness 
np(NP, GP) --> det(NP), n(NP, GP), pp.

% noun phrase to satisfy the condition of placing a determinant followed by 
% any number of adjectives as allowed by nbar followed by a prepositional phrase
% NP is passed in as we need to care about the plurality used in the determinant, with
% a wildcard used for the GP as it doesn't affect the grammatical correctness of the phrase  
% a preposition can then be appended to the noun phrase
np(NP, GP) --> det(NP), adjective_noun, n(NP, GP), pp.

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
vp(NP, GP) --> v(NP, GP), det(NP2), n(NP2, GP).

% almost the same as the verb rule above except that the noun can be preceeded by any number of adjectives
% as we implement the recursive adjective rule, e.g. the tall old man
vp(NP, GP) --> v(NP, GP), det(NP2), adjective_noun, n(NP2, GP).

% also similar to the first noun phrase rule but here we can place a propositional phrase after the noun
% yet, again we can use two different noun pluralities for the verb and determinant and noun or use the same for
% all for 3, the propositional phrase doesn't need to regard this pluraility since it comes after the noun.
vp(NP, GP) --> v(NP, GP), det(NP2), n(NP2, GP), pp.

% this rule combines the two rules preceeding it, allowing us to add any number of adjectives
% before the noun and then placing a propositional phrase after the noun
vp(NP, GP) --> v(NP, GP), det(NP2), adjective_noun, n(NP2, GP), pp.

% the final verb phrase rule where we place a verb with respect to the pluraility given to us,
% this verb can then be followed up by a pronoun where the pluraility or person does not matter
% but the pronoun is now the object in regards to the verb.
vp(NP, GP) --> v(NP, GP), pro(_, _, object).

% case of verb phrase where we don't care about the grammatical person used
% used when a pronoun has been used
vp(NP, GP) --> v(NP, GP), np(NP, _).

% combines the definitions of nbar and jp, can be used to recursively place
% an adjective and then call adjective noun again, in the higher call chain
% a noun is always placed after the last adjective as depicted in the phrase tree
% an nbar is effectively a noun or a call to adjective noun so it can be replaced as such
adjective_noun --> adj.
adjective_noun --> adj, adjective_noun.

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
n(singular, 3) --> [Word], {lex(Word, n, singular, 3)}.
n(plural, _) --> [Word], {lex(Word, n, plural, _)}.

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
lex(man, n, singular, 3).
lex(woman, n, singular, 3).
lex(apple, n, singular, 3).
lex(chair, n, singular, 3).
lex(room, n, singular, 3).
lex(men, n, plural, _).
lex(women, n, plural, _).
lex(apples, n, plural, _).
lex(chairs, n, plural, _).
lex(rooms, n, plural, _).

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
s(s(NP, _, PRO)) --> pro(NP, _, subject, PRO), vp(NP, _).

np(NP, GP, np(DET, N)) --> det(NP, DET), n(NP, GP, N).
np(NP, GP, np(DET, N, ADJN)) --> det(NP, DET), adjective_noun(ADJN), n(NP, GP, N).
np(NP, GP, np(DET, N, PP)) --> det(NP, DET), n(NP, GP, N), pp(PP).
np(NP, GP, np(DET, N, ADJN, PP)) --> det(NP, DET), adjective_noun(ADJN), n(NP, GP, N), pp(PP).
np(NP, GP, np(PRO)) --> pro(NP, GP, subject, PRO).

vp(NP, GP, vp(V, DET, N)) --> v(NP, GP, V), det(NP2, DET), n(NP2, GP, N).
vp(NP, GP, vp(V, DET, ADJN, N)) --> v(NP, GP, V), det(NP2, DET), adjective_noun(ADJN), n(NP2, GP, N).
vp(NP, GP, vp(V, DET, N, PP)) --> v(NP, GP, V), det(NP2, DET), n(NP2, GP, N), pp(PP).
vp(NP, GP, vp(V, DET, ADJN, N, PP)) --> v(NP, GP, V), det(NP2, DET), adjective_noun(ADJN), n(NP2, GP, N), pp(PP).
vp(NP, GP, vp(V, PRO)) --> v(NP, GP, V), pro(_, _, object, PRO).

vp(NP, GP, vp(V, PNP)) --> v(NP, GP, V), np(NP, _, PNP).

adjective_noun(adjective_noun(ADJ)) --> adj(ADJ).
adjective_noun(adjective_noun(ADJ, ADJN)) --> adj(ADJ), adjective_noun(ADJN).

pp(pp(PREP, PNP)) --> prep(PREP), np(_, _, PNP).

% adding the lexicon to the parse tree, rename the lexicon so it doesn't get confused with the above lexicon

pro(singular, 1, subject, pro(PRO)) --> [Word], {parse_lex(Word, pro, singular, 1, subject, PRO)}.
pro(singular, 2, subject, pro(PRO)) --> [Word], {parse_lex(Word, pro, singular, 2, subject, PRO)}.
pro(singular, 3, subject, pro(PRO)) --> [Word], {parse_lex(Word, pro, singular, 3, subject, PRO)}.
pro(plural, 1, subject, pro(PRO)) --> [Word], {parse_lex(Word, pro, plural, 1, subject, PRO)}.
pro(plural, 2, subject, pro(PRO)) --> [Word], {parse_lex(Word, pro, plural, 2, subject, PRO)}.
pro(plural, 3, subject, pro(PRO)) --> [Word], {parse_lex(Word, pro, plural, 3, subject, PRO)}.
pro(singular, 1, object, pro(PRO)) --> [Word], {parse_lex(Word, pro, singular, 1, object, PRO)}.
pro(singular, 2, object, pro(PRO)) --> [Word], {parse_lex(Word, pro, singular, 2, object, PRO)}.
pro(singular, 3, object, pro(PRO)) --> [Word], {parse_lex(Word, pro, singular, 3, object, PRO)}.
pro(plural, 1, object, pro(PRO)) --> [Word], {parse_lex(Word, pro, plural, 1, object, PRO)}.
pro(plural, 2, object, pro(PRO)) --> [Word], {parse_lex(Word, pro, plural, 2, object, PRO)}.
pro(plural, 3, object, pro(PRO)) --> [Word], {parse_lex(Word, pro, plural, 3, object, PRO)}.

v(singular, 1, v(V)) --> [Word], {parse_lex(Word, v, singular, 1, V)}.
v(singular, 2, v(V)) --> [Word], {parse_lex(Word, v, singular, 2, V)}.
v(singular, 3, v(V)) --> [Word], {parse_lex(Word, v, singular, 3, V)}.
v(plural, _, v(V)) --> [Word], {parse_lex(Word, v, plural, _, V)}.

det(singular, det(DET)) --> [Word], {parse_lex(Word, det, singular, DET)}.
det(plural, det(DET)) --> [Word], {parse_lex(Word, det, plural, DET)}.

n(singular, 3, n(N)) --> [Word], {parse_lex(Word, n, singular, 3, N)}.
n(plural, _, n(N)) --> [Word], {parse_lex(Word, n, plural, _, N)}.

adj(adj(ADJ)) --> [Word], {parse_lex(Word, adj, ADJ)}.
prep(prep(PREP)) --> [Word], {parse_lex(Word, prep, PREP)}.

% parse tree lexicon
% word, grammatical category (pronoun), number (singular/plural), grammatical person (1st,2nd,or 3rd), and grammatical role (subject or object)
parse_lex(i, pro, singular, 1, subject, parse_lex(i)).
parse_lex(you, pro, singular, 2, subject, parse_lex(you)).
parse_lex(he, pro, singular, 3, subject, parse_lex(he)).
parse_lex(she, pro, singular, 3, subject, parse_lex(she)).
parse_lex(it, pro, singular, 3, subject, parse_lex(it)).
parse_lex(we, pro, plural, 1, subject, parse_lex(we)).
parse_lex(you, pro, plural, 2, subject, parse_lex(you)).
parse_lex(they, pro, plural, 3, subject, parse_lex(they)).
parse_lex(me, pro, singular, 1,  object, parse_lex(me)).
parse_lex(you, pro, singular, 2, object, parse_lex(you)).
parse_lex(him, pro, singular, 3, object, parse_lex(him)).
parse_lex(her, pro, singular, 3, object, parse_lex(her)).
parse_lex(it, pro, singular, 3, object, parse_lex(it)).
parse_lex(us, pro, plural, 1, object, parse_lex(us)).
parse_lex(you, pro, plural, 2, object, parse_lex(you)).
parse_lex(them, pro, plural, 3, object, parse_lex(them)).

% word, grammatical category (verb), number (singular/plural), grammatical person (1st, 2nd, 3rd)
parse_lex(know, v, singular, 1, parse_lex(know)).
parse_lex(know, v, singular, 2, parse_lex(know)).
parse_lex(knows, v, singular, 3, parse_lex(knows)).
parse_lex(know, v, plural, _, parse_lex(know)).
parse_lex(see, v, singular, 1, parse_lex(see)).
parse_lex(see, v, singular, 2, parse_lex(see)).
parse_lex(sees, v, singular, 3, parse_lex(sees)).
parse_lex(see, v, plural, _, parse_lex(see)).

% word, grammatical category (noun), number
parse_lex(man, n, singular, 3,parse_lex(man)).
parse_lex(woman, n, singular, 3, parse_lex(woman)).
parse_lex(apple, n, singular, 3, parse_lex(apple)).
parse_lex(chair, n, singular, 3, parse_lex(chair)).
parse_lex(room, n, singular, 3, parse_lex(room)).
parse_lex(men, n, plural, _, parse_lex(men)).
parse_lex(women, n, plural, _, parse_lex(women)).
parse_lex(apples, n, plural, _, parse_lex(apples)).
parse_lex(chairs, n, plural, _, parse_lex(chairs)).
parse_lex(rooms, n, plural, _, parse_lex(rooms)).

% word, grammatical category (determiner), number
parse_lex(a, det, singular, parse_lex(a)).
parse_lex(two, det, plural, parse_lex(two)).
parse_lex(the, det, _, parse_lex(the)).

% word, grammatical category (preposition)
parse_lex(on, prep, parse_lex(on)).
parse_lex(in, prep, parse_lex(in)).
parse_lex(under, prep, parse_lex(under)).

% word, grammatical category (adjective)
parse_lex(old, adj, parse_lex(old)).
parse_lex(young, adj, parse_lex(young)).
parse_lex(red, adj, parse_lex(red)).
parse_lex(short, adj, parse_lex(short)).
parse_lex(tall, adj, parse_lex(tall)).
