%%%%%%%%%-------------------------%%%%%%%%%


% Begins the program
go :-
    write('Hello, I am INS BOT.'), nl,
    write('What is your name?'),nl,
    insurance_bot.


% Reads a query sentence from user and forms a reply (Main function)
insurance_bot :-
    nl, write('--> '), read_atomics(Input), insurance_bot(Input), !.


insurance_bot([bye]) :-
    write('Goodbye. I hope I have helped you'), nl, halt.
insurance_bot([goodbye]) :-
    write('Goodbye. Take care!'), nl, halt.
insurance_bot(Input) :-
    pattern(Stimulus, Response),
    match(Stimulus, Dictionary, Input),
    match(Response, Dictionary, Output),
    write('insurance_bot: '),
    reply(Output),
    !, insurance_bot.


% matching
match([N|Pattern], Dictionary, Target) :- integer(N), lookup(N, Dictionary, LeftTarget), append(LeftTarget, RightTarget, Target),
    match(Pattern, Dictionary, RightTarget).

match([Word | Pattern], Dictionary, [Word | Target]) :- atom(Word), match(Pattern, Dictionary, Target).

match([], _Dictionary, []).



%  greetings
pattern([hello,my,name,is,1],['Nice',to,meet,you,1,'!','Which',insurance,'do',you,want,A,'or',B,'?']) :- insurance(A,B).
pattern([hi,i,am,1],['Nice',to,meet,you,1,'!','Which',insurance,'do',you,want,A,'or',B,'?']):- insurance(A,B).

%  pattern([1], ['Hey,',1,'!','How',can, i, help, you,'?']).
pattern([thank,you],Ans) :- choose([['I',am,glad,'I',could,help,'!'],
                                    ['My','Pleasure','!']],Reply), Ans=Reply.


/* these are pattern for solution parts */

pattern([tel,no,1],[tel,nonuz,1,dir]).
pattern([tc,no,1],[tc,nonuz,1,dir]).
pattern([plaka,1],[plaka,numaraniz,1,dir]).

pattern([car],[A,'or',B]) :- car(A,B).
pattern([traffic],['About : ',A,B]) :- infotraffic(A),phone(B).
pattern(['phone5377789999'],[A]) :- id(A).
pattern(['id12345678999'],[A]) :- plate(A).
pattern(['06aaa06'],[A]) :- doc(A).
pattern(['bg123456'],[A,B,'.',C]) :- call0(A),car(B,_),call1(C).

pattern([car],[A,'or',B]) :- car(A,B).
pattern([kasko],['About : ',A,B]) :- infokasko(A),phone(B).
pattern(['phone5377789998'],[A]) :- id(A).
pattern(['id12345678998'],[A]) :- plate(A).
pattern(['06aaa05'],[A]) :- doc(A).
pattern(['bg123455'],[A,B,'.',C]) :- call0(A),car(_,B),call1(C).

pattern([health],['About : ',A,B]) :- infohealth(A),phone(B).
pattern(['phone5377789997'],[A]) :- id(A).
pattern(['id12345678997'],[A,B,'.',C]) :- call0(A),health(B),call1(C).


%  wrong question type
pattern([1],Ans):-choose([['I',see,but,'I',can,not,answer,this,'.',
                           'Please',ask,me,another,question,'.'],
                          ['I',do,not,understand,'.','Please',
                           can,you,be,a,little,more,clear,'?']],Reply), Ans=Reply.


% knowledge-base

infotraffic('Covers the damage of the other person in the event of an accident.').
infokasko(': It protects your own vehicle against accident and other stolen, etc. situations.').
infohealth('It covers all or some of the risk that a person incurs medical expenses.').

insurance('car','health').
car('traffic','kasko').
phone('Phone number:').
id('ID number:').
health('health').
plate('Plate number').
doc('Document number:').
call0('We will call you for your insurance').
call1('For quit say bye or goodbye.').


%  print the reply
reply([Head | Tail]) :-
    write(Head), write(' '), reply(Tail).
reply([]) :- nl.


%  uses the integer position as index for Dictionary and returns the
%  word at that index
lookup(Key, [(Key, Value) | _Dict], Value).
lookup(Key, [(Key1, _Val1) | Dictionary], Value) :-
    Key \= Key1, lookup(Key, Dictionary, Value).


%  choose(List, Elt) - chooses a random element
%  in List and unifies it with Elt.
choose([], []).
choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).

/*
   read_atomics(-Atomics)
   reads a line of text, breaking it into a
   list of atomic terms: [this,is,an,example].
*/

read_atomics(Atomics) :-
   read_char(FirstC,FirstT),
   complete_line(FirstC,FirstT,Atomics).

/*
  read_char(-Char,-Type)
  reads a character and runs it through char_type/3.
*/

read_char(Char,Type) :-
   get0(C),
   char_type(C,Type,Char).


/* complete_line(+FirstC,+FirstT,-Atomics)
   given FirstC (the first character) and FirstT (its type), reads
   and tokenizes the rest of the line into atoms and numbers.
*/

complete_line(_,end,[]) :- !.                  % stop at end

complete_line(_,blank,Atomics) :-              % skip blanks
   !,
   read_atomics(Atomics).

complete_line(FirstC,special,[A|Atomics]) :-   % special char
   !,
   name(A,[FirstC]),
   read_atomics(Atomics).

complete_line(FirstC,alpha,[A|Atomics]) :-     % begin word
   complete_word(FirstC,alpha,Word,NextC,NextT),
   name(A,Word),  % may not handle numbers correctly - see text
   complete_line(NextC,NextT,Atomics).

/*
   complete_word(+FirstC,+FirstT,-List,-FollC,-FollT)
   given FirstC (the first character) and FirstT (its type),
   reads the rest of a word, putting its characters into List.
*/

complete_word(FirstC,alpha,[FirstC|List],FollC,FollT) :-!, read_char(NextC,NextT),
   complete_word(NextC,NextT,List,FollC,FollT).

complete_word(FirstC,FirstT,[],FirstC,FirstT).
   % where FirstT is not alpha

/*
   char_type(+Code,?Type,-NewCode)
   given an ASCII code, classifies the character as
   'end' (of line-file), 'blank', 'alpha'(numeric), or 'special',
   and changes it to a potentially different character (NewCode).
*/

char_type(10,end,10) :- !.         % UNIX end of line mark
char_type(13,end,13) :- !.         % DOS end of line mark
char_type(-1,end,-1) :- !.         % get0 end of file code

char_type(Code,blank,32) :-        % blanks, other ctrl codes
  Code =< 32,
  !.

char_type(Code,alpha,Code) :-      % digits
  48 =< Code, Code =< 57,
  !.

char_type(Code,alpha,Code) :-      % lower-case letters

  97 =< Code, Code =< 122,
  !.

char_type(Code,alpha,NewCode) :-   % upper-case letters
  65 =< Code, Code =< 90,
  !,
  NewCode is Code + 32.            %  (translate to lower case)

char_type(95,alpha,95):-!.		   % '_'
char_type(46,alpha,46):-!.		   % '.'
char_type(Code,special,Code).      % all others




