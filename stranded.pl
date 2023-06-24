/* Stranded, by Aichinger Tobias, Ilming Winnie, Schludermann Julian. */

:- dynamic i_am_at/1, at/2, holding/1, is_ship_complete/0, marble_labels/1, has_unlocked_crate/0, comb_lock_user_state/1, translation_func/4, key/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* */
/* === Starting zone === */

i_am_at(beach).

/* === Paths to other zones === */

path(beach, n, forest).
path(beach, s, ocean) :- is_ship_complete, write('TODO: ending'), nl, finish.
path(beach, s, ocean) :-
        write('Are you out of your mind? '), nl,
        write('Trying to cross the ocean without a ship seems like a'), nl,
        write('suicide mission. You\'re lucky you survived!'), nl, !, fail.
path(waterfall, enter, waterfall_room) :-
        write('You found a secret chamber behind the waterfall. Congratulations!'), nl.

path(cave, enter, cave_entrance) :-
        not(holding(torch)),
        write('While going deeper into the cave you realize that you can\'t see anything.'), nl,
        write('You are starting to get scared as you hear strange noises. You run out of the cave.'), nl, !, fail.
path(cave, enter, cave_entrance) :-
        write('While going deeper into the cave you realize that you can\'t see anything.'), nl,
        write('You remember that you collected a torch.'), nl,
        write('You light the torch and start to see the entrance of the cave.'), nl.
path(cave_entrance, enter, inner_cave_gate) :-
        cave_gate_part_one,
        write('The stone gate opens, as you walk inside and get deeper you see another gate.'), nl,
        write('While getting closer you see a few scratches on the wall and a sheet of papyrus on the floor.'), nl,
        write('Trying to see it better you bring the torch closer and you realise'), nl,
        write('that this is the language of an old civilization and on the papyrus is the translation for it.'), nl,
        write('You need to decipher the message to get the secret key!'), nl, 
        write('To accomplish this write your own prolog rule and use the provided utilities to check if it is right.'), nl,
        write('Your rule should have the following fields => AncientAlphabet(List), Translation(List), Input(List), Output(List)'), nl,
        write('Where AncientAlphabet maps to Translation. Utils are listed below: '), nl,
        write('set_translation_func(Name).        --sets your rule, expects your rules name as parameter'), nl,
        write('test_translation.                  --which tests your rule'), nl,
        write('get_ancient_alphabet(Alphabet)     --puts the ancient alphabet in Alphabet'), nl,
        write('get_translation(Translation)       --puts the translation for the alphabet in Translation'), nl, !.
path(inner_cave_gate, enter, chamber) :-
        cave_gate_part_two,
        write('You finally enter the deepest part of the cave.'), nl, !.

path(forest, n, waterfall).
path(forest, e, cave).
path(forest, s, beach).
path(waterfall, s, forest).
path(cave, w, forest).
path(waterfall_room, back, waterfall).
path(cave_entrance, back, cave).
path(inner_cave_gate, back, cave_entrance).

path(waterfall, e, cave).

/* Locations of items */

at(pager, beach).
at(axe, inner_cave_gate).

/* Locations of objects */

objects([shipwreck, lonely_stone], beach).
objects([tree], forest).
objects([marbles], cave_entrance).
objects([axe], inner_cave_gate).
objects([crate], waterfall_room).

/* Pick up items. */

take(X) :-
        holding(X),
        write('You\'re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don\'t see it here.'),
        nl,
        false.

/* Helpers for shipwreck Interaction */
repair_items([wood, saw, hammer, nails, cloth]).

has_items([Head|Tail]) :-
        not(holding(Head)), write('You are missing: '), write(Head), nl, has_items(Tail), fail, !.
has_items([Head|Tail]) :-
        holding(Head), has_items(Tail).
has_items([]).

delete_items([Head|Tail]) :-
        holding(Head), retract(holding(Head)), delete_items(Tail).
delete_items([]).

delete_wreck_items :-
        repair_items(Items), delete_items(Items).

can_repair_wreck() :-
        repair_items(Items), (
        not(has_items(Items)),
        write('To successfully repair the wreck, gather those materials first!'),
        nl, !, fail
        ;
        write('Congratulations you gatherered all needed resources!'),
        nl, !).

/* Interact with objects */
interact(shipwreck) :-
        i_am_at(beach),
        not(is_ship_complete),
        not(can_repair_wreck),!.
interact(shipwreck) :-
        i_am_at(beach),
        not(is_ship_complete),
        can_repair_wreck,
        write('You successfully fixed the SHIPWRECK. You can now use the SHIP.'),
        assert(is_ship_complete),
        delete_wreck_items,!.
interact(shipwreck) :-
        i_am_at(beach),
        is_ship_complete,
        write('You already completed the ship. Venture out to escape'),!.

interact(tree) :-
        i_am_at(forest),
        not(is_ship_complete),
        not(at(wood, forest)),
        not(holding(wood)),
        holding(axe),
        assert(at(wood, forest)), 
        write('You chopped down the tree, now there is wood laying around.'), nl, !.
interact(tree) :-
        not(i_am_at(forest)),
        write('You are not in the forest.'), nl, !.
interact(tree) :-
        is_ship_complete,
        write('The ship is already completed.'), nl, !.
interact(tree) :-
        at(wood, forest),
        write('There is already wood laying around.'), nl, !.
interact(tree) :-
        holding(wood),
        write('You already have wood. Are you really that greedy?'), nl, !.
interact(tree) :-
        not(holding(axe)),
        write('You are trying to cut a tree with your hands?'), nl,
        write('Is everything alright inside your head?'), nl,
        write('Maybe use that head of yours to find an axe.'), nl, !.

interact(crate) :-
        i_am_at(waterfall_room),
        not(has_unlocked_crate),
        write('No matter how much you try, you just can\'t open this crate.'), nl,
        write('So you will have to do it the boring way and crack the combination'), nl,
        write('of the lock. After 25 Minutes of brute force and out of ideas you'), nl,
        write('think to yourself: I got HERE FROM the BEACH in such LITTLE TIME.'), nl,
        write('Just a race AGAINST the CLOCK and a fight against the BLOCK.'), nl,
        write('If I STARTed in the NORTH I would have gotten here first.'), nl,
        describe_crate_puzzle, nl, !.
interact(crate) :-
        i_am_at(waterfall_room),
        has_unlocked_crate,
        write('Luckily you already figured out how to unlock this crate.'), nl,
        write('You never want to go through that pain again...'), !.
interact(crate) :-
        not(i_am_at(waterfall_room)),
        write('?????????'), nl,
        write('What exactly are you trying to do?'), nl, !.

interact(gate) :-
        not(i_am_at(inner_cave_gate)),
        write('Are your delusions getting out of hand? There is no gate at the '), i_am_at(Loc), write(Loc), nl,
        write('Maybe rest for a bit so you can focus again.'), nl, !.
interact(gate) :-
        not(cave_gate_part_one),
        write('How do you know about this? Put the marbles in the right order first!'), !.
interact(gate) :-
        not(translation_func(_)),
        write('Write your own prolog rule first and set it with set_translation_func.'), !.
interact(gate) :-
        cave_gate_part_one.
        
interact(_) :-
        write('I don\'t see that here.'), nl,
        i_am_at(Place),
        at(_,Place),
        write('But there seems to be an item around here...'), nl,
        write('Maybe you meant take(Item) instead?'), nl,
        false.

/* === Cave zone === */

marble_labels([3,1,4,2,9,8,6,7,5]).

wrong_place(Loc, Action) :-
        write('What do you intend to '), write(Action),
        write('? You are currently at the '), write(Loc),
        write(' and there is nothing to '), write(Action), nl, !.

inspect_marbles :- not(i_am_at(cave_entrance)), i_am_at(Loc), wrong_place(Loc, 'inspect'), !.
inspect_marbles :-
        cave_gate_part_one,
        write('You already solved the puzzle, you don\'t have to inspect the marbles anymore.'), nl, !.
inspect_marbles :- 
        marble_labels(L), print_marbles(L), nl.

print_marbles([]) :- true, !.
print_marbles([Head]) :- write(Head), !.
print_marbles([Head | Tail]) :-
        write(Head), write(', '), print_marbles(Tail), !.

roll :- not(i_am_at(cave_entrance)), i_am_at(Loc), wrong_place(Loc, 'roll'), !.
roll :-
        cave_gate_part_one,
        write('You already solved the puzzle, you don\'t have to roll anymore.'), nl, !.
roll :-
        marble_labels(Labels),
        retract(marble_labels(Labels)),
        remove_last(Labels, ReducedLabels, Elem),
        prepend(Elem, ReducedLabels, NewLabels),
        assert(marble_labels(NewLabels)), !.

swap :- not(i_am_at(cave_entrance)), i_am_at(Loc), wrong_place(Loc, 'swap'), !.
swap :-
        cave_gate_part_one,
        write('You already solved the puzzle, you don\'t have to roll anymore.'), nl, !.        
swap :-
        marble_labels(Labels),
        retract(marble_labels(Labels)),
        swap_first_and_second(Labels, NewLabels),
        assert(marble_labels(NewLabels)), !.

swap_first_and_second([First, Second | Tail], [Second, First | Tail]).

remove_last([Elem], [], Elem).
remove_last([Head|Tail], [Head|NewTail], Elem) :- remove_last(Tail, NewTail, Elem).

prepend(Elem, List, [Elem|List]).

is_in_asc_order(_, []) :- true, !.
is_in_asc_order(Start, [Head]) :- Start < Head, !.
is_in_asc_order(Start, [Head|Tail]) :-
        Start < Head , is_in_asc_order(Head, Tail), !.

cave_gate_part_one :- marble_labels([Head|Tail]), is_in_asc_order(Head, Tail).

/* Cave puzzle 2: Write your own prolog function
 *
 * Message: The key is the best school subject -> LOAL
*/

key([]).
pass_key(Key) :- assert(key(Key)).

input(['ፚ', 'ᶋ឵', 'ⵇ', ' ', 'Ψ', 'ⵇ', 'Հ', ' ', 'Æ', 'ꝟ', ' ', 'ፚ', 'ᶋ឵', 'ⵇ', ' ', '∆', 'ⵇ', 'ꝟ', 'ፚ', ' ', 'ꝟ', 'ш', '∆', '∰ަ', 'ⵇ', '⅁', 'ፚ']).

get_ancient_alphabet(Alphabet) :- 
        Alphabet    = ['⍼', '∆', '⅁', 'ᙝ', 'ⵇ', '▙', '☭', 'ᶋ឵', 'Æ', '∰ަ', 'Ψ', 'ᗅ', '⏻'','⌬', 'Ø', '⌘', '♗', '๑', 'ꝟ', 'ፚ', 'ш', 'Ξ', 'ᘜ', '⊙', 'Հ', '℻'', ' '].
get_translation(Translation) :-
        Translation = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', ' '].

cave_gate_part_two :- key(Keys), is_correct_key(Keys), !.

is_correct_key([]) :- false.
is_correct_key([Head | Tail]) :- are_arrays_equal(Head, ['L','O','A','L']) ; is_correct_key(Tail), !.

% This function loads a given function with 4 Parameters into our dynamic "translation_func"
% It's complicated and simple at the same time, so I will make some comments to explain what happens here
set_translation_func(Name) :-
    % Since we don't want multiple translation functions we delete everything in our dynamic first
    retractall(translation_func(_)),
    % Here we get the Functor (https://www.swi-prolog.org/pldoc/man?predicate=functor/3) by name and arity in this case 4
    functor(Help, Name, 4),
    % Here we use clause (https://www.swi-prolog.org/pldoc/doc_for?object=clause/2) to get the clause body to the functor we just aquired
    clause(Help , Body),
    % Since the clause could tecnically be a fact (what we don't want in this case) we have to check for it (Body == true => fact) (\== <=> !=)
    Body \== true,
    % Now we use (https://www.swi-prolog.org/pldoc/man?predicate=%3D../2) to split our functor into the rule's name and it's arguments
    % since we are only interested in it's arguments, because we actually want to rename the given function, we discard the head of the list
    Help =.. [_|Arguments],
    % Now we reasseble a new functor with a new function name (the one we choose: "translation_func") and the arguments as before
    NewMethod =.. [translation_func | Arguments],
    % Finally We build a new clause and add it to our dynamic database
    assert(NewMethod :- Body).

% Now that the function is loaded we want to check if the function translates correctly
% To do that we just call the function with some data and expect some output in the last variable
test_translation :-
    % Check if database has function
    translation_func(_),
    % call function
    translation_func([],[],[], Output),
    % Make sure Output is actually set to some value and not just a variable
    not(var(Output)),
    % For testing purposes print the value of output
    write(Output), nl.

/* Waterfall room: crate puzzle */
combination_lock('|---|---|---|---|---|---|\n|   |   |   |   |   |   |\n|---|---|---|---|---|---|\n').
combination_lock_chars(X) :- combination_lock(Chars), atom_chars(Chars, X).
combination_lock_values(['N', 'E', 'S', 'W']).
comb_lock_user_state([0,0,0,0,0,0]).

% Inaccessible zone starting from beach and north going counterclockwise: B: W, S, E; F: W; W: N, W
comb_lock_secret_state([3,2,1,3,0,3]).

% Hardcode go brrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
comb_lock_first_symbol_index(28).
comb_lock_symbol_increase(4).
comb_lock_symbols(6).

are_arrays_equal([], []).
are_arrays_equal([Head1|Tail1], [Head2|Tail2]) :-
        Head1 == Head2,
        are_arrays_equal(Tail1, Tail2).

print_array([], _).
print_array([Head], _) :- write(Head).
print_array([Head|Tail], Delimiter) :-
        write(Head),
        write(Delimiter),
        print_array(Tail, Delimiter).

replace_nth(NewChar, 0, [_|Tail], [NewChar|Tail]).
replace_nth(NewChar, Index, [Head|Tail], [Head|NewTail]) :-
        Index > 0,
        NewIndex is Index - 1,
        replace_nth(NewChar, NewIndex, Tail, NewTail), !.

get_nth(0, [Head|_], Head).
get_nth(Index, [_|Tail], Elem) :-
        Index > 0,
        NewIndex is Index - 1,
        get_nth(NewIndex, Tail, Elem), !.

adapt_chars_to_state(Chars, [], _, Chars).
adapt_chars_to_state(Chars, [Head|Tail], Count, Out) :-
        combination_lock_values(Vals),
        get_nth(Head, Vals, Symbol),
        comb_lock_first_symbol_index(Base),
        comb_lock_symbol_increase(Inc),
        Idx is Count * Inc + Base,
        replace_nth(Symbol, Idx, Chars, Intermediate),
        NewCount is Count + 1,
        adapt_chars_to_state(Intermediate, Tail, NewCount, Out).

print_lock :-
        i_am_at(waterfall_room),
        combination_lock_chars(Chars),
        comb_lock_user_state(State),
        adapt_chars_to_state(Chars, State, 0, AdaptedChars),
        print_array(AdaptedChars, ''), !.

rotate(Pos) :-
        i_am_at(waterfall_room),
        not(has_unlocked_crate),
        comb_lock_symbols(SymbolsCount),
        Pos =< SymbolsCount,
        Pos > 0,
        comb_lock_user_state(OldStates),
        retract(comb_lock_user_state(OldStates)),
        Idx is Pos - 1,
        get_nth(Idx, OldStates, OldState),
        NewState is (OldState + 1) mod 4, % 4...possible symbols
        replace_nth(NewState, Idx, OldStates, NewStates),
        assert(comb_lock_user_state(NewStates)), !.

pull_on_lock :-
        i_am_at(waterfall_room),
        not(has_unlocked_crate),
        write('You pull on the lock and....'), nl,
        comb_lock_secret_state(Solution),
        comb_lock_user_state(User),
        are_arrays_equal(Solution, User),
        write('The lock clicks open. Congratulations!'), nl,
        write('You open the crate and look inside...'), nl,
        write('It\'s a torch! Weird why would anyone leave it here?'), nl,
        write('You better pick it up fast so the owner doesn\'t catch you redhanded'), nl,
        assert(has_unlocked_crate),
        assert(at(torch, waterfall_room))
        ;
        write('Nothing.'), nl,
        write('Damn it!'), nl,
        write('If you\'re stuck, interact with the crate again'), nl,
        write('and make sure to think outside the box.'), nl, !.

describe_crate_puzzle :-
        i_am_at(waterfall_room),
        write('It\'s puzzle time'), nl,
        write('You are given a combination lock with a sequence consisting of five symbols.'), nl,
        write('Each Slot can contain: '), combination_lock_values(Vals), print_array(Vals, ', '), nl,
        write('You can use the following commands:'), nl,
        write('rotate(Pos).              -- rotates the disc at the given position (first disc = 1)'), nl,
        write('pull_on_lock.             -- pull on the lock\'s shackle and check if it unlocks'), nl,
        write('print_lock.               -- prints the current state of the lock'), nl,
        write('Good Luck! You are gonna need it.'), nl,
        write('Current lock state: '), nl,
        print_lock, !.

/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

enter :- go(enter).
back :- go(back).

/* Move in a given direction */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.
go(Direction) :-
        i_am_at(Loc),
        deny(Loc,Direction), !.
go(_) :-
        write('You can\'t go that way!'), nl.

/* Step into site */

enter(X) :-
        not(i_am_at(X)),
        write('You are currently not able to enter anything'), nl, !.

/* Look around you */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_items_at(Place),
        nl,
        (not(notice_objects_at(Place)), !); true.

/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_items_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_items_at(_).

/* Notice objects in your vicinity */

notice_objects_at(Place) :-
        write('Looking around you also see the following:'), nl,
        objects(Objects, Place),
        describe_objects(Objects)
        ;
        write('Nothing...').


describe_objects([Head|Tail]) :-
        describe(Head),
        nl,
        describe_objects(Tail).

describe_objects([]).

/* Print items currently held */
items :-
        write('You are currently holding: '),
        nl,
        not(holding(_)),
        write('Nothing...'),
        nl, !
        ;
        not(print_items), !.

print_items :-
        holding(X),
        write('- '),
        write(X),
        nl,
        fail.
/* This rule tells how to die. */

die :-
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.


/* Print game instructions */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('story.             -- to listen to game\'s backstory.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Item).        -- to pick up an item.'), nl,
        write('interact(Object).  -- to interact with an object'), nl,
        write('enter.             -- to step into a site.'), nl,
        write('back.              -- opposite of enter.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('items.             -- to see all items you are carrying'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        story,
        instructions,
        look.


/* This rule prints out the backstory of the game */
story :-
        nl,
        write('It felt like just seconds ago, when you were on a cruise'), nl,
        write('with your colleges enjoing the nice weather. Now you\'re left'), nl,
        write('with nothing except your dirty, wet and ripped clothes.'), nl,
        write('You have no idea where you came from nor where exactly you are now.'), nl,
        write('Around you is just water, lots of water. You seem to have stranded'), nl,
        write('on an island of some sorts.'), nl,
        write('But one thing is for sure. You can\'t stay here forever...'), nl,
        nl.


/* Descriptions of rooms */

describe(beach) :- write('You are at the beach. The only significant thing'), nl,
                write('seems to be a large forest to your north. Everything else'), nl,
                write('is just sand to your east, the ocean to your south and more sand'), nl,
                write('to your east'), nl,
                nl.

describe(forest) :- 
        write('You are at the forest. You hear water splashing in the near distance.'), nl,
        write('Because of the trees you can\'t see more.'), nl.

describe(waterfall) :-
        write('A waterfall you can drink the fresh water directly from it.'), nl,
        write('Maybe you should explore some more before you continue your journey.'), nl.
describe(waterfall_room) :-
        write('You are behind the waterfall. It\'s really dark in here, but you can still see something.'), nl.

describe(cave) :-
        write('A mysterious dark cave. If you want to enter it grab some light source.'), nl.
describe(cave_entrance) :-
        write('There is a huge stone gate infront of you. On the right site you'), nl,
        write('notice a few small marbles integrated in the wall with numbers on them.'), nl, !.
describe(inner_cave_gate) :-
        write('You walk even deeper in the cave and see a new gate with a new problem in the distance'), nl, !.

/* Descriptions of objects */
describe(shipwreck) :-
        write('A shipwreck.'), nl, write('If you had some wood, some nails, a hammer,'), nl,
        write('a saw and some cloth you could probably repair it.'), nl.

describe(lonely_stone) :-
        write('A lonely stone.'), nl,
        write('It seems to be the only stone on this beach.'), nl,
        write('For some reason it looks kind of sad and abandoned.'), nl,
        write('You probably don\'t want to stare at it for too long...'), nl. /* TODO: Stare command?*/

describe(tree) :-
        write('A tree.'), nl,
        write('Here are only trees, all you can see are trees.'), nl.

describe(axe) :-
        write('An axe can be used to cut trees.'), nl.

describe(marbles) :-
        write('These marbles are in a random order but they are in a circular formation.'), nl,
        write('So if you move one of them to the right the element at the bottom is going to'), nl,
        write('appear at the front again'), nl,
        write('There is a special rule which the marbles have to follow so that the gate opens.'), nl, 
        write('For example the following numbers are matching that rule:'), nl,
        write('2, 4, 16'), nl,
        write('3, 9, 81'), nl,
        write('1, 2, 3'), nl,
        write('You have the following options to interact with them: '), nl,
        write('roll.                --which moves the circle once'), nl,
        write('swap.                --which swaps the first two elements'), nl,
        write('inspect_marbles.     --shows you the order of the marbles'), nl, !.

describe(crate) :-
        write('A crate.'), nl,
        has_unlocked_crate,
        holding(torch),
        write('Do you remeber it? Your torch. You stole it from here... '),
        nl,
        write('Luckly you don\'t have to find out the passcode again...'), nl, !
        ;
        write('You can barely see anything in here but it looks like it '),
        nl,
        write('has a combination lock guarding whatever is inside.'), !.

/* Reasons why the path in a specific direction is denied */

deny(beach, e) :- write('You probably shouldn\'t explore the beach right now.'), nl.
deny(beach, w) :- deny(beach, e).
deny(forest, w) :-
        write('You start wandering west. Suddenly you hear a strange noise.'), nl,
        write('This sound does not seem very friendly. The last thing you would want right now'), nl,
        write('is to encounter something hostile... So you turn around and go back to where you came from.'), nl.
