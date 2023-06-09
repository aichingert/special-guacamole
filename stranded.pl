/* Stranded, by Aichinger Tobias, Ilming Winnie, Schludermann Julian. */

:- dynamic i_am_at/1, at/2, holding/1, is_ship_complete/0, marble_labels/1, has_unlocked_crate/0, comb_lock_user_state/1, translation_func/4, key/1, objects/2, tree_stump_state/1, has_solved_stump_puzzle/0.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(holding(_)), retractall(is_ship_complete), retractall(marble_labels(_)), retractall(has_unlocked_crate), retractall(comb_lock_user_state(_)), retractall(translation_func(_,_,_,_)), retractall(key(_)), retractall(objects(_,_)), retractall(tree_stump_state(_)), retractall(has_solved_stump_puzzle).

/* === Starting zone === */

i_am_at(beach).

/* === Paths to other zones === */

path(beach, n, forest).
path(beach, s, ocean) :- is_ship_complete,
        write('You drag the repaired ship into the water and set sail.'), nl,
        write('Yes! You think to yourself. You\'re finally going home.'), nl,
        write('...'), nl,
        write('A few hours pass by and all the energy and excitement you had before'), nl,
        write('vanished. You are out here, far away from your island. The sun is setting'), nl,
        write('waves are constantly hitting your boat and you also feel a bit dizzy because of this.'), nl,
        write('As a last ditch effort you use the pager you found on the beach and cry for help.'), nl,
        write('Again and again you press the little buttons but nothing happens. Not even a single sound.'), nl,
        write('Dehydrated, hungry and nauseous you pass out...'), nl,
        write('You open your eyes. Four men are standing next to you. You look around.'), nl,
        write('You are in the hospital. Was this all just a dream, was is saved from my boat?'), nl,
        write('What is happening!!'), nl,
        finish.
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
        not(cave_gate_part_one),
        write('You have to solve the puzzle first.'), nl, !, fail.
path(cave_entrance, enter, inner_cave_gate) :-
        write('You are going deeper into the cave.'), nl.
path(inner_cave_gate, enter, chamber) :-
        not(cave_gate_part_two),
        write('You have to submit the key first, maybe decipher the ancient message for information first!'), nl, !, fail.
path(inner_cave_gate, enter, chamber) :-
        write('You are now in the deepest part of the cave.'), nl, !.

path(forest, n, waterfall).
path(forest, e, cave).
path(forest, s, beach).
path(waterfall, s, forest).
path(cave, w, forest).
path(waterfall_room, back, waterfall).
path(cave_entrance, back, cave).
path(inner_cave_gate, back, cave_entrance).
path(chamber, back, inner_cave_gate).

path(waterfall, e, cave).

/* === Locations of items === */

at(pager, beach).
at(axe, inner_cave_gate).
at(hammer, chamber).

/* === Locations of objects === */

objects([shipwreck, lonely_stone], beach).
objects([tree], forest).
objects([tree_stump], waterfall).
objects([marbles], cave_entrance).
objects([axe], inner_cave_gate).
objects([crate], waterfall_room).

/* === Pick up items === */

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

/* === Helpers for shipwreck Interaction === */
repair_items([wood, axe, hammer, nails, cloth, pager]).

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

remove_element(Element, [Element], []).
remove_element(Element, [Element|Tail], Tail).
remove_element(Element, [Head|Tail], [Head|NewTail]) :- remove_element(Element, Tail, NewTail).

can_repair_wreck() :-
        repair_items(Items),(
        not(has_items(Items)),
        write('To successfully repair the wreck, gather those materials first!'),
        nl, !, fail
        ;
        has_items(Items)).

/* === Interact with objects === */
interact(shipwreck) :-
        i_am_at(beach),
        not(is_ship_complete),
        not(can_repair_wreck),!.
interact(shipwreck) :-
        i_am_at(beach),
        not(is_ship_complete),
        can_repair_wreck,
        write('Congratulations you gatherered all needed resources!'), nl,
        write('You successfully fixed the SHIPWRECK. You can now use the SHIP.'),
        assert(is_ship_complete),
        delete_wreck_items,
        objects(Objects, beach),
        retract(objects(Objects, beach)),
        remove_element(shipwreck, Objects, ReducedObjects),
        prepend(ship, ReducedObjects, NewObjects),
        assert(objects(NewObjects, beach)), !.
interact(shipwreck) :-
        i_am_at(beach),
        is_ship_complete,
        write('You already completed the ship. Venture out to escape!'),!.

interact(ship) :-
        i_am_at(beach),
        is_ship_complete,
        write('You already completed the ship. Venture out to escape!'),!.

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
        write('It was just a race AGAINST the CLOCK.'), nl,
        write('If I STARTed in the NORTH I would have gotten here first.'), nl,
        write('But now you are tired and TAKEing a quick nap would be nice.'), nl,
        write('But the ground is too hard and uncomfortable for you to sleep now.'), nl,
        write('Since this crate is LOCKED anyways you should probably explore some'), nl,
        write('of the other DIRECTIONS.'), nl,
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
      
interact(stump) :-
        not(i_am_at(waterfall)),
        write('...'), nl, !.

interact(stump) :-
        write('You approach the tree stump to take a closer look.'), nl,
        write('What you see inside blows your mind.'), nl,
        write('Small creatures looking like beetles.'), nl,
        write('Some of them are glowing, some are not.'), nl,
        write('They are also aligned horizontally and form multiple rows.'), nl,
        write('Out of curiosity you touch one of them'), nl,
        write('and to your suprise it starts glowing.'), nl,
        write('Ok weird...'), nl,
        write('Looking at the side you also notice a large X carved into the tree stump.'), nl,
        write('You get distracted a bit and start toching the beetles one by one.'), nl,
        write('Funnily enough you notice that only the beetles in the bottom most row'), nl,
        write('react to your touches.'), nl,
        write('There has to be more to this! You think to yourself...'), nl,
        describe_stump_puzzle, !.


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
        assert(marble_labels(NewLabels)),
        (cave_gate_part_one, write('Good job! You solved the puzzle! You can now advance further into the cave'), nl ; true), !.

swap :- not(i_am_at(cave_entrance)), i_am_at(Loc), wrong_place(Loc, 'swap'), !.
swap :-
        cave_gate_part_one,
        write('You already solved the puzzle, you don\'t have to roll anymore.'), nl, !.        
swap :-
        marble_labels(Labels),
        retract(marble_labels(Labels)),
        swap_first_and_second(Labels, NewLabels),
        assert(marble_labels(NewLabels)),
        (cave_gate_part_one, write('Good job! You solved the puzzle! You can now advance further into the cave'), nl ; true), !.

swap_first_and_second([First, Second | Tail], [Second, First | Tail]).

remove_last([Elem], [], Elem).
remove_last([Head|Tail], [Head|NewTail], Elem) :- remove_last(Tail, NewTail, Elem).

prepend(Elem, List, [Elem|List]).

is_in_asc_order(_, []) :- true, !.
is_in_asc_order(Start, [Head]) :- Start < Head, !.
is_in_asc_order(Start, [Head|Tail]) :-
        Start < Head , is_in_asc_order(Head, Tail), !.

cave_gate_part_one :- marble_labels([Head|Tail]), is_in_asc_order(Head, Tail).

/* === Cave puzzle 2: Write your own prolog function === */

key().
pass_key(_) :-
        not(i_am_at(inner_cave_gate)),
        write('Where do you intend to pass your key to? You are currently at the '),
        i_am_at(Loc), write(Loc), write('.'), nl, !.
pass_key(Key) :- 
        Key \== 'LOAL',
        write(Key), write(' is not the correct key, did you really translate the message?'), nl, !.
pass_key(Key) :- 
        assert(key(Key)), 
        write('This is the correct key! You can now go deeper into the cave.'), nl, !.

input(['℻', '๑', '⌘', '♗', 'ፚ', '⌘', ' ', '♗', '℻', '▙', '♗', '℻', '๑', '⌘', '♗', '∆', '⌘', 'Ξ', '∰', '⌘', '℻', '♗', 'Ξ', '๑', 'Æ', 'Հ', '⊙', '⌘', '∰', '♗', '⍼', '∆', '♗', '▙', 'ᘜ', '⌘', '♗', ' ', '▙', 'ᗅ', '♗', 'Æ', '⏻', '∰', '⌘', 'Æ', 'ꝟ', ' ', '♗', 'ፚ', 'ᘜ', '▙', '⌬', '⅁', '♗', '℻', '๑', '⍼', 'ᘜ', 'ፚ', '♗', 'Æ', '⊙', '▙', 'ᗅ', '℻', '♗', '℻', '๑', '⌘', '♗', '⊙', '⌘', '∆', '℻', '♗', '∆', 'ᗅ', '⊙', 'Ø', '⌘', 'Ξ', '℻', '♗', 'Æ', '℻', '♗', '∆', 'Ξ', '๑', '▙', '▙', '⏻', '♗', 'Æ', 'ᘜ', 'ꝟ', '♗', 'Հ', 'Æ', ' ', '⊙', '⌘', '♗', 'Ø', 'ᗅ', '∆', '℻', '♗', 'Հ', 'Æ', ' ', '⊙', '⌘', '♗', ' ', '▙', 'ᗅ', '♗', 'Æ', '∰', '⌘', '♗', 'Æ', '⏻', '∰', '⌘', 'Æ', 'ꝟ', ' ', '♗', '℻', '⌘', 'Æ', 'Ξ', '๑', '⍼', 'ᘜ', 'ᶋ', '♗', '⍼', '℻', '⅁']).

get_ancient_alphabet(['⍼', '∆', '⅁', 'ᙝ', 'ⵇ', '▙', '☭', 'ᶋ', 'Æ', '∰', 'Ψ', 'ᗅ', '⏻','⌬', 'Ø', '⌘', '♗', '๑', 'ꝟ', 'ፚ', 'ш', 'Ξ', 'ᘜ', '⊙', 'Հ', '℻', ' ', '⚝']).

get_translation(['I', 'S', '.', 'Q', 'Z', 'O', 'F', 'G', 'A', 'R', 'V', 'U', 'L', 'W', 'J', 'E', ' ', 'H', 'D', 'K', 'P', 'C', 'N', 'B', 'M', 'T', 'Y', 'X']).

cave_gate_part_two :- key(Keys), is_correct_key(Keys), !.
is_correct_key(Key) :- Key == 'LOAL', !.

% === Reference implementation (not optimal) ===
/*
find_translation(AncientCharacter, [AncientHead|AncientTail], [LatinHead|LatinTail], Translation) :-
    AncientHead == AncientCharacter,
    Translation = LatinHead
    ;
    find_translation(AncientCharacter, AncientTail, LatinTail, Translation).

translate(_, _, [], []).
translate(Ancient, Latin, [Head|Tail], [NewHead|NewTail]) :-
        find_translation(Head, Ancient, Latin, NewHead),
        translate(Ancient, Latin, Tail, NewTail), !.
*/

/* === Validating user rule === */

% This function loads a given function with 4 Parameters into our dynamic "translation_func"
% It's complicated and simple at the same time, so I will make some comments to explain what happens here
set_translation_func(_) :-
        not(i_am_at(inner_cave_gate)),
        write('You can\'t use that here'), !.

set_translation_func(Name) :-
    % Since we don't want multiple translation functions we delete everything in our dynamic first
    retractall(translation_func(_,_,_,_)),
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
        not(i_am_at(inner_cave_gate)),
        write('You can\'t use that here'), !.

test_translation :-
        get_ancient_alphabet(Alphabet),
        get_translation(Translation),
        input(Input),
        not(translation_func(Alphabet, Translation, Input, _)),
        write('The given translation function could not be executed or returned false!'), nl, !.

test_translation :-
        get_ancient_alphabet(Alphabet),
        get_translation(Translation),
        input(Input),
        translation_func(Alphabet, Translation, Input, Output),
        var(Output),
        write('The given translation function does not output anything on the last argument!'), nl, !.

test_translation :-
    % call function
    get_ancient_alphabet(Alphabet),
    get_translation(Translation),
    input(Input),
    translation_func(Alphabet, Translation, Input, Output),
    % Make sure Output is actually set to some value and not just a variable
    not(var(Output)),
    write('Your translation function\'s output:'), nl,
    print_array(Output, ''), nl, !.

/* === Waterfall room: crate puzzle === */

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
        write('It\'s a torch! Well and some nails... Weird why would anyone leave this here?'), nl,
        write('You better pick it up fast so the owner doesn\'t catch you red handed'), nl,
        assert(has_unlocked_crate),
        assert(at(torch, waterfall_room)),
        assert(at(nails, waterfall_room)), !
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

/* === Tree stump puzzle === */
tree_stump_row('o~-~o-~-o~-~o-~-o~-~o-~-o~-~o-~-o\n|   |   |   |   |   |   |   |   |\no-~-o~-~o-~-o~-~o-~-o~-~o-~-o~-~o').
tree_stump_row_chars(X) :- tree_stump_row(Chars), atom_chars(Chars, X).
% Hardcode go brrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
tree_stump_first_symbol_index(36).
tree_stump_symbol_increase(4).
tree_stump_symbols(8).

tree_stump_beetles(['o', '#']).

tree_stump_state([0,0,0,0,0,0,0,0]).
tree_stump_first_row([0,1,0,0,1,1,0,1]).
tree_stump_second_row([1,0,1,0,1,0,0,1]).

tree_stump_solution([], [], []).
tree_stump_solution([FirstHead|FirstTail], [SecondHead|SecondTail], [Head|Tail]) :-
        Head is FirstHead xor SecondHead,
        tree_stump_solution(FirstTail, SecondTail, Tail).

print_stump_row(Chars, [], _) :- print_array(Chars, '').
print_stump_row(Chars, [StateHead|StateTail], Count) :-
        tree_stump_beetles(Beetles),
        tree_stump_symbol_increase(Inc),
        tree_stump_first_symbol_index(Base),
        get_nth(StateHead, Beetles, Beetle),
        Idx is Count * Inc + Base,
        replace_nth(Beetle, Idx, Chars, NewChars),
        NewCount is Count + 1,
        print_stump_row(NewChars, StateTail, NewCount), !.

print_stump :-
        i_am_at(waterfall),
        not(has_solved_stump_puzzle),
        tree_stump_row_chars(Chars),
        tree_stump_first_row(FirstRow),
        tree_stump_second_row(SecondRow),
        tree_stump_state(UserRow),
        print_stump_row(Chars, FirstRow, 0), nl,
        print_stump_row(Chars, SecondRow, 0), nl,
        write('================================='), nl,
        print_stump_row(Chars, UserRow, 0), nl, !.

touch(Pos) :-
        i_am_at(waterfall),
        not(has_solved_stump_puzzle),
        tree_stump_symbols(Count),
        Pos > 0,
        Pos =< Count,
        Idx is Pos - 1,
        tree_stump_state(OldStates),
        retract(tree_stump_state(OldStates)),
        get_nth(Idx, OldStates, State),
        NewState is (State + 1) mod 2,
        replace_nth(NewState, Idx, OldStates, NewStates),
        assert(tree_stump_state(NewStates)),
        (
        tree_stump_first_row(FR),
        tree_stump_second_row(SR),
        tree_stump_solution(FR, SR, X),
        are_arrays_equal(X, NewStates),
        assert(has_solved_stump_puzzle),
        stump_puzzle_solve_text
        ;
        true
        ), !.

describe_stump_puzzle :-
        i_am_at(waterfall),
        not(has_solved_stump_puzzle),
        write('Now to the puzzle'), nl,
        write('Bring the beetles in the last row in the correct state!'), nl,
        write('#: indicates that the beetle is glowing'), nl,
        write('o: indicates that the beetle is not glowing'), nl,
        write('The following commands are available during this puzzle'), nl,
        write('touch(Pos).                 -- Toch the beetle at Pos (Pos = 1 == first beetle)'), nl,
        write('print_stump.                -- prints a visual representation of the stump'), nl,
        write('If you are stuck interact with the tree stump again and make sure to read the text carfully.'), nl,
        write('Good Luck!'), nl, !.


stump_puzzle_solve_text :-
        write('Respect. You solved the puzzle. You hear a little click and a small hidden door'), nl,
        write('unlocks. You look inside and pull out a tiny hat and put it into your iventory.'), nl,
        write('It\'s not much but it\'s honest work.'), nl,
        assert(holding(hat)), !.

/* === These rules define the direction letters as calls to go/1. === */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

enter :- go(enter).
back :- go(back).

/* === Move in a given direction === */
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, beach),
        cave_gate_part_two,
        not(holding(cloth)),
        not(at(cloth, beach)),
        write('You return to the beach and you notice immediately that something is different...'), nl,
        write('At the shore where you originally woke up at, a large pice of cloth was washed up.'), nl,
        write('How much luck do you have.'), nl,
        assert(at(cloth, beach)), fail.

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

/* === Step into site === */

enter(X) :-
        not(i_am_at(X)),
        write('You are currently not able to enter anything'), nl, !.

/* === Look around you === */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_items_at(Place),
        nl,
        (not(notice_objects_at(Place)), !); true.

/* === These rules set up a loop to mention all the objects in your vicinity. === */

notice_items_at(Place) :-
        at(X, Place),
        write('There '), get_phrase(X, Phrase), write(Phrase), write(X), write(' here.'), nl,
        fail.

notice_items_at(_).

get_phrase(nails, Phrase) :- Phrase = 'are some ', !.
get_phrase(cloth, Phrase) :- Phrase = 'is some ', !.
get_phrase(_, 'is a ').

/* === Notice objects in your vicinity === */

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

/* === Print items currently held === */
items :-
        write('You are currently holding: '),
        nl,
        not(holding(_)),
        write('Nothing...'),
        nl, !
        ;
        not(print_items), !.

inventory :- items.

print_items :-
        holding(X),
        write('- '),
        write(X),
        nl,
        fail.

/* === This rule tells how to die. === */

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

/* === Print game instructions === */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('story.             -- to listen to game\'s backstory.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Item).        -- to pick up an item.'), nl,
        write('interact(Object).  -- to interact with an object.'), nl,
        write('enter.             -- to step into a site.'), nl,
        write('back.              -- opposite of enter.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('items.             -- to see all items you are carrying.'), nl,
        write('inventory.         -- same as items.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.

/* === This rule prints out instructions and tells where you are. === */

start :-
        story,
        instructions,
        look.


/* === This rule prints out the backstory of the game === */
story :-
        nl,
        write('It felt like just seconds ago, when you were on a cruise'), nl,
        write('with your colleges enjoying the nice weather. Now you\'re left'), nl,
        write('with nothing except your dirty, wet and ripped clothes.'), nl,
        write('You have no idea where you came from nor where exactly you are now.'), nl,
        write('Around you is just water, lots of water. You seem to have stranded'), nl,
        write('on an island of some sorts.'), nl,
        write('But one thing is for sure. You can\'t stay here forever...'), nl,
        nl.

/* === Descriptions of rooms === */

describe(beach) :- write('You are at the beach. The only significant thing'), nl,
                write('seems to be a large forest to your north. Everything else'), nl,
                write('is just sand (to your east), the ocean (to your south) and more sand.'), nl,
                write('(to your west)'), nl,
                nl.

describe(forest) :- 
        write('You are at the forest. You hear water splashing in the near distance.'), nl,
        write('Because of the trees you can\'t see more.'), nl.

describe(waterfall) :-
        write('A waterfall. You can drink the fresh water directly from it.'), nl,
        write('Maybe you should explore some more before you continue your journey.'), nl.
describe(waterfall_room) :-
        write('You are behind the waterfall. It\'s really dark in here, but you can still see something.'), nl.

describe(cave) :-
        write('A mysterious dark cave. If you want to enter it grab some light source.'), nl.
describe(cave_entrance) :-
        write('There is a huge stone gate infront of you. On the right side you'), nl,
        write('notice a few small marbles integrated in the wall with numbers on them.'), nl, !.
describe(inner_cave_gate) :-
        not(cave_gate_part_two),
        write('The stone gate opens, as you walk inside and get deeper you see another gate.'), nl,
        write('While getting closer you see a few scratches on the wall and a sheet of papyrus on the floor.'), nl,
        write('Trying to see it better you bring the torch closer and you realise'), nl,
        write('that this is the language of an old civilization and on the papyrus is the translation for it.'), nl,
        write('You need to decipher the message to get the secret key!'), nl, 
        write('To accomplish this write your own prolog rule and use the provided utilities to check if it is right.'), nl,
        write('Your rule should have the following arguments => AncientAlphabet(List), Translation(List), Input(List), Output(List)'), nl,
        write('Where AncientAlphabet maps to Translation. Utils are listed below: '), nl,
        write('set_translation_func(Name).        --sets your rule, expects your rules name as parameter'), nl,
        write('test_translation.                  --which tests your rule'), nl,
        write('get_ancient_alphabet(Alphabet)     --puts the ancient alphabet in Alphabet'), nl,
        write('get_translation(Translation)       --puts the translation for the alphabet in Translation'), nl, nl,
        write('After you successfully translated the message pass the key:'), nl,
        write('pass_key(Key).                     --is used to pass a key to the gate(Key is a string)'), nl,
        write('The message you read is: '), nl,
        input(X),
        print_array(X, ''), nl,
        write('Don\'t worry, you don\'t have to copy this. The string will also be given to your '), nl,
        write('function when checking it with test_translation.'), nl, !.

describe(inner_cave_gate) :-
        write('This is the second gate of the cave.'), nl, !.
describe(chamber) :-
        write('This is the secret treasure vault from the ancient civilization where they stored their tools.'), nl, !.

/* === Descriptions of objects  === */

describe(shipwreck) :-
        write('A shipwreck.'), nl, write('If you had some wood, some nails, a hammer,'), nl,
        write('an axe and some cloth you could probably repair it.'), nl.

describe(lonely_stone) :-
        write('A lonely stone.'), nl,
        write('It seems to be the only stone on this beach.'), nl,
        write('For some reason it looks kind of sad and abandoned.'), nl,
        write('You probably don\'t want to stare at it for too long...'), nl. /* TODO: Stare command?*/

describe(tree) :-
        write('A tree.'), nl,
        write('Here are only trees, all you can see are trees.'), nl.

describe(marbles) :-
        not(cave_gate_part_one),
        write('These marbles are in a random order but they are in a circular formation.'), nl,
        write('So if you move one of them to the right the element at the bottom is going to'), nl,
        write('appear at the front again'), nl,
        write('There is a special rule which the marbles have to follow so that the gate opens.'), nl, 
        write('For example the following numbers are matching that rule:'), nl,
        write('2, 4, 16'), nl,
        write('3, 9, 81'), nl,
        write('4, 16, 256'), nl,
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

describe(ship) :-
        write('A ship'), nl,
        write('With your highly advanced tinkering skills you successfully repaired this ship.'), nl,
        write('Be proud of yourself.'), nl, !.

describe(tree_stump) :-
        write('A tree stump (stump)'), nl,
        write('It looks weird. Maybe you should take a closer look.'), nl.

/* === Reasons why the path in a specific direction is denied === */

deny(beach, e) :- write('You probably shouldn\'t explore the beach right now.'), nl.
deny(beach, w) :- deny(beach, e).
deny(forest, w) :-
        write('You start wandering west. Suddenly you hear a strange noise.'), nl,
        write('This sound does not seem very friendly. The last thing you would want right now'), nl,
        write('is to encounter something hostile... So you turn around and go back to where you came from.'), nl.
