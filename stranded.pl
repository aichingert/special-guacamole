/* Stranded, by Aichinger Tobias, Ilming Winnie, Schludermann Julian. */

:- dynamic i_am_at/1, at/2, holding/1, is_ship_complete/0.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* Starting zone */

i_am_at(beach).

/* Ship complete */
is_ship_complete :- false. /* TODO */

/* Paths to other zones */

path(beach, n, forest).
path(beach, s, ocean) :- is_ship_complete, write('TODO: ending'), nl, finish.
path(beach, s, ocean) :-
                        write('Are you out of your mind? '), nl,
                        write('Trying to cross the ocean without a ship seems like a'), nl,
                        write('suicide mission. You\'re lucky you survived!'), nl, !, fail.

path(forest, n, waterfall).
path(forest, e, cave).
path(forest, s, beach).

path(waterfall, e, cave).

/* Locations of items */

at(pager, beach).
/* at(axe, cave). */

/* Locations of objects */

objects([shipwreck, lonely_stone], beach).
objects([tree], forest).

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
        can_repair_wreck,
        write('You successfully fixed the SHIPWRECK. You can now use the SHIP.'),
        assert(is_ship_complete),
        delete_wreck_items,!.
interact(shipwreck) :-
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

interact(cave_entrance) :-
        i_am_at(cave),
        cave_gate_part_one, !.
interact(cave_entrance) :-
        not(i_am_at(cave)),
        write('I don\'t see a cave? Do you? Travel a bit more to explore the area.'), nl, !.
interact(cave_entrance) :-
        not(cave_gate_part_one),
        write('There is a huge stone gate infront of you. On the right site'), nl,
        write('you notice a few small marbles integrated in the wall with numbers'), nl,
        write('on them. You are able interact with the marbles.'), nl, !.
interact(marbles) :-
        not(cave_gate_part_one),
        write('These marbles are not in ascending order but they are in a circular formation.'), nl,
        write('So if you move one of them to the right the element at the bottom is going to'), nl,
        write('appear at the front again'), nl,
        write('Try to get them in the right order.'), nl, nl,
        write('You have the following options to interact with them: '), nl,
        write('roll.                --which moves the circle once'),
        write('inspect_marbles.     --shows you the order of the marbles'), nl, !.

interact(_) :-
        write('I don\'t see that here.'),
        i_am_at(Place),
        at(_,Place),
        write('But there seems to be an item around here...'), nl,
        write('Maybe you meant take(Item) instead?'), nl,
        false.

/* === Cave zone === */

numbers([4,6,1,3,5,2,7,8,9]).

inspect_marbles :- numbers(L), print_marbles(L), nl.

print_marbles([]) :- true, !.
print_marbles([Head]) :- write(Head), !.
print_marbles([Head | Tail]) :-
        write(Head), write(', '), print_marbles(Tail), !.

roll :-
        numbers(L),
        remove_last(L, Last),
        prepend(Last, [], L).

remove_last([Last], Last).
remove_last([_ | Tail], Last) :- remove_last(Tail, Last), !.

prepend(Elem, List, [Elem | List]).

is_in_asc_order(_, []) :- true, !.
is_in_asc_order(Start, [Head]) :- Start < Head, !.
is_in_asc_order(Start, [Head|Tail]) :-
        Start < Head , is_in_asc_order(Head, Tail), !.

cave_gate_part_one :- numbers([Head|Tail]), is_in_asc_order(Head, Tail).

/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


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

enter(waterfall) :-
        i_am_at(waterfall),
        retract(i_am_at(waterfall)),
        assert(i_am_at(waterfall_room)),
        write('You found a secret chamber behind the waterfall. Congratulations!'), nl,  !.

enter(cave) :-
        i_am_at(cave),
        not(holding(torch)),
        write('While going deeper into the cave you realize that you can\'t see anything.'), nl,
        write('You are starting to get scared as you hear strange noises. You run out of the cave.'), nl, !.
enter(cave) :-
        i_am_at(cave),
        holding(torch),
        retract(i_am_at(cave)),
        assert(i_am_at(cave_interiour)),
        write('While going deeper into the cave you realize that you can\'t see anything.'), nl,
        write('You remember that you collected a torch.'), nl,
        write('You light the torch and start to see the interour of the cave.'), nl, !.

enter(_) :-
        write('You are currently not able to enter anything'), nl.

/* Look around you */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_items_at(Place),
        nl,
        notice_objects_at(Place).

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
        describe_objects(Objects).


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
        write('enter(Location).   -- to step into a site.'), nl,
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
        write('You are behind the waterfall altough it is not very brigth, it is bright enough for you to see.'), nl.

describe(cave) :-
        write('A mysterious dark cave. If you want to enter it grab some light source.'), nl.
describe(cave_interiour) :-
        write('The interiour of the cave is cramped you can not see anything interesting from here.'), nl.

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

/* Reasons why the path in a specific direction is denied */

deny(beach, e) :- write('You probably shouldn\'t explore the beach right now.'), nl.
deny(beach, w) :- deny(beach, e).
deny(forest, w) :-
                write('You start wandering west. Suddenly you hear a strange noise.'), nl,
                write('This sound does not seem very friendly. The last thing you would want right now'), nl,
                write('is to encounter something hostile... So you turn around and go back to where you came from.'), nl.
