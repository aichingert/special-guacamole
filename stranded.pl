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
                        write('suicide mission. You''re lucky you survived!'), nl, !, fail.

path(forest, n, waterfall).
path(forest, e, cave).
path(waterfall, e, cave).

/* Locations of items */

at(pager, beach).
at(saw, cave).
at(nails, waterfall).
at(hammer, waterfall).
at(cloth, cave).
/* at(axe, cave). */

/* Locations of objects */

objects([shipwreck, lonely_stone], beach).
objects([tree], forest).

/* Pick up items. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* Helpers for shipwreck Interaction */
repair_items([wood, saw, hammer, nails]).

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
        repair_items(Items), has_items(Items).

/* Interact with objects */
interact(shipwreck) :-
        not(is_ship_complete),
        can_repair_wreck,
        write('You successfully fixed the SHIPWRECK. You can now use the SHIP.'),
        assert(is_ship_complete),
        delete_wreck_items,!.
interact(shipwreck) :-
        is_ship_complete,
        write('You already completed the ship. Venture out to escape'),!.

interact(tree) :-
        not(is_ship_complete),
        not(at(wood, forest)),
        not(holding(wood)),
        holding(axe),
        i_am_at(forest),
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
        write('You can''t go that way!').


/* Look around you */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_items_at(Place),
        notice_objects_at(Place),
        nl.


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
        write('story.             -- to listen to game''s backstory.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Item).        -- to pick up an item.'), nl,
        write('interact(Object).  -- to interact with an object'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
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
        write('with your colleges enjoing the nice weather. Now you''re left'), nl,
        write('with nothing except your dirty, wet and ripped clothes.'), nl,
        write('You have no idea where you came from nor where exactly you are now.'), nl,
        write('Around you is just water, lots of water. You seem to have stranded'), nl,
        write('on an island of some sorts.'), nl,
        write('But one thing is for sure. You can''t stay here forever...'), nl,
        nl.


/* Descriptions of rooms */

describe(beach) :- write('You are at the beach. The only significant thing'), nl,
                write('seems to be a large forest to your north. Everything else'), nl,
                write('is just sand to your east, the ocean to your south and more sand'), nl,
                write('to your east'), nl,
                nl.

describe(forest) :- 
        write('You are at the forest. You hear water splashing in the near distance.'), nl,
        write('Because of the trees you can''t see more.'), nl.

/* Descriptions of objects */
describe(shipwreck) :-
        write('A shipwreck.'), nl, write('If you had some wood, some nails, a hammer,'), nl,
        write('a saw and some cloth you could probably repair it.'), nl.

describe(lonely_stone) :-
        write('A lonely stone.'), nl,
        write('It seems to be the only stone on this beach.'), nl,
        write('For some reason it looks kind of sad and abandoned.'), nl,
        write('You probably don''t want to stare at it for too long...'), nl. /* TODO: Stare command?*/

describe(tree) :-
        write('A tree.'), nl,
        write('Here are only trees, all you can see are trees.'), nl.

/* Reasons why the path in a specific direction is denied */

deny(beach, e) :- write('You probably shouldn''t explore the beach right now.'), nl.
deny(beach, w) :- deny(beach, e).