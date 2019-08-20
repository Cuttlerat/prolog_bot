
% telegram_command_date
get_date(TimeStamp, Date) :-
    http_timestamp(TimeStamp, Atom),
    atom_string(Atom, Date).
    

% telegram_command_factorial
factorial(X, Output) :- factorial(X, 1, Output).
factorial(0, Output, Output) :- !.
factorial(X, Y, Output) :-
    X1 is X - 1,
    Y1 is Y * X,
    factorial(X1, Y1, Output).

% telegram_command_ping_add
% telegram_command_ping_delete
save_matches :-
    open('pingers.pl', write, S),
    set_output(S),
    listing(ping_phrase),
    listing(ping_match),
    close(S).

% telegram_command_ping_add
assert_match(ChatID, Username, Match) :-
    assert(ping_match(ChatID, Username, Match)).

% telegram_command_ping_delete
retract_match(ChatID, Username, Match) :-
    retract(ping_match(ChatID, Username, Match)).
