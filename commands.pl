%
%! telegram_command_*(+Args:list, -Output:string) is det.
% @arg Args is list of strings
%

% Info command
% @arg Output is info about the bot
:- use_module(library(http/http_header)).

telegram_command_info(_, _, "OMG, I work somehow!\nPlease star me: https://github.com/Cuttlerat/prolog_bot").

% Date command
% @arg Args is [] or [TimeStamp]
% @arg TimeStamp is a UNIX timestamp
% @arg Output date in HTTP format
% If Args is empty, then current time will be taken

telegram_command_date([], _, Date) :-
    get_time(TimeStamp),
    get_date(TimeStamp, Date).

telegram_command_date([TimeStampStr], _, Date) :-
    number_string(TimeStamp, TimeStampStr),
    get_date(TimeStamp, Date).

get_date(TimeStamp, Date) :-
    http_timestamp(TimeStamp, Atom),
    atom_string(Atom, Date).

% Factorial command
% @arg Args [Number]
% @arg Number is string of int
% @arg Output is factorial of the Number

telegram_command_factorial([Input], _, Output) :-
    number_string(XNumber, Input),
    Number is round(XNumber),
    between(0, 1494, Number),
    factorial(Number, Factorial),
    number_string(Factorial, Output),
    !.

telegram_command_factorial(_, _, "false.").

factorial(X, Output) :- factorial(X, 1, Output).
factorial(0, Output, Output) :- !.
factorial(X, Y, Output) :-
    X1 is X - 1,
    Y1 is Y * X,
    factorial(X1, Y1, Output).


telegram_command_ping_add(Matches, Message, Output) :-
    string_concat("@", Message.get(message).get(from).get(username), Username),
    ChatID = Message.get(message).get(chat).get(id),
    exclude(ping_match(ChatID, Username), Matches, NewMatches),
    \+ NewMatches = [],
    save_matches(ChatID, Username, NewMatches),
    atomics_to_string(["Added matches:"|NewMatches], "\n", Output),
    !.

telegram_command_ping_add(_, _, "Nothing to add") :- !.

save_matches(_, _, []) :-
    open('pingers.pl', write, S),
    set_output(S),
    listing(ping_phrase),
    listing(ping_match),
    close(S),
    !.

save_matches(ChatID, Username, [Match|Tail]) :-
    assert(ping_match(ChatID, Username, Match)),
    save_matches(ChatID, Username, Tail).
