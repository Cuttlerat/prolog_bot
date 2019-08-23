:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(url)).
:- use_module(library(http/http_header)).
:- dynamic ping_match/3.

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

% telegram_command_ping_add
% telegram_command_ping_delete
% get_ping_match
% unify_match
replace(From,To,In,Out) :-
    maplist(string_codes, [From, To, In],
                          [[FromCode], [ToCode], InCodes]),
    replace_(FromCode, ToCode, InCodes, OutCodes),
    string_codes(Out, OutCodes).

replace_(_,_,[],[]).
replace_(X,Y,[X|T],[Y|NT]) :-
    replace_(X,Y,T,NT),
    !.
replace_(X,Y,[H|T],[H|NT]) :-
    replace_(X,Y,T,NT).

% telegram_command_ping_add
% telegram_command_ping_delete
unify_match(In, Out) :-
    string_lower(In, Tmp),
    replace("ё", "е", Tmp, Out).

% router
get_ping_match(Message, Username) :-
    string_lower(Message.get(message).get(text), Text),
    consult(pingers),
    ping_phrase(PingPhrase),
    strip_chars(Text, [",", ".", ";", ":", "!", "?"], TextStripped),
    replace("ё", "е", TextStripped, TextUnified),
    split_string(TextUnified, " ", " ", TextSplitted),
    member(PingPhrase, TextSplitted),
    ping_match(ChatID, Username, Match),
    Message.get(message).get(chat).get(id) = ChatID,
    member(Match, TextSplitted).

% get_ping_match
strip_chars(X, [], X) :- !.
strip_chars(String, [H|Exclude], Result) :-
    split_string(String, H, "", StrippedList),
    atomics_to_string(StrippedList, Result1),
    strip_chars(Result1, Exclude, Result).

% telegram_command_location
get_locations_from_yandex(Location, Response) :-
    www_form_encode(Location, TextLocEncoded),
    atomics_to_string(["https://geocode-maps.yandex.ru/1.x/?format=json&geocode=", TextLocEncoded], "", URL),
    setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json')]),
        json_read_dict(In, Response),
        close(In)
    ).
