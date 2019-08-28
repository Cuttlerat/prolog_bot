:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(url)).
:- use_module(library(http/http_header)).
:- use_module(library(pcre)).
:- dynamic ping_match/3.
:- dynamic me/3.
?- consult(emoji).

url(Command, URL) :-
    token(Token),
    atomics_to_string(["https://api.telegram.org/bot", Token, "/", Command], URL).

get_updates(Data) :-
    url("getUpdates", URL),
    setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json')]),
        json_read_dict(In, Data),
        close(In)
    ).

update_offset(Message) :-
    url("getUpdates", URL),
    NewOffset is Message.get(update_id) + 1,
    http_post(URL, form_data([offset = NewOffset]), _, []).

is_command(Message) :-
    [Entity|_] = Message.get(message).get(entities),
    Entity.get(type) = "bot_command".

command_to_name(Command, Name) :-
    atomic_concat("telegram_command_", Command, Name).

text_to_command(Message, Command, Args) :-
    split_string(Message, " ", " ", [CommandTmp|Args]),
    split_string(CommandTmp, "@", "/", [Command|_]).

log_print(Message, Text) :-
    get_time(TimeStamp),
    round(TimeStamp, UnixTimeStamp),
    replace_emoji(Text, EmojiText),
    format('[~d] ~d: ~s~n', [UnixTimeStamp,
                             Message.get(message).get(chat).get(id),
                             EmojiText]).

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
    listing(me),
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
    catch(
        setup_call_cleanup(
            http_open(URL, In, [request_header('Accept'='application/json')]),
            json_read_dict(In, Response),
            close(In)
        ),
        error(_, _),
        fail
    ),
    !.

get_locations_from_yandex(_, _{}).


% telegram_command_set_me
update_me(ChatID, UserID, Match) :-
    me(ChatID, UserID, Match),
    !.

update_me(ChatID, UserID, Match) :-
    me(ChatID, UserID, _),
    retract(me(ChatID, UserID, _)),
    assert(me(ChatID, UserID, Match)),
    save_matches,
    !.

update_me(ChatID, UserID, Match) :-
    assert(me(ChatID, UserID, Match)),
    save_matches.


% telegram_command_me
delete_message(Message) :-
    MessageID = Message.get(message).get(message_id),
    ChatID = Message.get(message).get(chat).get(id),
    url("deleteMessage", URL),
    catch(
        http_post(URL, form_data([ message_id = MessageID,
                                   chat_id = ChatID ]), _, []),
        error(_, context(_, status(ErrorCode, Response))),
        format("[ERROR] Couldn't delete message: ~w - ~w~n", [ErrorCode, Response])
    ).

capitalize(Text, Capitalized) :-
    string_chars(Text, [H|T]),
    upcase_atom(H, H1),
    string_chars(Capitalized, [H1|T]).


replace_emoji(Text, Text) :-
    not(re_match("\u00a9|\u00ae|[\u2000-\u3300]|\ud83c[\ud000-\udfff]|\ud83d[\ud000-\udfff]|\ud83e[\ud000-\udfff]", Text)),
    !.

replace_emoji(Text, Output) :-
    string_concat(A, B, Text),
    string_concat(X, C, B),
    re_match("^(\u00a9|\u00ae|[\u2000-\u3300]|\ud83c[\ud000-\udfff]|\ud83d[\ud000-\udfff]|\ud83e[\ud000-\udfff])$", X),
    emoji(Emoji, X),
    string_concat(A, Emoji, Head),
    replace_emoji(C, Tail),
    string_concat(Head, Tail, Output),
    !.
