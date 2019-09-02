:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(url)).
:- use_module(library(http/http_header)).
:- use_module(library(pcre)).
:- dynamic ping_match/4.
:- dynamic me/3.

url(Command, URL) :-
    token(Token),
    atomics_to_string(["https://api.telegram.org/bot", Token, "/", Command], URL).

get_updates(Data) :-
    url("getUpdates", URL),
    Headers = [request_header('Accept'='application/json')],
    setup_call_cleanup(
        http_open(URL, In, Headers),
        json_read_dict(In, Data),
        close(In)
    ).

update_offset(Message) :-
    url("getUpdates", URL),
    NewOffset is Message.get(update_id) + 1,
    catch(
        http_post(URL, form_data([offset = NewOffset]), _, []),
        error(_, context(_, status(ErrorCode, Response))),
        (
            format(string(Log), "Couldn't update offset: ~w - ~w", [ErrorCode, Response]),
            log_print(log_level('ERROR'), Log)
        )
    ).

is_command(Message) :-
    [Entity|_] = Message.get(message).get(entities),
    Entity.get(type) = "bot_command".

text_to_command(Message, Command, Args) :-
    split_string(Message, " ", " ", [CommandTmp|Args]),
    split_string(CommandTmp, "@", "/", [Command|_]).

log_print(log_level(LogLevel), Text) :-
    get_time(TimeStamp),
    round(TimeStamp, UnixTimeStamp),
    replace_emoji(Text, EmojiText),
    current_output(Out),
    format(Out, '[~d] [~s] ~s~n', [UnixTimeStamp, LogLevel, EmojiText]).

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
    open('db/pingers.pl', write, S),
    set_output(S),
    listing(ping_phrase),
    listing(ping_match),
    listing(me),
    close(S).

% telegram_command_ping_add
assert_match(ChatID, Username, Match) :-
    assert(ping_match(ChatID, Username, 0, Match)).

assert_match(ChatID, Username, Rating, Match) :-
    assert(ping_match(ChatID, Username, Rating, Match)).

% telegram_command_ping_delete
retract_match(ChatID, Username, Match) :-
    retract(ping_match(ChatID, Username, _, Match)).

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

% ping
get_ping_match(Message, Username) :-
    ChatID = Message.get(message).get(chat).get(id),
    MessageText = Message.get(message).get(text),
    unify_match(MessageText, TextUnified),
    strip_chars(TextUnified, [",", ".", ";", ":", "!", "?"], TextStripped),
    split_string(TextStripped, " ", " ", TextSplitted),
    consult('db/pingers'),
    ping_phrase(PingPhrase),
    member(PingPhrase, TextSplitted),
    ping_match(ChatID, Username, Rating, Match),
    member(Match, TextSplitted),
    increment_rating(ping_match(ChatID, Username, Rating, Match)).


increment_rating(ping_match(ChatID, Username, Rating, Match)) :-
    retract_match(ChatID, Username, Match),
    NewRating is Rating + 1,
    assert_match(ChatID, Username, NewRating, Match),
    sort_matches(ChatID, Username).

sort_matches(ChatID, Username) :-
    findall(
        X,
        (ping_match(ChatID, Username, Rating, Match), X = Rating-Match),
        Matches
    ),
    keysort(Matches, Sorted),
    reverse(Sorted, RevSorted),

    forall(member(_-XMatch, Matches),
          retract_match(ChatID, Username, XMatch)),

    findall(RevRate, member(RevRate-_, RevSorted), RevRates),
    list_to_set(RevRates, SetRates),
    forall(member(SRate, SetRates),
        (
            findall(UMatch, member(SRate-UMatch, RevSorted), MatchesUnsorted),
            sort(MatchesUnsorted, MatchesSorted),
            forall(member(SMatch, MatchesSorted),
                assert_match(ChatID, Username, SRate, SMatch))
        )
    ),

    save_matches.


% get_ping_match
strip_chars(X, [], X) :- !.
strip_chars(String, [H|Exclude], Result) :-
    split_string(String, H, "", StrippedList),
    atomics_to_string(StrippedList, Result1),
    strip_chars(Result1, Exclude, Result).

% telegram_command_location
get_locations_from_yandex(Location, Response) :-
    www_form_encode(Location, TextLocEncoded),
    YandexMapURL = "https://geocode-maps.yandex.ru/1.x/?format=json&geocode=",
    atomics_to_string([YandexMapURL, TextLocEncoded], "", URL),
    catch(
        setup_call_cleanup(
            http_open(URL, In, [request_header('Accept'='application/json')]),
            json_read_dict(In, Response),
            close(In)
        ),
        error(_, _),
        Response = _{}
    ),
    !.


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
        (
            format(string(Log), "Couldn't delete message: ~w - ~w", [ErrorCode, Response]),
            log_print(log_level('ERROR'), Log)
        )
    ).

capitalize(Text, Capitalized) :-
    string_chars(Text, [H|T]),
    upcase_atom(H, H1),
    string_chars(Capitalized, [H1|T]).

convert_emoji(Text, Emoji) :-
    atom_codes(Text, TextCodes),
    convert_emoji_(TextCodes, EmojiCodes),
    string_codes(Emoji, EmojiCodes).

convert_emoji_([], []) :- !.

convert_emoji_([H,L|T], [Z|T1]) :-
    H >= 55296, H =< 56319,
    L >= 56320, L =< 57343,
    Z is 65536+(H-55296)*1024+(L-56320),
    convert_emoji_(T, T1),
    !.

convert_emoji_([H|T], [H|T1]) :-
    convert_emoji_(T, T1),
    !.

replace_emoji(Text, Text) :-
    not(re_match("\u00a9|\u00ae|[\u2000-\u3300]|\ud83c[\ud000-\udfff]|\ud83d[\ud000-\udfff]|\ud83e[\ud000-\udfff]", Text)),
    !.

replace_emoji(Text, Output) :-
    string_concat(A, B, Text),
    string_concat(X, C, B),
    re_match("^(\u00a9|\u00ae|[\u2000-\u3300]|\ud83c[\ud000-\udfff]|\ud83d[\ud000-\udfff]|\ud83e[\ud000-\udfff])$", X),
    convert_emoji(X, Emoji),
    string_concat(A, Emoji, Head),
    replace_emoji(C, Tail),
    string_concat(Head, Tail, Output),
    !.
