:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(clpfd)).

?- consult(token).
?- consult(utils).

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

construct_functor(Name, Args, Functor) :-
    length(Args, Arity),
    functor(Functor, Name, Arity),
    construct_functor_(Name, Args, Functor, Arity, 0).

construct_functor_(_, [], _, X, X) :- !.
construct_functor_(Name, [Arg|Args], Functor, Arity, ArgNumber) :-
    ArgNumber1 is ArgNumber + 1,
    arg(ArgNumber1, Functor, Arg),
    construct_functor_(Name, Args, Functor, Arity, ArgNumber1).

log_print(Message, Text) :-
    get_time(TimeStamp),
    round(TimeStamp, UnixTimeStamp),
    format('[~d] ~d: ~s~n', [UnixTimeStamp,
                             Message.get(message).get(chat).get(id),
                             Text]).


send_message(location(Lat, Lon), MessageID, ChatID) :-
    send_location(location(Lat, Lon), MessageID, ChatID),
    !.

send_message(Text, MessageID, ChatID) :-
    url("sendMessage", URL),
    catch(
        http_post(URL, form_data([ text = Text,
                                   reply_to_message_id = MessageID,
                                   chat_id = ChatID ]), _, []),
        error(_, context(_, status(ErrorCode, Response))),
        format("[ERROR] Couldn't send message: ~w - ~w~n", [ErrorCode, Response])).

send_location(location(Lat, Lon), MessageID, ChatID) :-
    url("sendLocation", URL),
    catch(
        http_post(URL, form_data([ latitude = Lat,
                                   longitude = Lon,
                                   reply_to_message_id = MessageID,
                                   chat_id = ChatID ]), _, []),
        error(_, context(_, status(ErrorCode, Response))),
        format("[ERROR] Couldn't send location: ~w - ~w~n", [ErrorCode, Response])).

router(command, Message) :-
    text_to_command(Message.get(message).get(text), Command, Args),
    command_to_name(Command, Name),
    construct_functor(Name, [Args, Message, Text], Functor),
    consult(commands),
    current_predicate(_, Functor),
    call(Functor),
    send_message(Text,
        Message.get(message).get(message_id),
        Message.get(message).get(chat).get(id)),
    atomics_to_string([Command|Args], " ", Log),
    log_print(Message, Log).

router(ping, _, []) :- !.
router(ping, Message, Usernames) :-
    atomics_to_string(Usernames, " ", Text),
    send_message(Text,
        Message.get(message).get(message_id),
        Message.get(message).get(chat).get(id)),
    atomics_to_string(["Ping by",
                       Message.get(message).get(from).get(username),
                       "to", Text], " ", Log),
    log_print(Message, Log),
    !.

process_message(Message) :-
    findall(Username, get_ping_match(Message, Username), Usernames),
    list_to_set(Usernames, UsernamesUniq),
    router(ping, Message, UsernamesUniq).

process_message(Message) :-
    is_command(Message),
    router(command, Message),
    !.

process_message(_).

main :-
    repeat,
    get_updates(Data),
    Messages = Data.get(result),
    last(Messages, LastMessage),
    update_offset(LastMessage),
    maplist(process_message, Messages),
    fail.

?- main.
