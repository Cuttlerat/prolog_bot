:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(clpfd)).

?- consult(token).
?- consult(utils).


send_no_reply_message(Text, ChatID) :-
    url("sendMessage", URL),
    catch(
        http_post(URL, form_data([ text = Text,
                                   chat_id = ChatID ]), _, []),
        error(_, context(_, status(ErrorCode, Response))),
        format("[ERROR] Couldn't send message w/o reply: ~w - ~w~n", [ErrorCode, Response])
    ).

send_message(location(Lat, Lon), MessageID, ChatID) :-
    send_location(location(Lat, Lon), MessageID, ChatID),
    !.

send_message(Text, MessageID, ChatID) :-
    url("sendMessage", URL),
    catch(
        http_post(URL, form_data([ text = Text,
                                   reply_to_message_id = MessageID,
                                   chat_id = ChatID ]), _, []),
        error(_, context(_, status(_, _))),
        send_no_reply_message(Text, ChatID)
    ).

send_location(location(Lat, Lon), MessageID, ChatID) :-
    url("sendLocation", URL),
    catch(
        http_post(URL, form_data([ latitude = Lat,
                                   longitude = Lon,
                                   reply_to_message_id = MessageID,
                                   chat_id = ChatID ]), _, []),
        error(_, context(_, status(ErrorCode, Response))),
        format("[ERROR] Couldn't send location: ~w - ~w~n", [ErrorCode, Response])
    ).

router(command, Message) :-
    text_to_command(Message.get(message).get(text), Command, Args),
    command_to_name(Command, Name),
    Functor =.. [Name, Args, Message, Text],
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
    is_command(Message),
    router(command, Message),
    !.

process_message(Message) :-
    findall(Username, get_ping_match(Message, Username), Usernames),
    list_to_set(Usernames, UsernamesUniq),
    router(ping, Message, UsernamesUniq),
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
