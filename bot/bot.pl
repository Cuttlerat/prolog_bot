:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(clpfd)).

?- consult('conf/token').
?- consult('conf/config').
?- consult('db/pingers').
?- consult(utils).

send_message_(URL, Data, Response) :-
    catch(
        http_post(URL, Data, Response, []),
        error(_, context(_, status(ErrorCode, Response))),
        (
            format(string(Log), "Couldn't send message: ~w - ~w", [ErrorCode, Response]),
            log_print(log_level('ERROR'), Log)
        )
    ).

send_message(Text, MessageID, ChatID) :-
    string(Text),
    send_message(reply(Text), MessageID, ChatID),
    !.

send_message(reply(Text), MessageID, ChatID) :-
    url("sendMessage", URL),
    replace_emoji(Text, EmojiText),
    Data = form_data([
        text = EmojiText,
        reply_to_message_id = MessageID,
        chat_id = ChatID]),
    send_message_(URL, Data, _),
    !.

send_message(no_reply(html(Text)), _, ChatID) :-
    url("sendMessage", URL),
    replace_emoji(Text, EmojiText),
    Data = form_data([
       text = EmojiText,
       chat_id = ChatID,
       parse_mode = "HTML"]),
    send_message_(URL, Data, _),
    !.

send_message(no_reply(Text), _, ChatID) :-
    url("sendMessage", URL),
    replace_emoji(Text, EmojiText),
    Data = form_data([
        text = EmojiText,
        chat_id = ChatID]),
    send_message_(URL, Data, _),
    !.

send_message(location(Lat, Lon), MessageID, ChatID) :-
    url("sendLocation", URL),
    Data = form_data([
        latitude = Lat,
        longitude = Lon,
        reply_to_message_id = MessageID,
        chat_id = ChatID]),
    send_message_(URL, Data, _).

bot_command(Message) :-
    text_to_command(Message.get(message).get(text), Command, Args),
    atomic_concat("telegram_command_", Command, BotCommand),
    Functor =.. [BotCommand, Args, Message, Output],
    consult(commands),
    current_predicate(_, Functor),
    call(Functor),
    send_message(Output,
        Message.get(message).get(message_id),
        Message.get(message).get(chat).get(id)),
    atomics_to_string([BotCommand|Args], " ", Log),
    log_print(log_level('INFO'), Log).

ping(_, []) :- !.

ping(Message, Usernames) :-
    atomics_to_string(Usernames, " ", Text),
    send_message(reply(Text),
        Message.get(message).get(message_id),
        Message.get(message).get(chat).get(id)),
    atomics_to_string(["Ping by",
         Message.get(message).get(from).get(username),
         "to", Text], " ", Log),
    log_print(log_level('INFO'), Log),
    !.

process_message(Message) :-
    _ = Message.get(message).get(forward_from),
    !.

process_message(Message) :-
    is_command(Message),
    bot_command(Message),
    !.

process_message(Message) :-
    findall(Username, get_ping_match(Message, Username), Usernames),
    list_to_set(Usernames, UsernamesUniq),
    \+ UsernamesUniq = [],
    ping(Message, UsernamesUniq),
    !.

process_message(_).


main :-
    log_print(log_level('INFO'), "Started"),
    fail.

main :-
    update_method("polling"),
    !,
    set_webhook(""),
    repeat,
    get_updates(Data),
    Messages = Data.get(result),
    last(Messages, LastMessage),
    update_offset(LastMessage),
    maplist(process_message, Messages),
    fail.

main :-
    update_method("webhook"),
    !,
    consult(webhook),
    webhook.

?- main.
