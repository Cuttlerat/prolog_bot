:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(clpfd)).

?- consult(token).

url_concat([X], X) :- !.
url_concat([H1, H2|T], Output) :-
    string_concat(H1, H2, H3),
    url_concat([H3|T], Output).

url(Command, URL) :-
    token(Token),
    url_concat(["https://api.telegram.org/bot", Token, "/", Command], URL).

get_updates(Data) :-
    url("getUpdates", URL),
    setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json')]),
        json_read_dict(In, Data),
        close(In)
    ).

update_offset(Message) :-
    url("getUpdates", URL),
    NewOffset #= Message.get(update_id) + 1,
    http_post(URL, form_data([offset = NewOffset]), _, []).

get_commands([], []) :- !.
get_commands([Message|Tail], [Message|OutputTail]) :-
    [Entity|_] = Message.get(message).get(entities),
    Entity.get(type) = "bot_command",
    update_offset(Message),
    get_commands(Tail, OutputTail),
    !.
get_commands([_|Tail], Output) :-
    get_commands(Tail, Output).

router([]) :- !.
router([Message|Tail]) :-
    ChatID = Message.get(message).get(chat).get(id),
    Command = Message.get(message).get(text),
    url("sendMessage", URL),
    string_concat(Command, " is not implemented yet.", Text),
    http_post(URL, form_data([ text = Text,
                               chat_id = ChatID ]), _, []),
    format('[~d]: ~s~n', [ChatID, Command]),
    router(Tail).

process_messages() :-
    get_updates(Data),
    get_commands(Data.get(result), Commands),
    router(Commands),
    process_messages().

%?- process_messages().
