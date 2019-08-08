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

is_command(Message) :-
    [Entity|_] = Message.get(message).get(entities),
    Entity.get(type) = "bot_command".

command_to_name(Command, Name) :-
    atomic_concat("telegram_command_", Command, Name).

text_to_command(Message, Command, Args) :-
    split_string(Message, " ", "", [CommandTmp|Args]),
    split_string(CommandTmp, "@", "/", [Command|_]).

construct_functor(Name, Args, Functor) :-
    length(Args, Arity),
    functor(Functor, Name, Arity),
    construct_functor_(Name, Args, Functor, Arity, 0).

construct_functor_(_, [], _, X, X) :- !.
construct_functor_(Name, [Arg|Args], Functor, Arity, ArgNumber) :-
    ArgNumber1 #= ArgNumber + 1,
    arg(ArgNumber1, Functor, Arg),
    construct_functor_(Name, Args, Functor, Arity, ArgNumber1).

log_print(ChatID, Command) :-
    format('[~d]: ~s~n', [ChatID, Command]).

router(Message) :-
    ChatID = Message.get(message).get(chat).get(id),
    text_to_command(Message.get(message).get(text), Command, Args),
    command_to_name(Command, Name),
    construct_functor(Name, [Args, Text], Functor),
    consult(commands),
    current_predicate(_, Functor),
    call(Functor),
    url("sendMessage", URL),
    http_post(URL, form_data([ text = Text,
                               chat_id = ChatID ]), _, []),
    log_print(ChatID, Command),
    !.
router(_).

process_messages :-
    get_updates(Data),
    include(is_command, Data.get(result), Commands),
    maplist(router, Commands),
    (
        last(Data.get(result), Last),
        update_offset(Last)
    ;
        true
    ),
    process_messages.

%?- process_messages.
