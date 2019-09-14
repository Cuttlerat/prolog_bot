:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).

:- http_handler(root(.), process_request, []).

?- consult('db/pingers').
?- consult(utils).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

process_request(Request) :-
    member(method(post), Request),
    http_read_json_dict(Request, Data),
    reply_json(_{}),
    set_output(user_output),
    process_message(Data),
    !.

webhook :-
    webhook_url(WebhookURL),
    server(80),
    set_webhook(WebhookURL),
    thread_get_message(stop).
