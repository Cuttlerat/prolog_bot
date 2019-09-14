:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).

:- http_handler(root(.), process_post, []).

webhook(Port) :-
    http_server(http_dispatch, [port(Port)]).

process_post(Request) :-
    member(method(post), Request),
    http_read_json_dict(Request, Data),
    Messages = Data.get(result),
    maplist(process_message, Messages),
    !.

set_webhook :-
    url("setWebhook", URL),
    webhook_url(WebhookURL),
    webhook(80),
    http_post(URL,
        form_data([
            url = WebhookURL,
        ]), _, []
    ).
