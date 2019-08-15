
get_ping_match(Message, Username) :-
    string_lower(Message.get(message).get(text), Text),
    consult(pingers),
    ping_phrase(PingPhrase),
    sub_string(Text, _, _, _, PingPhrase),
    ping_match(ChatID, Username, Match),
    Message.get(message).get(chat).get(id) = ChatID,
    sub_string(Text, _, _, _, Match).


