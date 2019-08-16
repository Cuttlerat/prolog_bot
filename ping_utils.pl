
get_ping_match(Message, Username) :-
    string_lower(Message.get(message).get(text), Text),
    consult(pingers),
    ping_phrase(PingPhrase),
    split_string(Text, " ", " ", TextSplitted),
    member(PingPhrase, TextSplitted),
    ping_match(ChatID, Username, Match),
    Message.get(message).get(chat).get(id) = ChatID,
    member(Match, TextSplitted).
