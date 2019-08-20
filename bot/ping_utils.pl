
get_ping_match(Message, Username) :-
    string_lower(Message.get(message).get(text), Text),
    consult(pingers),
    ping_phrase(PingPhrase),
    strip_chars(Text, [",", ".", ";", ":", "!", "?"], TextStripped),
    split_string(TextStripped, " ", " ", TextSplitted),
    member(PingPhrase, TextSplitted),
    ping_match(ChatID, Username, Match),
    Message.get(message).get(chat).get(id) = ChatID,
    member(Match, TextSplitted).

strip_chars(X, [], X) :- !.
strip_chars(String, [H|Exclude], Result) :-
    split_string(String, H, "", StrippedList),
    atomics_to_string(StrippedList, Result1),
    strip_chars(Result1, Exclude, Result).

