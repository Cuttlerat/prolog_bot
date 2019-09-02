?- consult(utils).

%
%! telegram_command_*(+Args:list, -Output:string) is det.
% @arg Args is list of strings
%

% Info command
% @arg Output is info about the bot

telegram_command_info(_, _, "OMG, I work somehow!\nPlease star me: https://github.com/Cuttlerat/prolog_bot").

% Date command
% @arg Args is [] or [TimeStamp]
% @arg TimeStamp is a UNIX timestamp
% @arg Output date in HTTP format
% If Args is empty, then current time will be taken

telegram_command_date([], _, Date) :-
    get_time(TimeStamp),
    get_date(TimeStamp, Date).

telegram_command_date([TimeStampStr], _, Date) :-
    number_string(TimeStamp, TimeStampStr),
    get_date(TimeStamp, Date).

% Factorial command
% @arg Args [Number]
% @arg Number is string of int
% @arg Output is factorial of the Number

telegram_command_factorial([Input], _, Output) :-
    number_string(XNumber, Input),
    Number is round(XNumber),
    % Telegram cannot handle factorial of the number greater than 1494 in one message
    between(0, 1494, Number),
    factorial(Number, Factorial),
    number_string(Factorial, Output),
    !.

telegram_command_factorial(_, _, "false.").

% Ping show command
% @arg Args is a [] or [Username]
% @arg Username is a username which matches should be shown
% @arg Output is a message with matches or a message that there no matches

telegram_command_ping_show([], Message, Output) :-
    telegram_command_ping_show_(
        Message.get(message).get(from).get(username),
        Message,
        Output
    ),
    !.

telegram_command_ping_show([Username], Message, Output) :-
    telegram_command_ping_show_(Username, Message, Output),
    !.

telegram_command_ping_show(_, _, "Please enter only one username").

telegram_command_ping_show_(UsernameArg, Message, Output) :-
    ( string_concat("@", _, UsernameArg)
    -> Username = UsernameArg
    ; string_concat("@", UsernameArg, Username)
    ),
    ChatID = Message.get(message).get(chat).get(id),
    findall(Match, ping_match(ChatID, Username, _, Match), Matches),
    ( Matches = []
    -> string_concat("No matches for ", Username, Output)
    ; atomics_to_string(Matches, "\n", Output)
    ),
    !.

% Ping add command
% @arg Args is a list of strings with matches
% @arg Output is a message with added matches or a message that there nothing to add

telegram_command_ping_add(Matches, Message, Output) :-
    string_concat("@", Message.get(message).get(from).get(username), Username),
    ChatID = Message.get(message).get(chat).get(id),
    maplist(unify_match, Matches, UnifiedMatches),
    exclude(ping_match(ChatID, Username, _), UnifiedMatches, NewMatches),
    maplist(assert_match(ChatID, Username), NewMatches),
    save_matches,
    ( NewMatches = []
    -> Output = "Nothing to add"
    ; atomics_to_string(["Added matches:"|NewMatches], "\n", Output)
    ).

% Ping delete command
% @arg Args is a list of strings with matches
% @arg Output is a message with deleted matches or a message that there nothing to delete

telegram_command_ping_delete(Matches, Message, Output) :-
    string_concat("@", Message.get(message).get(from).get(username), Username),
    ChatID = Message.get(message).get(chat).get(id),
    maplist(unify_match, Matches, UnifiedMatches),
    include(ping_match(ChatID, Username, _), UnifiedMatches, FoundMatches),
    maplist(retract_match(ChatID, Username), FoundMatches),
    save_matches,
    ( FoundMatches = []
    -> Output = "Nothing to delete"
    ; atomics_to_string(["Deleted matches:"|FoundMatches], "\n", Output)
    ).

% Location command
% @arg Args is a list of strings with a human readable location
% @arg Location is a location(Lat, Lon)

telegram_command_location([], _, Output) :-
    Output = "Please enter location like /location Moscow",
    !.

telegram_command_location(Args, _, Location) :-
    atomics_to_string(Args, " ", TextLocation),
    get_locations_from_yandex(TextLocation, Locations),
    Results = Locations.get(response).get('GeoObjectCollection').get(featureMember),
    Results = [FirstResult|_],
    Point = FirstResult.get('GeoObject').get('Point').get(pos),
    atomics_to_string(Coordinates, " ", Point),
    Coordinates = [Lon, Lat],
    Location = location(Lat, Lon),
    !.

telegram_command_location(Args, _, Output) :-
    atomics_to_string(["Could not find location: "|Args], " ", Output).

% Set me command
% @arg Args is a list of strings with a username 'me' name
% This command saves a string from Args as a 'me' name in database
% 'me' name uses for telegram_command_me

telegram_command_set_me([], _, "Please define your name.") :- !.

telegram_command_set_me(Args, Message, Output) :-
    UserID = Message.get(message).get(from).get(id),
    ChatID = Message.get(message).get(chat).get(id),
    atomics_to_string(Args, " ", Match),
    update_me(ChatID, UserID, Match),
    format(string(Output), "Now I'll call you ~s", [Match]).

% Me command
% @arg Args is a list of strings with a text
% This command replaces the user's message with a 'me' name + text from Args

telegram_command_me(Args, Message, no_reply(Output)) :-
    UserID = Message.get(message).get(from).get(id),
    ChatID = Message.get(message).get(chat).get(id),
    consult('db/pingers'),
    me(ChatID, UserID, Match),
    capitalize(Match, CapitalizedMatch),
    delete_message(Message),
    atomics_to_string([CapitalizedMatch|Args], " ", Output),
    !.

telegram_command_me(_, _, "Please add a 'me' cast for yourself by /set_me command").
