
telegram_command_info(_, "OMG, I work somehow! Please star me: https://github.com/Cuttlerat/prolog_bot").

telegram_command_date([], Date) :-
    get_time(TimeStamp),
    get_date(TimeStamp, Date).

telegram_command_date([TimeStampStr], Date) :-
    number_string(TimeStamp, TimeStampStr),
    get_date(TimeStamp, Date).

get_date(TimeStamp, Date) :-
    use_module(library(http/http_header)),
    http_timestamp(TimeStamp, Atom),
    atom_string(Atom, Date).
