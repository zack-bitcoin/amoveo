-module(db).
-export([test/0, save/2, read/1]).
save(F, X) -> file:write_file(F, term_to_binary(X)).
read(F) ->
    case file:read_file(F) of
        {ok, Out} -> binary_to_term(Out);
        {error, enoent} -> 
            %io:fwrite("file does not exist\n"),
            "";
        {error, Reason} -> Reason
    end.
-record(d, {a = "", b = "" }).
test() ->
    X = #d{a=[1, 2, <<"abc">>, []], b = <<1,2,3,200>> },
    File = constants:database(),
    %File = "database.db",
    save(File, X),
    X = read(File),
    success.

