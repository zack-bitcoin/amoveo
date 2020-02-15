

-module(update_all_files).

-export([doit/0]).

doit() ->
    F = fun(X) ->
                io:fwrite("updating "),
                io:fwrite(X),
                io:fwrite("\n"),
                {ok, Txt} = file:read_file(X),

                Warning = <<"WARNING\n========\n
this is an old expired version of the documentation.\n
Please use the new documentation instead. \n
Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs \n
And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master/">>,
                {".", X2} = lists:split(1, X),
                Txt2 = <<Warning/binary, (list_to_binary(X2))/binary, ")\n\n", Txt/binary>>,
                %io:fwrite(Txt2),
                file:write_file(X, Txt2)
                %1=2,
                %ok
        end,
                
    recursive_folders(".", F).

remove_substring(Pattern, B) ->
    S = 8*size(Pattern),
    <<P:S>> = Pattern,
    remove_substring2(P, S, B, <<>>).
remove_substring2(P, S, B, T) ->
    case B of
        <<P:S, Rest/binary>> ->
            remove_substring2(P, S, Rest, T);
        <<A, Rest/binary>> ->
            remove_substring2(P, S, Rest, <<T/binary, A>>);
        <<>> -> T
    end.
            

recursive_folders(Dir, F) ->
    {ok, L} = file:list_dir(Dir),
    recursive_folders2(Dir, L, F).

recursive_folders2(_, [], _) -> ok;
recursive_folders2(Dir, [H|T], F) ->
    Loc = Dir ++ "/" ++ H,
    Length = length(Loc),
    case file:list_dir(Loc) of
        {error, enoent} ->
            io:fwrite("this should never happen.");
        {error, enotdir} -> 
            {_, Type} = lists:split(Length-3, Loc),
            case Type of
                ".md" ->
                    F(Loc);
                _ -> ok
            end;
        {ok, L} ->
            recursive_folders2(Loc, L, F)
    end,
    recursive_folders2(Dir, T, F).
           
