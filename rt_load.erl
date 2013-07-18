-module(rt_load).
-compile([export_all]).

read_file(Name) ->
    {ok, Bin} = file:read_file(Name),
    FL = binary:bin_to_list(Bin),
    load_source(FL).

load_source(Str) ->
    {ok, Terms, _} = erl_scan:string(Str),
    SplitTerms = split_terms(Terms),
    Forms = [case erl_parse:parse_form(T) of {ok , F} -> F end || T <- SplitTerms],

    {ok, ModName, ModBin} = compile:forms(Forms),
    code:load_binary(ModName, "manual_load", ModBin).

split_terms(Terms) ->
    split_terms(Terms, []).

split_terms([], Acc) ->
    lists:reverse(Acc);
split_terms(Terms, Acc) ->
    {FirstTerms, [Dot|Rem]} = lists:splitwith(fun
            ({dot, _}) -> false;
            (_) -> true
        end, Terms),
    Term1 = FirstTerms ++ [Dot],
    split_terms(Rem, [Term1|Acc]).
