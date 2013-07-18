-module(netstat).
%-author('proger@hackndev.com').
-compile([export_all]).

-record(sock, {port, local, peer, stat, proc}).

retinfo({State, Term}) when is_atom(State) ->
    Term.

sockinfo(Port, PortInfo) ->
    Proc = proplists:get_value(connected, PortInfo),
    #sock{
        port=Port,
        local=retinfo(inet:sockname(Port)),
        peer=retinfo(inet:peername(Port)),
        stat=retinfo(inet:getstat(Port)), 
        proc=[{pid, Proc}|erlang:process_info(Proc, [registered_name,current_function,initial_call])]
    }.

ports(MatchStr) ->
    [T || {_Port, [{name, N}|_]} = T <- [{Port, erlang:port_info(Port)} || Port <- erlang:ports()],
             N =:= MatchStr].

tcp() ->
    [sockinfo(P, Pi) || {P, Pi}<- ports("tcp_inet")].

fmt(F, A) -> lists:flatten(io_lib:format(F, A)).

format_peer({{A,B,C,D}, P}) -> fmt("~p.~p.~p.~p:~p", [A,B,C,D,P]);
format_peer(Term) -> fmt("~p", [Term]).

format_proc([{pid, P}, {registered_name, []}, {current_function, {M,F,A}} | _Px]) ->
        fmt("~10w ~w:~w/~w", [P, M, F, A]);
format_proc([{pid, P}, {registered_name, Name}, {current_function, {_,_,_}} | _Px]) ->
        fmt("~10w ~s", [P, Name]).

format_stat(L) when is_list(L) ->
    RB = proplists:get_value(recv_oct, L),
    SB = proplists:get_value(send_oct, L),
    SPB = proplists:get_value(send_pend, L),
    fmt("i ~7B o ~7B obuf ~7B", [RB, SB, SPB]).

format_tcp(#sock{port=Port, local=Local, peer=Peer, stat=Stat, proc=Proc}) ->
    fmt("~-13s ~21s ~-21s ~-40s ~s",
            [erlang:port_to_list(Port),
                format_peer(Local),
                format_peer(Peer),
                format_proc(Proc),
                format_stat(Stat)]).

tcpi() ->
    L = [io:format("~s~n", [format_tcp(S)]) || S <- tcp()],
    {ok, length(L)}.

tcpi(Interval) when is_integer(Interval) ->
    {ok, L} = tcpi(),
    L > 1 andalso io:format("~n"),
    timer:sleep(Interval),
    tcpi(Interval);

tcpi(_) ->
    tcpi(1000).
