-module(userboot).
-compile([export_all]).

start() ->
    erlang:display(hello),

    '9p_server':add_listener(global, '9p_tcp', {{0,0,0,0}, 564}),
    '9p_server':add_export(<<"proc">>, procfs, []).
