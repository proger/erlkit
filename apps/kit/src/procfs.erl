-module(procfs).

%% Callbacks
-export([list_dir/2,find/3]).
-export([write/5,read/5]).
-export([truncate/4]).
-export([top_stat/2,file_stat/3]).

%% returned/consumed by stat/wstat operations
-record(stat, {ver =e,  %% 9P2000.e or 9P2000.u
                           type =0,     %% for kernel use?
                           dev =0,      %% for kernel use?
                           qid,
                           mode,
                           atime,
                           mtime,
                           length,
                           name,
                           uid = <<>>,
                           gid = <<>>,
                           muid = <<>>,
                           ext = <<>>,
                           num_uid =0,
                           num_gid =0,
                           num_muid =0}).

%% -spec top_granted(binary() | {binary(),integer()} | undefined, any(), any()) -> boolean().
%%
%% top_granted(User, Conn, Bucket) -> true.
%%
%% -spec file_granted(binary(), binary() | {binary(),integer()} | undefined,
%% any(), any()) -> boolean().
%%
%% file_granted(File, User, Conn, Bucket) -> true.

-spec list_dir(any(), any()) -> [{binary(),any()}].

list_dir(_Conn, _Pid) ->
    [begin PidL = erlang:iolist_to_binary(io_lib:format("~p", [P])), {PidL, PidL} end || P <- erlang:processes()].

-spec find(binary(), any(), any()) -> {found,_} | false.

find(FileName, _Conn, _Pid) ->
    case erlang:process_info(bin_to_pid(FileName)) of
        undefined -> false;
        _ -> {found, FileName}
    end.

%% -spec create(binary(), any(), any()) -> boolean().
%%
%% create(Name, Conn, ModConf) -> false.

%% -spec remove(any(), any(), any()) -> boolean().
%%
%% remove(Name, Conn, ModConf) -> false.

-spec read(any(), integer(), integer(), any(), any()) -> binary().

read(File, Offset, Count, _Conn, _Pid) ->
    Pid = bin_to_pid(File),
    Binary = erlang:iolist_to_binary(io_lib:format("~p", [erlang:process_info(Pid)])),
    {cache, Binary}.

-spec write(any(), integer(), binary(), any(), any()) -> integer().
 
write(FileName, 0, Data, _Conn, Pid) ->
    Pid = bin_to_pid(FileName),
    Msg = binary_to_term(Data),
    Pid ! Msg,
    byte_size(Data);
write(_, _, _, _, _) ->
    0.

-spec truncate(any(), integer(), any(), any()) -> integer().

truncate(_File, _Size, _Conn, _Pid) ->
        0.

%-spec top_stat(any(), any()) -> #stat{}.

top_stat(_Conn, _Pid) ->
        #stat{name = <<"sink">>,length =0}.

%-spec file_stat(any(), any(), any()) -> #stat{}.

file_stat(File, _Conn, _Pid) ->
        #stat{name =to_bin(File),length =0}.

%%------------------------------------------------------------------------------

to_bin(A) when is_atom(A) ->
        list_to_binary(atom_to_list(A));
to_bin(B) ->
        B.

bin_to_pid(Bin) ->
    erlang:list_to_pid(erlang:binary_to_list(Bin)).

%%EOF
