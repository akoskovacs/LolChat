-module(chat_server).
-compile(export_all).
-include("chat.hrl").

start() ->
    start(?DEFAULT_PORT).

start(Port) ->
    case global:whereis_name(?MODULE) of
        undefined ->
            {ok, Pid, LSock} = start_acceptor(Port),
            Fun = fun() ->
                global:register_name(?MODULE, self()), ?MODULE:loop(Pid, LSock)
            end,
            spawn(Fun),
            chat_adm:start();
        _         ->  {error, already_started}
    end.

start_listener(Port, Pid) ->
    {ok, Listener} = gen_tcp:listen(Port, ?LISTEN_OPTIONS),
    Pid ! {ok, Listener},
    ?MODULE:acceptor(Listener).

start_acceptor(Port) ->
    P = spawn_link(?MODULE, start_listener, [Port, self()]),
    receive
        {ok, LSock} -> {ok, P, LSock}
    end.

%% Interface %%

broadcast_msg(LUser, Message) ->
    send_msg({broadcast_msg, LUser, Message}).

broadcast_login(LUser) ->
    send_msg({broadcast_login, LUser}).

broadcast_logout(LUser) ->
    send_msg({broadcast_logout, LUser}).

broadcast_away(LUser) ->
    send_msg({broadcast_away, LUser}).

broadcast_wake(LUser) ->
    send_msg({broadcast_wake, LUser}).

stop() ->
    send_msg(stop).

%% Private %%

send_msg_recv(Message) ->
    send_msg(Message),
    receive
        Answer -> Answer
    end.

send_msg(Message) ->
    case global:whereis_name(?MODULE) of
        undefined -> self() ! {error, not_started};
        Pid       -> Pid    ! Message 
    end.

do_broadcast(LUser, Message) ->
    F = fun(RUser) ->
        case LUser /= RUser of
            true -> gen_tcp:send(RUser#logged_user.socket, lists:flatten(Message));
            _    -> ok
        end
    end,
    lists:foreach(F, chat_adm:get_logged_in()).

do_broadcast_msg(LUser, Message) ->
    do_broadcast(LUser, io_lib:format("MSG ~s ~s~n", [LUser#logged_user.name, Message])),
    gen_tcp:send(LUser#logged_user.socket, "MSG_SENT\n").

do_broadcast_simple(LUser, Cmd) ->
    do_broadcast(LUser, io_lib:format("~s ~s~n", [Cmd, LUser#logged_user.name])).

do_broadcast_login(LUser) ->
    do_broadcast_simple(LUser, "LOGIN").

do_broadcast_logout(LUser) ->
    do_broadcast_simple(LUser, "LOGOUT").

do_broadcast_away(LUser) ->
    do_broadcast_simple(LUser, "AWAY").

do_broadcast_wake(LUser) ->
    do_broadcast_simple(LUser, "WAKE").

loop(APid, Listener) ->
    receive
        stop -> global:unregister_name(?MODULE),
            gen_tcp:close(Listener), exit(APid, exit), chat_adm:stop();

        {broadcast_msg, LUser, Message} ->
            ?MODULE:do_broadcast_msg(LUser, Message), ?MODULE:loop(APid, Listener);

        {broadcast_login, LUser} ->
            ?MODULE:do_broadcast_login(LUser), ?MODULE:loop(APid, Listener);

        {broadcast_logout, LUser} ->
            ?MODULE:do_broadcast_logout(LUser), ?MODULE:loop(APid, Listener);

        {broadcast_away, LUser} ->
            ?MODULE:do_broadcast_away(LUser), ?MODULE:loop(APid, Listener);

        {broadcast_wake, LUser} ->
            ?MODULE:do_broadcast_wake(LUser), ?MODULE:loop(APid, Listener);

        Other -> error_logger:error_msg("~p~n", [Other]), ?MODULE:loop(APid, Listener)
    end.

acceptor(Listener) ->
    case gen_tcp:accept(Listener) of
        {ok, Socket} -> 
            {ok, Pid} = client_handler:start(Socket),   
            gen_tcp:controlling_process(Socket, Pid), ?MODULE:acceptor(Listener);

        {error, closed} -> 
            error_logger:info_msg("Remote client closed connection~n"), ?MODULE:acceptor(Listener)
    end.

