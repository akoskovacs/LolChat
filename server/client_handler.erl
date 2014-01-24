-module(client_handler).
-compile(export_all).
-include("chat.hrl").

start(Socket) ->  
    Pid = spawn(
        fun() -> 
            gen_tcp:send(Socket, lists:flatten(io_lib:format("HAI ME ~s ~s~n",
                [?SERVER_NAME, ?SERVER_VERSION]))),
            ?MODULE:loop(#logged_user{name=undefined, socket=Socket})
        end), {ok, Pid}.

generate_peer_list([], Acc) ->
    string:concat(string:strip(Acc), "\n");

generate_peer_list([User|Rest], Acc) ->
    Name = lists:flatten(io_lib:format("~s ", [User#logged_user.name])),
    NName = case User#logged_user.is_away of
        true -> string:concat("*", Name);
        _    -> Name
    end,
    ?MODULE:generate_peer_list(Rest, string:concat(Acc, NName)).

generate_peer_list(UserList) ->
    generate_peer_list(UserList, "PEERS ").

send_peer_list(User) ->
    LoggedUsers = chat_adm:get_logged_in(),
    gen_tcp:send(User#logged_user.socket, ?MODULE:generate_peer_list(LoggedUsers)),
    User.

handle_auth([UserName|Rest], User) ->
    [Password|_] = Rest,
    Socket = User#logged_user.socket,
    case chat_adm:login(UserName, Password, Socket) of
        {error, _} -> error_logger:info_msg("Bad login for ~p~n", [UserName]),
                      gen_tcp:send(Socket, "U_SHALL_NOT_PASS\n"), User;

        ok         -> U = User#logged_user{name=UserName},
                      gen_tcp:send(Socket, "U_IN\n"), chat_server:broadcast_login(U),
                      ?MODULE:send_peer_list(U), U
    end;

handle_auth(_, User) ->
    gen_tcp:send(User#logged_user.socket, "U_SHALL_NOT_PASS\n"), User.

check_creditals(User) ->
    case User#logged_user.name of
        undefined -> gen_tcp:send(User#logged_user.socket, "U_NO_LOGIN?\n"), false;
        _         -> true
    end.

handle_peers(User) ->
    case ?MODULE:check_creditals(User) of
        true -> ?MODULE:send_peer_list(User);
        _ -> pass
    end, User.

handle_away(User) ->
    case ?MODULE:check_creditals(User) of 
        true  -> U = User#logged_user{is_away=true},
                 chat_adm:set_user_awayness(User#logged_user.name, true),
                 chat_server:broadcast_away(U), U;
        false -> User
    end.

handle_wake(User) ->
    case ?MODULE:check_creditals(User) of 
        true  -> U = User#logged_user{is_away=false},
                 chat_adm:set_user_awayness(User#logged_user.name, false),
                 chat_server:broadcast_wake(U), U;
        false -> User
    end.

handle_msg(Message, User) ->
    case ?MODULE:check_creditals(User) of
        true  -> chat_server:broadcast_msg(User, string:join(Message, " ")), User;
        false -> User
    end.

handle_commands([], User) ->
    User;

handle_commands([Command|Rest], User) ->
    case Command of
        "AUTH"   -> ?MODULE:handle_auth(Rest, User);
        "EXIT"   -> self() ! stop, User;
        "CLOSE"  -> self() ! stop, User;
        "LOGOUT" -> self() ! stop, User;
        "PEERS"  -> ?MODULE:handle_peers(User);
        "MSG"    -> ?MODULE:handle_msg(Rest, User);
        "AWAY"   -> ?MODULE:handle_away(User);
        "WAKE_UP"-> ?MODULE:handle_wake(User);
        "HELP"   ->  gen_tcp:send(User#logged_user.socket, "AUTH EXIT CLOSE LOGOUT PEERS MSG AWAY WAKE_UP HELP\n"), User;
        _        -> gen_tcp:send(User#logged_user.socket, "MEH?\n"), User
    end.

parse_lines([], User) ->
    User;

parse_lines([Line|_], User) ->
   Toks = string:tokens(Line, " "),
   ?MODULE:handle_commands(Toks, User).

handle_data(User, Data) ->
    Lines = string:tokens(Data, "\n\r"),
    ?MODULE:parse_lines(Lines, User).

end_connection(User) ->
    case User#logged_user.name of
        undefined -> pass;
        Name      -> chat_server:broadcast_logout(User), chat_adm:logout(Name)
    end,
    gen_tcp:close(User#logged_user.socket).

loop(User) ->
    receive
        stop -> ?MODULE:end_connection(User);
        {tcp, _Socket, Data}  -> ?MODULE:loop(?MODULE:handle_data(User, Data));
        {tcp_closed, _Socket} -> ?MODULE:end_connection(User);
        Other -> io:format("~p~n", [Other]), ?MODULE:loop(User)
    end.
