-module(chat_adm).
-compile(export_all).
-include("chat.hrl").

%%% Interface %%%

start() ->
    case global:whereis_name(?MODULE) of
        undefined ->
            Pid = spawn(fun() -> ?MODULE:loop(#log{}) end),
            global:register_name(?MODULE, Pid), ok;
        _         ->  {error, already_started}
    end.

stop() ->
    ?MODULE:send_msg(stop).

register_user(Name, Password) ->
    ?MODULE:send_msg_recv({register_user, Name, Password, self()}).

rename_user(OldName, NewName) ->
    ?MODULE:send_msg_recv({rename_user, OldName, NewName, self()}).

set_user_awayness(Name, IsAway) ->
    ?MODULE:send_msg_recv({user_away, Name, IsAway, self()}).

get_registered() ->
    ?MODULE:send_msg_recv({get_registered, self()}).

get_logged_in() ->
    ?MODULE:send_msg_recv({get_logged_in, self()}).

login(Name, Password, Socket) ->
    ?MODULE:send_msg_recv({login_user, Name, Password, Socket, self()}).

logout(Name) -> 
    ?MODULE:send_msg_recv({logout_user, Name, self()}).

load_user_file(FileName) ->
    ?MODULE:send_msg_recv({load_user_file, FileName, self()}).

load_passwd() ->
    load_user_file(?PASSWD_FILE).

ping() ->
    ?MODULE:send_msg_recv({ping, self()}).

%%% Private %%%

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

user_state(UserName, #log{regs=RegList, logged_in=LoggedList}) ->
    case lists:keyfind(UserName, #logged_user.name, LoggedList) of
        false ->
            case lists:keyfind(UserName, #user.name, RegList) of
                false -> {error, not_exist};
                User  -> {logged_out, User}
            end;
        User -> {logged_in, User}
    end.

check_password(Name, Password, Log) ->
    case ?MODULE:user_state(Name, Log) of
        {logged_out, User} ->
            case (User#user.password == Password) of
                true -> ok;
                _    -> {error, badpass}
            end;
        {logged_in, _} -> {error, logged_in};
        Error          -> Error
    end.

rename_user(OldName, NewName, Log=#log{logged_in=Logged}) ->
    case ?MODULE:user_state(OldName, Log) of
        {logged_in, User} ->
            case lists:keyreplace(OldName
                    , #logged_user.name, Logged, User#logged_user{name=NewName}) of
                false -> {error, no_user};
                List -> {ok, List}
            end;
        {error, E} -> {error, E};
        {Error, _} -> {error, Error}
    end.
            
remove_user(UserName, UserList) ->
    case lists:keytake(UserName, #user.name, UserList) of
        false -> UserList;
        {value, _, NewList} -> NewList
    end.

load_by_line(Handle) ->
    case file:read_line(Handle) of
        {ok, Line} ->
            [UserName|[Password|_]] = string:tokens(Line, " \n"),
            ?MODULE:register_user(UserName, Password),
            load_by_line(Handle);
        eof -> eof;
        {error, Error} -> 
            error_logger:error("Error while reading file ~p~n", [Error])
     end.
    

load_user_file(FileName, Pid) ->
    case file:open(FileName, [read]) of
        {error, Error} -> Pid ! {error, Error};
        {ok, Handle}   -> Pid ! ok, load_by_line(Handle)
    end.

do_set_awayness(User, IsAway, Log=#log{logged_in=LoggedIn}) ->
    F = fun(U) ->
        case U == User of
            true -> U#logged_user{is_away=IsAway};
            _    -> U
        end
    end,
    Log#log{logged_in=lists:map(F, LoggedIn)}.

loop(Log = #log{regs=Regs, logged_in=LoggedIn}) ->
    receive
        stop  -> global:unregister_name(?MODULE), ok;
        {register_user, Name, Password, Pid} ->
            case ?MODULE:user_state(Name, Log) of
               {error, not_exist} -> Pid ! ok,
                    ?MODULE:loop(Log#log{regs=[#user{name=Name, password=Password}|Regs]});
               _                  -> Pid ! {error, registered}, ?MODULE:loop(Log)
            end;

        {rename_user, OldName, NewName, Pid} ->
            case ?MODULE:rename_user(OldName, NewName, Log) of
                {ok, NewLogged} ->
                    Pid ! ok, ?MODULE:loop(Log#log{logged_in=NewLogged});
                Error           -> Pid ! Error, ?MODULE:loop(Log)
            end;

        {login_user, UserName, Password, Socket, Pid} ->
            case ?MODULE:check_password(UserName, Password, Log) of
                ok    -> Pid ! ok, ?MODULE:loop(Log#log{logged_in=[#logged_user{name=UserName, socket=Socket}|LoggedIn]});
                Error -> Pid ! Error, ?MODULE:loop(Log)
            end;

        {logout_user, UserName, Pid} ->
            case ?MODULE:user_state(UserName, Log) of
                {logged_in, User}  -> Pid ! ok, ?MODULE:loop(Log#log{logged_in=remove_user(User#logged_user.name, LoggedIn)});
                {logged_out,   _}  -> Pid ! {error, already_logged_out}, ?MODULE:loop(Log);
                {error, not_exist} -> Pid ! {error, not_exist}, ?MODULE:loop(Log)
            end;

        {ping, Pid} -> 
            Pid ! pong, ?MODULE:loop(Log);

        {user_away, Name, IsAway, Pid} ->
            case ?MODULE:user_state(Name, Log) of
                {logged_in, User}  -> Pid ! ok, ?MODULE:loop(?MODULE:do_set_awayness(User, IsAway, Log));
                {logged_out,   _}  -> Pid ! {error, logged_out}, ?MODULE:loop(Log);
                {error, not_exist} -> Pid ! {error, not_exist}, ?MODULE:loop(Log)
            end;

        {load_user_file, FileName, Pid} ->
            spawn(fun() -> load_user_file(FileName, Pid) end), ?MODULE:loop(Log);

        {get_logged_in,  Pid} -> Pid ! LoggedIn, ?MODULE:loop(Log);
        {get_registered, Pid} -> Pid ! Regs, ?MODULE:loop(Log)
    end.
