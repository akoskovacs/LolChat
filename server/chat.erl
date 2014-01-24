-module(chat).
-include("chat.hrl").

-export([start/0, start/1]).
-export([stop/0]).

start() ->
    start(?DEFAULT_PORT).

start(Port) ->
    S = chat_server:start(Port),
    error_logger:info_msg("~s started...~nListening on ~p~n"
        , [?SERVER_NAME, Port]),
    chat_adm:load_passwd(), S.


stop() -> 
    chat_server:stop(),
    chat_adm:stop(), ok.
