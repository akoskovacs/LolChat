-define(DEFAULT_PORT, 4625).
-define(LISTEN_OPTIONS, [list, {active, true}]).
-define(SERVER_NAME, "LolCatChat").
-define(SERVER_VERSION, "v0.1").
-define(PASSWD_FILE, "./passwd.txt").

-record(user, {name, password}).
-record(logged_user, {name, is_away=false, socket}).
-record(log, {regs=[], logged_in=[]}).
