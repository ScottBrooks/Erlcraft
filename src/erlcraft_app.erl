%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the erlcraft application.

-module(erlcraft_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1, init/1]).

%% Internal API
-export([start_client/0]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).


%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
    supervisor:start_child(tcp_client_sup, []).

ensure_tables_created() ->
    ok.

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erlcraft.
start(_Type, _StartArgs) ->
    erlcraft_deps:ensure(),
    mnesia:start(),
    ensure_tables_created(),
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, erlcraft_client_fsm]).

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erlcraft.
stop(_State) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Listener
              {   tcp_server_sup,                          % Id       = internal id
                  {tcp_listener,start_link,[Port,Module]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [tcp_listener]                           % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   tcp_client_sup,
                  {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [Module]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.
%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
