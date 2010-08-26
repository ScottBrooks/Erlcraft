-module(erlcraft_client).

-behaviour(gen_server).


%% Gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {fsm, timer_ref, client_start, loc, player_id}).

init([FSMPid]) ->
    io:format("Started!~n", []),
    {ok, TRef} = timer:send_interval(1000, update_time),
    {ok, #state{fsm = FSMPid, timer_ref = TRef, client_start = now(), loc = undefined, player_id = undefined}}.

handle_call(_Request, _From, _State) ->
    io:format("Call: ~p~n", [_Request]),
    {reply, none, _State}.

handle_cast({move_look, X,Y,Z, S, R, P}, #state{fsm = FSMPid, loc = Location} = State) ->
    case Location of
        undefined ->
            mc_reply:fake_world(FSMPid, 32, 0, 0, 0),
            LocY = 5,
            erlcraft_client_fsm:send_packet(FSMPid, mc_reply:position_and_look(0, LocY * 32, 0, (LocY*32)-1.5, R, P));
        _ -> ok
    end,

    {noreply, State#state{loc = {X, Y, Z, S, R, P}}};
handle_cast({postion, X, Y, Z, S, _U}, #state{loc = {_, _, _, _, R, P}} = State) ->
    {noreply, State#state{loc = {X, Y, Z, S, R, P}}};
handle_cast({look, R, P, _U}, #state{loc = {X, Y, Z, S, _, _}} = State) ->
    {noreply, State#state{loc = {X, Y, Z, S, R, P}}};

handle_cast({player_id, PlayerID}, State) ->
    {noreply, State#state{player_id = PlayerID}};

handle_cast(_Request, _State) ->
    io:format("Cast: ~p~n", [_Request]),
    {noreply, _State}.


handle_info(update_time, #state{fsm = FSMPid, client_start = ClientStart} = State) ->
    Timestamp = trunc(timer:now_diff(now(), ClientStart)/1000000)*20,

    erlcraft_client_fsm:send_packet(FSMPid, mc_reply:timestamp(Timestamp)),
    
    {noreply, State};

handle_info(_Info, _State) ->
    io:format("Info: ~p~n", [_Info]),
    {noreply, _State}.

terminate(_Reason, _State) ->
    io:format("Term: ~p~n", [_Reason]),
    normal.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
