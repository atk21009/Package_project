%%%-------------------------------------------------------------------
%% @doc mocking public API
%% @end
%%%-------------------------------------------------------------------

-module(mocking_app).
-behaviour(gen_server).

-behaviour(application).

%% Required Exports
-export([start/2, stop/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([validate_pack_id/1, validate_loc_id/1,common_teardown/1]).



%% Start the application
start(_StartType, _StartArgs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(_State) ->
    ok.

init([]) ->
    {ok, #{}}.

handle_call({package_transfered, PackId, LocId}, _From, State) ->
    case {validate_pack_id(PackId), validate_loc_id(LocId)} of
        {true, true} -> {reply, ok, State};
        false -> {reply, fail, State}
    end;
handle_call({delivered, PackId}, _From, State) ->
    case validate_pack_id(PackId) of
        true -> {reply, ok, State};
        false -> {reply, fail, State}
    end;
handle_call({location_request, PackId}, _From, State) ->
    case validate_pack_id(PackId) of
        true -> {reply, {ok, longitude(PackId), latitude(PackId)}, State};
        false -> {reply, fail, State}
    end;
handle_call({location_update, LocId, Lon, Lat}, _From, State) ->
    case {validate_loc_id(LocId), erlang:is_number(Lon), erlang:is_number(Lat)} of
        {true, true, true} -> {reply, ok, State};
        false -> {reply, fail, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% internal functions

%% Stub functions for validation and state updates
validate_pack_id(PackId) when is_atom(PackId) -> true;
validate_pack_id(_) -> false.

validate_loc_id(LocId) when is_atom(LocId) -> true;
validate_loc_id(_) -> false.

longitude(PackId) ->
    %% Mock longitude based on PackId, replace with actual logic
    123.0.

latitude(PackId) ->
    %% Mock latitude based on PackId, replace with actual logic
    456.0.




common_teardown(_ReturnedState) ->
    %% Clean up after tests, such as unloading mock modules
    ok.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

%% Setting up the common environment for all tests
common_setup() ->
    %% Assume any necessary setup like initializing a mock state or configuring a mock database
    {ok, InitialState}.

%% Test suite for package_server functionalities
package_server_test_() ->
    {setup,
     fun() -> common_setup() end,
     fun(_) -> common_teardown() end,
     [
         package_transfer_tests(),
         delivered_tests(),
         location_request_tests(),
         location_update_tests()
     ]}.

package_transfer_tests() ->
    State = ok,
    [
        ?_assertEqual({reply, ok, State}, package_server:handle_call({package_transfered, pack_id, loc_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({package_transfered, bad_id, loc_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({package_transfered, pack_id, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({package_transfered, pack_id, undefined}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({package_transfered, bad_id, undefined}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({package_transfered, undefined, loc_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({package_transfered, undefined, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({package_transfered, undefined, undefined}, undefined, State))
    ].

delivered_tests() ->
    State = ok,
    [
        ?_assertEqual({reply, ok, State}, package_server:handle_call({delivered, pack_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({delivered, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({delivered, undefined}, undefined, State))
    ].

location_request_tests() ->
    State = ok,
    [
        ?_assertEqual({reply, {ok, 123.0, 456.0}, State}, package_server:handle_call({location_request, pack_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_request, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_request, undefined}, undefined, State))
    ].

location_update_tests() ->
    State = ok,
    [
        ?_assertEqual({reply, ok, State}, package_server:handle_call({location_update, loc_id, 123.0, 456.0}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, bad_id, 123.0, 456.0}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, loc_id, bad_id, 456.0}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, loc_id, 123.0, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, bad_id, bad_id, 456.0}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, bad_id, 123.0, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, loc_id, bad_id, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, bad_id, bad_id, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, undefined, 123.0, 456.0}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, loc_id, undefined, 456.0}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, loc_id, 123.0, undefined}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, undefined, undefined, 456.0}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, undefined, 123.0, undefined}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, loc_id, undefined, undefined}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, undefined, undefined, undefined}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, bad_id, undefined, 456.0}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, bad_id, 123.0, undefined}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, undefined, bad_id, 456.0}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, loc_id, bad_id, undefined}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, undefined, 123.0, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, loc_id, undefined, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, bad_id, undefined, undefined}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, undefined, bad_id, undefined}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, undefined, undefined, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, undefined, bad_id, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, bad_id, undefined, bad_id}, undefined, State)),
        ?_assertEqual({reply, fail, State}, package_server:handle_call({location_update, bad_id, bad_id, undefined}, undefined, State))
    ].

-endif.
