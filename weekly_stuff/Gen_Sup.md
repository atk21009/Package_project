# GenServer in Erlang

## What is GenServer?

GenServer is a generic server module in Erlang that abstracts and simplifies the process of writing server processes. It handles the boilerplate code for message passing, state management, and process lifecycle.

## Example:

```erlang
-module(example_gen_server).
-behaviour(gen_server).

%% API
-export([start_link/0, call/1, cast/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call(Message) ->
    gen_server:call(?MODULE, Message).

cast(Message) ->
    gen_server:cast(?MODULE, Message).

%%% gen_server callbacks
init([]) ->
    {ok, 0}. %% Initial state is 0

handle_call(get, _From, State) ->
    {reply, State, State};
handle_call({set, Value}, _From, _State) ->
    {reply, ok, Value}.

handle_cast(increment, State) ->
    {noreply, State + 1};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

# gen_statem in Erlang

## What is gen_statem?

gen_statem is a behavior module in Erlang that implements a state machine. It can be used to manage processes with well-defined states and transitions.

## Example:

```erlang
-module(example_gen_statem).
-behaviour(gen_statem).

%% API
-export([start_link/0, stop/0, event/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

%%% API
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?MODULE).

event(Event) ->
    gen_statem:cast(?MODULE, Event).

%%% gen_statem callbacks
init([]) ->
    {ok, idle, undefined}.

callback_mode() ->
    state_functions.

handle_event(cast, hello, idle, State) ->
    io:format("Received hello in idle state~n"),
    {next_state, active, State};
handle_event(cast, hello, active, State) ->
    io:format("Received hello in active state~n"),
    {keep_state, State};
handle_event(_, _, State, Data) ->
    {keep_state_and_data, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
```


# gen_event in Erlang

## What is gen_event?

gen_event is a behavior module in Erlang for event handling. It provides a generic way to manage and dispatch events to event handlers.

## Example:

### Event Manager:

```erlang
-module(example_gen_event).
-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/1, notify/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%% API
start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler) ->
    gen_event:add_handler(?MODULE, Handler, []).

notify(Event) ->
    gen_event:notify(?MODULE, Event).
```


```erlang
-module(example_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

init([]) ->
    {ok, []}.

handle_event(Event, State) ->
    io:format("Received event: ~p~n", [Event]),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

# Supervisors and Supervision Trees in Erlang

## What is a Supervisor?

A supervisor is a process that monitors and manages other processes (workers or other supervisors). It restarts child processes when they fail according to a defined strategy.

## Example:

```erlang
-module(example_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10},
          [{example_gen_server, {example_gen_server, start_link, []}, permanent, 5000, worker, [example_gen_server]},
           {example_gen_statem, {example_gen_statem, start_link, []}, permanent, 5000, worker, [example_gen_statem]},
           {example_gen_event, {example_gen_event, start_link, []}, permanent, 5000, worker, [example_gen_event]}]}}.
```
