# Basic Logic Setup

## Make EUnit

1. Based on your project outline, start writing tests for the logic.
2. Begin with the happy path to ensure the basic functionality works as expected.
3. Write tests for all the unhappy paths to handle error scenarios and edge cases.

- Team Example
[example](https://github.com/atk21009/Package_project/blob/main/weekly_stuff/UnitTest.md)

## Create tracker_business_logic Repo

1. Create a GitHub repository named `tracker_business_logic`.
2. Initialize Git in your project directory and push the initial commit to the repository.
    ```bash
    git init
    git remote add origin https://github.com/your-username/tracker_business_logic.git
    git add .
    git commit -m "Initial commit with package server and tests"
    git push -u origin master
    ```

## Write the Test Cases with Stub Functions and Handle Call

### Stub Functions Example

1. Define stub functions in `package_server.erl` to simulate database interactions:
    ```erlang
    %% Stub functions for database interactions
    -module(db_api).

    -export([put_package/2, get_package/1, update_location/3]).

    put_package(PackageId, LocationId) ->
        %% Simulate a successful database insert
        ok.

    get_package(PackageId) ->
        %% Simulate a successful database retrieval
        {ok, {123.45, 67.89}}.

    update_location(LocationId, Longitude, Latitude) ->
        %% Simulate a successful database update
        ok.
    ```

### Test Case Example

1. Define EUnit test cases in `package_server_tests.erl` for the following functions:
    - `transfer_package`
    - `mark_delivered`
    - `request_location`
    - `update_location`

2. Each test case should follow this structure:
    ```erlang
    ?_assertEqual(ExpectedOutput, package_server:handle_call(Input, From, State)).
    ```

3. Example:
    ```erlang
    -module(package_server_tests).
    -include_lib("eunit/include/eunit.hrl").

    handle_call_test_() ->
        {setup,
         fun() -> package_server:start_link() end,
         fun(_) -> package_server:stop() end,
         fun(_) ->
            %% Example for transfer_package
            ?_assertEqual({reply, ok, some_state}, package_server:handle_call({transfer_package, valid_location, valid_package}, self(), some_state)),
            ?_assertEqual({reply, fail, some_state}, package_server:handle_call({transfer_package, bad_location, valid_package}, self(), some_state))
         end}.
    ```

## Write Out the Handle Call Function

1. Implement the `handle_call` function in `package_server.erl` to handle different requests:
    ```erlang
    handle_call({transfer_package, LocationId, PackageId}, _From, State) ->
        case db_api:put_package(PackageId, LocationId) of
            ok ->
                {reply, ok, State};
            _ ->
                {reply, fail, State}
        end;

    handle_call({mark_delivered, PackageId}, _From, State) ->
        case db_api:get_package(PackageId) of
            {ok, _} ->
                {reply, ok, State};
            _ ->
                {reply, fail, State}
        end;

    handle_call({request_location, PackageId}, _From, State) ->
        case db_api:get_package(PackageId) of
            {ok, Location} ->
                {reply, {ok, Location}, State};
            _ ->
                {reply, fail, State}
        end;

    handle_call({update_location, LocationId, Longitude, Latitude}, _From, State) ->
        case db_api:update_location(LocationId, Longitude, Latitude) of
            ok ->
                {reply, ok, State};
            _ ->
                {reply, fail, State}
        end;

    handle_call(stop, _From, State) ->
        {stop, normal, ok, State};

    handle_call(_Request, _From, State) ->
        {reply, {error, unknown_request}, State}.
    ```

## Write the Main Functions

1. Implement the main functions that will be used to interact with the server:
    ```erlang
    transfer_package(LocationId, PackageId) ->
        gen_server:call(?MODULE, {transfer_package, LocationId, PackageId}).

    mark_delivered(PackageId) ->
        gen_server:call(?MODULE, {mark_delivered, PackageId}).

    request_location(PackageId) ->
        gen_server:call(?MODULE, {request_location, PackageId}).

    update_location(LocationId, Longitude, Latitude) ->
        gen_server:call(?MODULE, {update_location, LocationId, Longitude, Latitude}).
    ```

2. Use these stub functions in your `handle_call` implementations.

3. Ensure all stub functions return appropriate values for your test cases.

# My Repo

[repo](repo_path)
