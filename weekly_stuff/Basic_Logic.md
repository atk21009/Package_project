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

## Write the Test Cases with Stub Functions and Handle Call

1. Define EUnit test cases in `package_server_tests.erl` for the following functions:
    - `transfer_package`
    - `mark_delivered`
    - `request_location`
    - `update_location`

2. Each test case should follow this structure:
    ```erlang
    ?_assertEqual(ExpectedOutput, package_server:FunctionName(Input)).
    ```

## Write Out the Handle Call Function

1. Implement the `handle_call` function in `package_server.erl` to handle different requests:
    ```erlang
    handle_call({transfer_package, LocationId, PackageId}, _From, State) ->
        case {LocationId, PackageId} of
            {valid_location, valid_package} ->
                {reply, ok, State};
            _ ->
                {reply, fail, State}
        end;

    handle_call({mark_delivered, PackageId}, _From, State) ->
        case PackageId of
            valid_package ->
                {reply, ok, State};
            _ ->
                {reply, fail, State}
        end;

    handle_call({request_location, PackageId}, _From, State) ->
        case PackageId of
            valid_package ->
                {reply, {ok, {123.45, 67.89}}, State};
            _ ->
                {reply, fail, State}
        end;

    handle_call({update_location, LocationId, Longitude, Latitude}, _From, State) ->
        case {LocationId, Longitude, Latitude} of
            {valid_location, 123.45, 67.89} ->
                {reply, ok, State};
            _ ->
                {reply, fail, State}
        end;

    handle_call(stop, _From, State) ->
        {stop, normal, ok, State};

    handle_call(_Request, _From, State) ->
        {reply, {error, unknown_request}, State}.
    ```

## Write the Stub Functions

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

2. Use these stub functions in your `handle_call` implementations.

3. Ensure all stub functions return appropriate values for your test cases.

# My Repo

[repo](repo_path)
