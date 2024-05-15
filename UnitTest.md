# Mod -> package_server 
__EUnit Test for each of out main functions__

## Package Transfer

__Example of test__
?_assertEqual({reply, ok, some_Db_PID},
              package_module:handle_call({package_transfered, pack_id, loc_id}, some_from_pid, some_Db_PID)).

__Test for this segment__
| Input   | Output  |
|---------|---------|
| {package_transfered(pack_id, loc_id)} | ok |
| {package_transfered(bad_id, loc_id)} | fail |
| {package_transfered(pack_id, bad_id)} | fail |
| {package_transfered(pack_id, _ )} | fail |
| {package_transfered(bad_id, _ )} | fail |
| {package_transfered( _ , loc_id)} | fail |
| {package_transfered( _ , bad_id)} | fail |
| {package_transfered( _ , _ )} | fail |


## Delievered

__Example of test__
?_assertEqual({reply, ok, some_Db_PID},
              package_module:handle_call({delievered, pack_id}, some_from_pid, some_Db_PID)).

__Test for this segment__
| Input   | Output  |
|---------|---------|
| {delievered(pack_id)} | ok |
| {delievered(bad_id} | fail |
| {delievered( _ )} | fail |


## Location Request

__Example of test__
?_assertEqual({reply, {ok, Longitude, Latitude}, some_Db_PID},
              package_module:handle_call({location_request, pack_id}, some_from_pid, some_Db_PID)).

__Test for this segment__
| Input   | Output  |
|---------|---------|
| {location_request(pack_id)} | {ok, Longitude, Latitude} |
| {location_request(bad_id} | fail |
| {location_request( _ )} | fail |



## Location Update

__Example of test__
?_assertEqual({reply, ok, some_Db_PID},
              package_module:handle_call({location_update, loc_id, lon, lat}, some_from_pid, some_Db_PID)).

__Test for this segment__
| Input   | Output  |
|---------|---------|
| {location_update(loc_id, lon, lat)} | ok |
| {location_update(bad_id, lon, lat)} | fail |
| {location_update(loc_id, bad_id, lat)} | fail |
| {location_update(loc_id, lon, bad_id)} | fail |
| {location_update(bad_id, bad_id, lat)} | fail |
| {location_update(bad_id, lon, bad_id)} | fail |
| {location_update(loc_id, bad_id, bad_id)} | fail |
| {location_update(bad_id, bad_id, bad_id)} | fail |
| {location_update( _ , lon, lat)} | fail |
| {location_update(loc_id, _ , lat)} | fail |
| {location_update(loc_id, lon, _ )} | fail |
| {location_update( _ , _ , lat)} | fail |
| {location_update( _ , lon, _ )} | fail |
| {location_update(loc_id, _ , _ )} | fail |
| {location_update( _ , _ , _ )} | fail |
| {location_update(bad_id, _ , lat)} | fail |
| {location_update(bad_id, lon, _)} | fail |
| {location_update( _ , bad_id, lat)} | fail |
| {location_update(loc_id, bad_id, _)} | fail |
| {location_update(_, lon, bad_id)} | fail |
| {location_update(loc_id, _, bad_id)} | fail |
| {location_update(bad_id, _ , _)} | fail |
| {location_update( _ , bad_id, _ )} | fail |
| {location_update(_, _, bad_id)} | fail |
| {location_update(_, bad_id, bad_id)} | fail |
| {location_update(bad_id, _ , bad_id)} | fail |
| {location_update(bad_id, bad_id, _)} | fail |

