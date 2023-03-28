%%%-------------------------------------------------------------------
%% @doc package_tracking public API
%% @end
%%%-------------------------------------------------------------------

-module(package_tracking_app).

-behaviour(application).

-export([start/2, stop/1]).

% start(_StartType, _StartArgs) ->
%     package_tracking_sup:start_link().

% stop(_State) ->
%     ok.

%% internal functions



start(_Type, Args) ->
	Dispatch = cowboy_router:compile([
	    {'_', [
	        {"/", toppage_h, []},
		{"/query_facility",get_facility_h,Args},
		{"/query_package_history",get_package_h,Args},
		{"/query_vehicle_history",get_vehicle_h,Args},
        {"/store_facility_info",store_facility_h,Args},
        {"/store_package_info",store_package_h,Args},
        {"/store_vehicle_info",store_vehicle_h,Args}
	    ]}
	]),

	PrivDir = code:priv_dir(db_access),
        {ok,_} = cowboy:start_tls(https_listener, [
                  		{port, 443},
				{certfile, PrivDir ++ "/ssl/fullchain.pem"},
				{keyfile, PrivDir ++ "/ssl/privkey.pem"}
              		], #{env => #{dispatch => Dispatch}}),
	db_access_sup:start_link().
stop(_State) ->
    ok.

%% internal functions
