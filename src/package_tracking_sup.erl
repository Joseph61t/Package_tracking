%%%-------------------------------------------------------------------
%% @doc package_tracking top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(package_tracking_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1000,
                 period => 5},
    ChildSpecs = [#{id => get_facility_dist_sup,
                start => {get_facility_dist_sup,start_link,[]},
                % start => {get_facility_dist_sup,start,[local,get_facility_dist_sup,[]]},
                restart => transient,
                shutdown => 2000,
                type => supervisor,
                modules => [get_facility_dist_sup]},

                #{id => get_package_dist_sup,
                start => {get_package_dist_sup,start_link,[]},
                restart => transient,
                shutdown => 2000,
                type => supervisor,
                modules => [get_package_dist_sup]},
                
                #{id => get_vehicle_dist_sup,
                start => {get_vehicle_dist_sup,start_link,[]},
                restart => transient,
                shutdown => 2000,
                type => supervisor,
                modules => [get_vehicle_dist_sup]},
                
                #{id => store_facility_dist_sup,
                start => {store_facility_dist_sup,start_link,[]},
                restart => transient,
                shutdown => 2000,
                type => supervisor,
                modules => [store_facility_dist_sup]},
                
                #{id => store_package_dist_sup,
                start => {store_package_dist_sup,start_link,[]},
                restart => transient,
                shutdown => 2000,
                type => supervisor,
                modules => [store_package_dist_sup]},
                
                #{id => store_vehicle_dist_sup,
                start => {store_vehicle_dist_sup,start_link,[]},
                restart => transient,
                shutdown => 2000,
                type => supervisor,
                modules => [store_vehicle_dist_sup]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions












