%%%-------------------------------------------------------------------
%% @doc package_tracking top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(get_facility_dist_sup).

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
    ChildSpecs = [#{id => r_r,
                start => {r_r,start,[local,get_facility_rr]},
                restart => transient,
                shutdown => 2000,
                type => worker,
                modules => [r_r]},
                #{id => get_facility_dw_sup,
                start => {get_facility_dw_sup,start_link,[]},
                restart => transient,
                shutdown => 2000,
                type => supervisor,
                modules => [get_facility_dw_sup]}
          ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions


