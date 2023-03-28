%%%-------------------------------------------------------------------
%% @doc package_tracking top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(store_package_dist_sup).

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
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => r_r,
                start => {r_r,start,[local,store_package_rr]},
                restart => transient,
                shutdown => 2000,
                type => worker,
                modules => [r_r]},
                #{id => store_package_dw_sup,
                start => {store_package_dw_sup,start_link,[]},
                restart => transient,
                shutdown => 2000,
                type => supervisor,
                modules => [store_package_dw_sup]}
          ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions


