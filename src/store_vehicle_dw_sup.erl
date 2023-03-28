-module(store_vehicle_dw_sup).
-behaviour(supervisor).
-export([init/1,start_link/0,add_child/2,remove_child/2]).
-define(SERVER, ?MODULE).


% start_link() -> 
%     supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_link() -> 
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_child(Supervisor_name,Child_name)->
    r_r:add(store_vehicle_info_rr,Child_name), % DON'T FORGET TO CHANGE THE ROBIN!!
    supervisor:start_child(Supervisor_name,#{id => Child_name,
                                            start => {store_vehicle_info,start_link,[Child_name]},
                                            restart => permanent,
                                            type => worker,
                                            modules => [store_vehicle_info]}).

remove_child(Supervisor_name,Child_name)->
    r_r:remove(store_vehicle_info_rr, Child_name), % DON'T FORGET TO CHANGE THE ROBIN!!
    supervisor:terminate_child(Supervisor_name,Child_name),
    supervisor:delete_child(Supervisor_name,Child_name).


init(Start_info) ->
    %% This function has a value that is a tuple
    %% consisting of ok, a description of how to 
    %% interact with its children, and a list of its children. It is missing
    %% the restart strategy, intensity and period you intend to use.
    %% this template also starts no children of its own. The information
    %% to do so would come from Start_info if any children are to be 
    %% started.
    %%
    %% The pattern for the value of this function is, 
    %% {ok,{{restart_strategy,intensity,period},children}}
    ChildSpecs = [],
    ChildSpecs = [generate_spec(Name) || Name <- ["store_vehicle_info"++ Num || Num <- ["1","2","3","4","5","6","7","8","9","10"]]], % DON'T FORGET TO CHANGE THE NAME!!
    [r_r:add(store_vehicle_info_rr,Child_name) || Child_name <- ["store_vehicle_info"++ Num || Num <- ["1","2","3","4","5","6","7","8","9","10"]]], % DON'T FORGET TO CHANGE THE ROBIN AND NAME!!
    {ok,{{one_for_one,1000,5},ChildSpecs}}.




generate_spec(Name)->
%%
%% A child Specification is a record with the following mappings.
%%
%% child_spec() = #{id => child_id(),       % mandatory. The name to be registered.
%%                  start => mfargs(),      % mandatory. The module's startup function.
%%                  restart => atom(),              % optional. Options are permanent (restart always), transient (restart only after abnormal termination), and temporary (never restart).
%%                  shutdown => integer()|atom(),   % optional. A number or the atom infinity representing the milliseconds allowed for a soft, normal shutdown before it is killed brutally.
%%                  type => atom(),                 % optional. Options are worker or supervisor.
%%                  modules => [module()]|atom()}   % optional. A list of modules to be considered for upgrading
%%                                                  % when the child's code is upgraded. The dynamic atom is used for when 
%%                                                  % such a list is unknown, for example when the child is a 
%%                                                  % gen_event manager with some unknown types of gen_event handler
%%                                                  % modules to be added later.
        #{id => Name,
          start => {store_vehicle_info,start_link,[Name]},% This template forces local registration of the child and
                                                  % forces it to startup without parameters. 
          restart => transient,
          shutdown => 2000,
          type => worker,
          modules => [store_vehicle_info]}.