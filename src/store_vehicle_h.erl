-module(store_vehicle_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	Vehicle_Info = jsx:decode(Data),
    Pid = r_r:get_next(store_vehicle_rr),
	Vehicle_Id = maps:get("vehicle_uuid",Vehicle_Info),
    Results = jsx:encode(store_facility_info:set_info(Pid,Vehicle_Id,Vehicle_Info)),
	% {"facility_uuid":"string","city":"string"}
	% Friends = jsx:encode(get_friends_server:get_friends_of(Name)),
	% %io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Results, Req0), %
	{ok, Req, Opts}.
    	%out from server: {"city":"string"}
		% {"history":[{"location":{"lat":"real","long":"real"},"time_stamp":"UINT_32"}]}
		% {"history":[{"holder_uuid":"string","time_stamp":"UINT_32"}]}

	% init(Req0, Opts) ->
	% {ok,Data,_} = cowboy_req:read_body(Req0),
	% %it is expected that the data consists of one quoted-string name
	% %in an array.
	% [Name,Friend|_] = jsx:decode(Data),
	% Friends = [Friend]
	% 	++get_friends_server:get_friends_of(Name),
	% store_friends_server:set_friends_for(Name,Friends),
	% Req = cowboy_req:reply(200, #{
	% 	<<"content-type">> => <<"text/json">>
	% },"[\"done\"]", Req0),
	% {ok, Req, Opts}.