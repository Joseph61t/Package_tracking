-module(store_package_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	Package_Info = jsx:decode(Data),
    Pid = r_r:get_next(store_package_rr),
	Package_Id = maps:get("package_uuid",Package_Info),
    Results = jsx:encode(store_package_info:set_info(Pid,Package_Id,Package_Info)),
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