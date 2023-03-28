-module(get_vehicle_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	Vehicle_Id = maps:get("vehicle_uuid",jsx:decode(Data)),
    Pid = r_r:get_next(get_vehicle_rr),
    Vehicle_History = jsx:encode(get_vehicle_info:get_log(Pid,Vehicle_Id)),
    % {"facility_uuid":"string"}
	% Friends = jsx:encode(get_friends_server:get_friends_of(Name)),
	% %io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Vehicle_History, Req0),
	{ok, Req, Opts}.
    