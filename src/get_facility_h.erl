-module(get_facility_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	Facility_Id = maps:get("facility_uuid",jsx:decode(Data)),
    Pid = r_r:get_next(get_facility_rr),
    Facility_city = jsx:encode(get_facility:get_info(Pid,Facility_Id)),
    % {"facility_uuid":"string"}
	% Friends = jsx:encode(get_friends_server:get_friends_of(Name)),
	% %io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Facility_city, Req0),
	{ok, Req, Opts}.

	%out from server: {"city":"string"}
    