-module(get_package_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	Package_Id = maps:get("package_uuid",jsx:decode(Data)),
    Pid = r_r:get_next(get_package_rr),
    Package_history = jsx:encode(get_package:get_info(Pid,Package_Id)),
    % {"package_uuid":"string"}
	% Friends = jsx:encode(get_friends_server:get_friends_of(Name)),
	% %io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Package_history, Req0),
	{ok, Req, Opts}.
    