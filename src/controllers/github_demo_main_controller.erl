-module(github_demo_main_controller).
-export([
         index/1
        ]).

index(#{req := #{method := <<"GET">>}} = _NovaReq) ->
    {ok, [{message, "Nova is running!"}]}.
