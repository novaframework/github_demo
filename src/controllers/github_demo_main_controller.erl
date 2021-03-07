-module(github_demo_main_controller).
-export([
         index/1
        ]).

-define(HEADERS, #{<<"Accept">> => <<"application/vnd.github.mercy-preview+json">>,
                   <<"User-Agent">> => <<"Awesome-github-demo">>}).
-define(GITHUB_API, <<"https://api.github.com/search/repositories?q=topic:">>).

index(_NovaReq) ->
    Url = <<?GITHUB_API/binary, "erlang">>,   
    logger:info("url: ~p", [Url]),
    Options = #{close => true,
                headers => ?HEADERS},
    case shttpc:get(Url, Options) of
        #{status := {200, _}, body := Body} ->
            {ok, [{repositories, json:decode(Body, [maps])}]};
        Result ->
            logger:warning("did not go well: ~p", [Result]),
            {ok, []}
    end.
