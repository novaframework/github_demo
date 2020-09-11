# Get github repos with topic Erlang

To run this demo use `rebar3 shell` on each branch.

## Introduction

With this demo we show some parts of how views and controllers work.

The flow is that each branch does a new step:

- master: this is just a first templated nova application that you get when running `rebar3 new nova MYAPP`
- part1: Here we add a http call to github and return the response body into the {{ message }}
- part2: We change dtl to be a table that will show repo name, github url and stars.

## part 1

Nova use https://github.com/JanHenryNystrom/jhn_stdlib that have some really good tools like:

- shttpc: small http client
- json: json library
- bstring: module that works like strings but for binaries

```Erlang

-module(github_demo_main_controller).
-export([
         index/1
        ]).

-define(HEADERS, #{<<"Accept">> => <<"application/vnd.github.mercy-preview+json">>,
                   <<"User-Agent">> => <<"Awesome-github-demo">>}).
-define(GITHUB_API, <<"https://api.github.com/search/repositories?q=topic:">>).

index(#{req := #{method := <<"GET">>}} = _NovaReq) ->
    Url = <<?GITHUB_API/binary, "erlang">>,
    logger:info("url: ~p", [Url]),
    Options = #{close => true,
                headers => ?HEADERS},
    case shttpc:get(Url, Options) of
        #{status := {200, _}, body := Body} ->
            {ok, [{message, Body}]};
        Result ->
            {ok, []}
    end.
```

Here we see that we first define two macros, HEADERS and GITHUB_API.
In index function we use shttpc to make the GET to github to fetch repositories that have erlang in topic.
We will return message back to the dtl that will print out the body in the page in th `<h1>{{message}}</h1>`.

## part 2

In part 2 we will take the json we will get back and make it to an erlang map that we will wrap in a table.

```html
<html>
  <body>
    <div>
      <h1>{{ repositories.total_count }}</h1>
      <table style="width:100%">
        <tr>
          <th>Name</th>
          <th>Url</th>
          <th>Stars</th>
        </tr>
        {%for repo in repositories.items %}
        <tr>
          <td>{{ repo.name }}</td>
          <td>{{ repo.html_url }}</td>
          <td>{{ repo.stargazers_count }}</td>
        </tr>
        {% endfor %}
      </table>
    </div>
  </body>
</html>
```

Here we have changed the github_demo_main.dtl to first print the total_count hat we get back.
Then we create the table header and after that we user a for that will iterate over all objects that are in repositories item list.
From that we will pick out the name, html_url and stargazers_count.

We will also need to change the github_main_controller.erl so that it returns a map.

```erlang
-module(github_demo_main_controller).
-export([
         index/1
        ]).

-define(HEADERS, #{<<"Accept">> => <<"application/vnd.github.mercy-preview+json">>,
                   <<"User-Agent">> => <<"Awesome-github-demo">>}).
-define(GITHUB_API, <<"https://api.github.com/search/repositories?q=topic:">>).

index(#{req := #{method := <<"GET">>}} = _NovaReq) ->
    Url = <<?GITHUB_API/binary, "erlang">>,
    logger:info("url: ~p", [Url]),
    Options = #{close => true,
                headers => ?HEADERS},
    case shttpc:get(Url, Options) of
        #{status := {200, _}, body := Body} ->
            {ok, [{repositories, json:decode(Body, [maps])}]};
        Result ->
            {ok, []}
    end.
```

Here we can see that we use the json module that we have from jhn_stdlib that will decode the json to a map.
