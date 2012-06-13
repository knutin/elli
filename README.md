# elli - Erlang web server for HTTP APIs

Elli is a webserver aimed exclusively at building high-throughput,
low-latency HTTP APIs. If robustness and performance is more important
than a wide feature set, then `elli` might be for you.

Even though it is very early and Elli still has many rough edges, it
is used in production at Wooga.

Feedback and pull requests welcome!

## About

From operating and debugging high-volume, low-latency apps we have
gained some valuable insight into what we want from a webserver. We
want simplicity, robustness, performance, ease of debugging,
visibility into strange client behaviour, really good instrumentation
and good tests. We are willing to sacrifice basic features to achieve
this.

With this in mind we looked at the big names in the Erlang community:
Yaws, Mochiweb, Misultin and Cowboy. We found Mochiweb to be the best
match and we might eventually end up settling for it. However, we also
wanted to see if we could take the architecture of Mochiweb and
improve on it. `elli` takes the acceptor-turns-into-request-handler
idea found in Mochiweb, the binaries-only idea from Cowboy and the
request-response idea from WSGI/Rack (with chunked transfer being an
exception).

On top of this we built a handler that allows us to write HTTP
middleware modules to add practical features, like compression of
responses, access with timings, statistics dashboard and multiple
request handlers.

## Aren't there enough webservers in the Erlang community already?

There are a few very mature and robust projects with steady
development, one recently ceased development and one new kid on the
block with lots of interest. As `elli` is not a general purpose
webserver, but more of a specialized tool, we believe it has a very
different target audience and would not attract effort or users away
from the big names.

## Performance

"Hello World!" micro-benchmarks are really useful when measuring the
performance, but the numbers usually do more harm than good when
released. For this reason I include an ApacheBench script
(`bin/ab.sh`) and encourage you to run the benchmarks on your own.

## Installation

Add elli to you application by adding it as a dependency to you rebar
config.

```erlang
% rebar.config:
{deps, [
    {elli, "", {git, "git://github.com/knutin/elli.git"}},
    % ...
]}.
```

Afterwards you can run:

```
$: ./rebar get-deps
$: ./rebar compile
```


## Usage
```
$: erl -pa ebin

% starting elli
1>: {ok, Pid} = elli:start_link([{callback, elli_example_callback}, {port, 3000}]).

% stopping elli
2>: elli:stop(Pid).
```

## Callback module

There is an [example callback module](https://github.com/knutin/elli/blob/master/src/elli_example_callback.erl)
distributed with elli.

A minimal callback module would look like this:

```erlang
-module(elli_minimal_callback).
-export([handle/2, handle_event/3]).

-include("elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"hello">>, <<"world">>], _Req) ->
    %% Reply with a normal response. 'ok' can be used instead of '200'
    %% to signal success.
    {ok, [], <<"Hello World!">>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.

```

## Supervisor Childspec example

To add elli to a supervisor you can use the following example and adapt it to
your needs.


```erlang
-module(fancyapi_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ElliOpts = [{callback, fancyapi_callback}, {port, 3000}],
    ElliSpec = {
        fancy_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},

    {ok, { {one_for_one, 5, 10}, [ElliSpec]} }.

```


## Roadmap

 * Add proper `Date` header as clients use this for computing cache expirations
 * "Media" middleware for serving up static files from disk under a certain path
 * Sendfile


## Extensions

 * Access log: https://github.com/knutin/elli/blob/master/src/elli_access_log.erl
 * Real-time statistics dashboard: https://github.com/knutin/elli_stats
 * Basic auth: https://github.com/martinrehfeld/elli_basicauth
