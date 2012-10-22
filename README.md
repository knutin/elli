# elli - Erlang web server for HTTP APIs

![Travis CI](https://secure.travis-ci.org/knutin/elli.png)

Elli is a webserver you can run inside your Erlang application to
expose an HTTP API. Elli is a aimed exclusively at building
high-throughput, low-latency HTTP APIs. If robustness and performance
is more important than general purpose features, then `elli` might be
for you.

Elli is used in production at Wooga.

## Features

Here's the features Elli *does* have:

 * Rack-style request-response allowing "middlewares" to implement
   useful features like compression, encoding, stats

 * Short-circuiting of responses with `throw`

 * Many acceptors waiting for a client, handling the lifecycle of that
   connection, everything including the user code runs in one process
   to reduce overhead

 * Binaries everywhere for strings, response may be iolist

 * Instrumentation with user callbacks for events like client
   unexpectedly closed connection, request completed with timing
   details

 * Keep alive

 * Chunked transfer in responses for real-time push to clients

 * Pipelining


## Extensions

 * Access log: https://github.com/wooga/elli_access_log
 * Real-time statistics dashboard: https://github.com/knutin/elli_stats
 * Basic auth: https://github.com/martinrehfeld/elli_basicauth
 * Static content: https://github.com/chrisavl/elli_fileserve
 * "Date" header: https://github.com/knutin/elli_date


## About

From operating and debugging high-volume, low-latency apps we have
gained some valuable insight into what we want from a webserver. We
want simplicity, robustness, performance, ease of debugging,
visibility into strange client behaviour, really good instrumentation
and good tests. We are willing to sacrifice almost everything, even
basic features to achieve this.

With this in mind we looked at the big names in the Erlang community:
Yaws, Mochiweb, Misultin and Cowboy. We found Mochiweb to be the best
match. However, we also wanted to see if we could take the
architecture of Mochiweb and improve on it. `elli` takes the
acceptor-turns-into-request-handler idea found in Mochiweb, the
binaries-only idea from Cowboy and the request-response idea from
WSGI/Rack (with chunked transfer being an exception).

On top of this we built a handler that allows us to write HTTP
middleware modules to add practical features, like compression of
responses, HTTP access log with timings, a real-time statistics
dashboard and chaining multiple request handlers.

## Aren't there enough webservers in the Erlang community already?

There are a few very mature and robust projects with steady
development, one recently ceased development and one new kid on the
block with lots of interest. As `elli` is not a general purpose
webserver, but more of a specialized tool, we believe it has a very
different target audience and would not attract effort or users away
from the big names.

## Why another webserver? Isn't this just the NIH syndrome?

Yaws, Mochiweb, Misultin and Cowboy are great projects, hardened over
time and full of very useful features for web development. If you
value developer productivity, Yaws is an excellent choice. If you want
a fast and lightweight server, Mochiweb and Cowboy are excellent
choices.

Having used and studied all of these projects, we believed that if we
merged some of the existing ideas and added some ideas from other
communities, we could create a core that was better for our use cases.

It started out as an experiment to see if it is at all possible to
significantly improve and it turns out that for our particular use
cases, there is enough improvement to warrant a new project.

## What makes Elli different?

Elli has a very simple architecture. It avoids using more processes
and messages than absolutely necessary. It uses binaries for
strings. The request-response programming model allows middlewares to
do much heavy lifting, so the core can stay very simple. It has been
instrumented so as a user you can understand where time is spent. When
things go wrong, like the client closed the connection before you
could send a response, you are notified about these things so you can
better understand your client behaviour.

## Performance

"Hello World!" micro-benchmarks are really useful when measuring the
performance of the webserver itself, but the numbers usually do more
harm than good when released. I encourage you to run your own
benchmarks, on your own hardware. Mark Nottingham has some
[very good pointers](http://www.mnot.net/blog/2011/05/18/http_benchmark_rules)
about benchmarking HTTP servers.

## Installation

Add elli to your application by adding it as a dependency to your
rebar config.

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
$: erl -pa deps/*/ebin ebin

% starting elli
1>: {ok, Pid} = elli:start_link([{callback, elli_example_callback}, {port, 3000}]).
```

## Callback module

There is an [example callback module](https://github.com/knutin/elli/blob/master/src/elli_example_callback.erl)
distributed with elli that can be used and adopted right away.

A minimal callback module could look like this:

```erlang
-module(elli_minimal_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
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


k
