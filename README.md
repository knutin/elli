# elli - Erlang web server for HTTP APIs

This project is experimenting with a different core architecture to
create a webserver that can be the building block for a massive
throughput, low latency HTTP API. If robustness and performance is
more important than a wide feature set, then `elli` might be for you.

It is used in production at Wooga.

Goals:

 * Simple
 * Robustness
 * Performance
 * Not use more processes or messages than absolutely required
 * Upgrade without restart
 * Chunked transfer
 * Well tested
 * Traceability
 * Metrics, stats, hooks

Non-goals:

 * SSL
 * WebSockets
 * Rest/webmachine-like abstractions
 * HTTP compliance (Date headers, all verbs, pipelining, etc)
 * Normal webserver features like html templating, session handling
 * Virtual hosts, binding to ip addresses


## Usage

    $: rebar get-deps
    $: rebar compile
    $: erl -pa ebin

    % starting elli
    1>: {ok, Pid} = elli:start_link([{callback, elli_example_callback}, {port, 3000}]).

    % stopping elli
    2>: elli:stop(Pid).

## Callback module

see `src/elli_example_callback.erl`
