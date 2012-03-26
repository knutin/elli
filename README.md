# elli - experimental web server

The purpose of this project is to try out a few experiments in the
core of the web server loop to create a webserver that is as robust
and performant as it can be.


Goals:

 * Robustness
 * As minimalistic as can be, without sacrificing robustness and correctness
 * Not use more processes or messages than absolutely required
 * Upgrade without restart
 * Chunked transfer
 * Well tested
 * Traceability
 * Metrics, stats, hooks
 * Gzip compression for replies over a certain size

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

see examples/my_example_callback.erl
