-module(elli_send_file_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").

-include_lib("kernel/include/file.hrl").


elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(sendfile()),
      ?_test(sendfile_range_100_199()),
      ?_test(sendfile_range_0_end()),
      ?_test(sendfile_range_0_oversized()),
      ?_test(sendfile_range_suffix_42()),
      ?_test(sendfile_range_suffix_oversized()),
      ?_test(sendfile_range_invalid_syntax()),
      ?_test(sendfile_range_invalid_neg_start()),
      ?_test(sendfile_range_invalid_start_greater_than_end()),
      ?_test(sendfile_range_invalid_start_greater_than_size()),
      ?_test(sendfile_range_explicit_range()),
      ?_test(sendfile_size_0())
     ]}.



setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),
    {ok, P} = elli:start_link([{callback, elli_example_callback}, {port, 3001}]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].


%%
%% INTEGRATION TESTS
%% Uses inets httpc to actually call Elli over the network
%%

sendfile() ->
    Headers = [],
    {ok, Response} = httpc:request(
                       get, {"http://localhost:3001/sendfile", Headers}, [], []),

    F = "../README.md",
    {ok, Expected} = file:read_file(F),

    ?assertEqual(200, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", integer_to_list(size(Expected))}],
                 headers(Response)),
    ?assertEqual(binary_to_list(Expected), body(Response)).

%% Testing send_file with a range of 100-199,
%% i.e. the second 100 bytes of the file.  
sendfile_range_100_199() ->
    Headers = [{"Range", "bytes=100-199"}],
    {ok, Response} = httpc:request(
		       get, {"http://localhost:3001/sendfile", Headers}, [], []),

    F = "../README.md",
    {ok, Fd} = file:open(F, [read, raw, binary]),
    {ok, Expected} = file:pread(Fd, 100, 100),
    file:close(Fd),
    {ok, #file_info{size = Size}} = file:read_file_info(F),

    ?assertEqual(206, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "100"},
                  {"content-range", "bytes 100-199/" ++ ?i2l(Size)}],
                 headers(Response)),
    ?assertEqual(binary_to_list(Expected), body(Response)).

%% Testing send_file with a range of 0-,
%% i.e. the entire file.
sendfile_range_0_end() ->
    Headers = [{"Range", "bytes=0-"}],
    {ok, Response} = httpc:request(
		       get, {"http://localhost:3001/sendfile", Headers}, [], []),

    F = "../README.md",
    {ok, Expected} = file:read_file(F),
    Size = size(Expected),

    ?assertEqual(206, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", ?i2l(Size)},
                  {"content-range", "bytes 0-" ++ ?i2l(Size -1) ++ "/" ++ ?i2l(Size)}],
                 headers(Response)),
    ?assertEqual(binary_to_list(Expected), body(Response)).

%% Testing send_file with a range of 0-100000000,
%% i.e. the last-byte-pos is greater than the file size.
%% According to the rfc, this should be interpreted as
%% a last-byte-pos equal to one less than the file size. 
sendfile_range_0_oversized() ->
    Headers = [{"Range", "bytes=0-100000000"}],
    {ok, Response} = httpc:request(
		       get, {"http://localhost:3001/sendfile", Headers}, [], []),

    F = "../README.md",
    {ok, Expected} = file:read_file(F),
    Size = size(Expected),

    ?assertEqual(206, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", ?i2l(Size)},
                  {"content-range", "bytes 0-" ++ ?i2l(Size - 1) ++ "/" ++ ?i2l(Size)}],
                 headers(Response)),
    ?assertEqual(binary_to_list(Expected), body(Response)).

%% Testing send_file with a suffix range of 42,
%% i.e. the last 42 bytes of the file.
sendfile_range_suffix_42() ->
    Headers = [{"Range", "bytes=-42"}],
    {ok, Response} = httpc:request(
		       get, {"http://localhost:3001/sendfile", Headers}, [], []),

    F = "../README.md",
    {ok, #file_info{size = Size}} = file:read_file_info(F),
    {ok, Fd} = file:open(F, [read, raw, binary]),
    {ok, Expected} = file:pread(Fd, Size - 42, 42),
    file:close(Fd),

    ?assertEqual(206, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "42"},
                  {"content-range", "bytes " ++
                       ?i2l(Size - 42) ++ "-" ++ ?i2l(Size - 1) ++ "/" ++ ?i2l(Size)}],
                 headers(Response)),
    ?assertEqual(binary_to_list(Expected), body(Response)).

%% Testing send_file with a range of -100000000,
%% i.e. the suffix length is greater than the file size.
%% According to the rfc, this should be interpreted as
%% a suffix length equal to the file size. 
sendfile_range_suffix_oversized() ->
    Headers = [{"Range", "bytes=-100000000"}],
    {ok, Response} = httpc:request(
		       get, {"http://localhost:3001/sendfile", Headers}, [], []),

    F = "../README.md",
    {ok, Expected} = file:read_file(F),
    Size = size(Expected),

    ?assertEqual(206, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", ?i2l(Size)},
                  {"content-range", "bytes 0-" ++ ?i2l(Size -1) ++ "/" ++ ?i2l(Size)}],
                 headers(Response)),
    ?assertEqual(binary_to_list(Expected), body(Response)).

%% Testing send_file with a syntax error on the range request.
%% This should result in a 416 - Requested Range Not Satisfiable response.
sendfile_range_invalid_syntax() ->
    Headers = [{"Range", "boates=0-"}],
    {ok, Response} = httpc:request(
		       get, {"http://localhost:3001/sendfile", Headers}, [], []),

    F = "../README.md",
    {ok, Expected} = file:read_file(F),
    Size = size(Expected),

    ?assertEqual(416, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "0"},
                  {"content-range", "bytes */" ++ ?i2l(Size)}],
                 headers(Response)),
    ?assertEqual("", body(Response)).

%% Testing send_file with a negative start-byte-pos.
%% This should result in a 416 - Requested Range Not Satisfiable response.
sendfile_range_invalid_neg_start() ->
    Headers = [{"Range", "bytes=-100-100"}],
    {ok, Response} = httpc:request(
		       get, {"http://localhost:3001/sendfile", Headers}, [], []),

    F = "../README.md",
    {ok, Expected} = file:read_file(F),
    Size = size(Expected),

    ?assertEqual(416, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "0"},
                  {"content-range", "bytes */" ++ ?i2l(Size)}],
                 headers(Response)),
    ?assertEqual("", body(Response)).

%% Testing send_file with a start-byte-pos greater than end-byte-pos.
%% This should result in a 416 - Requested Range Not Satisfiable response.
sendfile_range_invalid_start_greater_than_end() ->
    Headers = [{"Range", "bytes=200-100"}],
    {ok, Response} = httpc:request(
		       get, {"http://localhost:3001/sendfile", Headers}, [], []),

    F = "../README.md",
    {ok, Expected} = file:read_file(F),
    Size = size(Expected),

    ?assertEqual(416, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "0"},
                  {"content-range", "bytes */" ++ ?i2l(Size)}],
                 headers(Response)),
    ?assertEqual("", body(Response)).

%% Testing send_file with a start-byte-pos greater that the file size.
%% This should result in a 416 - Requested Range Not Satisfiable response.
sendfile_range_invalid_start_greater_than_size() ->
    Headers = [{"Range", "bytes=100000000-200000000"}],
    {ok, Response} = httpc:request(
		       get, {"http://localhost:3001/sendfile", Headers}, [], []),

    F = "../README.md",
    {ok, Expected} = file:read_file(F),
    Size = size(Expected),


    ?assertEqual(416, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "0"},
                  {"content-range", "bytes */" ++ ?i2l(Size)}],
                 headers(Response)),
    ?assertEqual("", body(Response)).


%% Testing send_file with an explicit offset = 300
%% and length 400.
sendfile_range_explicit_range() ->
    {ok, Response} = httpc:request("http://localhost:3001/sendfile/range"),
    F = "../README.md",
    {ok, Fd} = file:open(F, [read, raw, binary]),
    {ok, Expected} = file:pread(Fd, 300, 400),
    file:close(Fd),
    {ok, #file_info{size = Size}} = file:read_file_info(F),

    ?assertEqual(206, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "400"},
                  {"content-range", "bytes 300-699/" ++ ?i2l(Size)}],
                 headers(Response)),
    ?assertEqual(binary_to_list(Expected), body(Response)).

sendfile_size_0() ->
    {ok, Response} = httpc:request("http://localhost:3001/sendfile/size-0"),
    ?assertEqual(204, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
		  {"content-length", "0"}],
                 headers(Response)),
    ?assertEqual("", body(Response)).    

%%
%% HELPERS
%%

status({{_, Status, _}, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).
