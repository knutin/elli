
-type callback_mod() :: module().
-type callback_args() :: any().
-type callback() :: {callback_mod(), callback_args()}.

-type path() :: binary().
-type args() :: binary().
-type version() :: {0,9} | {1,0} | {1,1}.
-type header() :: {Key::binary(), Value::binary() | string()}.
-type headers() :: [header()].
-type body() :: binary() | iolist().
-type response() :: iolist().
-type http_method() :: 'OPTIONS' | 'GET' | 'HEAD' | 'POST' |
                       'PUT' | 'DELETE' | 'TRACE'.
-type response_code() :: 100..999.
-type connection_token_atom() :: keep_alive | close.

-type http_range() :: {First::non_neg_integer(), Last::non_neg_integer()} |
                      {offset, Offset::non_neg_integer()} |
                      {suffix, Length::pos_integer()}.

-type range() :: {Offset::non_neg_integer(), Length::non_neg_integer()}.

-type timestamp() :: {integer(), integer(), integer()}.
-type elli_event() :: elli_startup |
                      bad_request | file_error |
                      chunk_complete | request_complete |
                      request_throw | request_error | request_exit |
                      request_closed | request_parse_error |
                      client_closed | client_timeout.
-type cfg() :: list() | [].

-record(req, {
          method :: http_method(),
          path :: [binary()],
          args :: [{binary(), any()}],
          raw_path :: binary(),
          version :: version(),
          headers :: headers(),
          body :: body(),
          pid :: pid(),
          socket :: inet:socket()
}).
-type req() :: #req{}.

-type chunk_ref() :: pid() | {error, not_supported}.
-type handler_ret() :: ignore | {response_code(), headers(), body()} 
                       | {ok, headers(), body()} 
                       | {chunk, headers()}.

-export_type([callback_mod/0, callback_args/0, callback/0, path/0,
  args/0, version/0, header/0, headers/0, body/0, response/0,
  http_method/0, response_code/0, connection_token_atom/0,
  http_range/0, range/0, timestamp/0, elli_event/0, req/0,
  chunk_ref/0, handler_ret/0]).

-define(EXAMPLE_CONF, [{callback, elli_example_callback},
                       {callback_args, []}]).
