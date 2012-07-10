
-type callback_mod() :: atom.
-type callback_args() :: any().
-type callback() :: {callback_mod(), callback_args()}.

-type path() :: binary().
-type args() :: binary().
-type version() :: {0,9} | {1,0} | {1,1}.
-type header() :: {Key::binary(), Value::binary() | string()}.
-type headers() :: [header()].
-type body() :: binary() | iolist().
-type response() :: iolist().

-type response_code() :: 200 | 404 | 500.
-type connection_token_atom() :: keep_alive | close.

-define(l2i(L), list_to_integer(L)).
-define(i2l(I), integer_to_list(I)).
-define(b2i(I), list_to_integer(binary_to_list(I))).

-type timestamp() :: {integer(), integer(), integer()}.


-record(state, {socket,
                acceptors = 0,
                open_reqs = 0,
                callback :: callback()
}).

-record(req, {
          method :: 'GET' | 'HEAD' | 'POST' | 'PUT',
          path :: [binary()],
          args,
          raw_path :: binary(),
          version,
          headers :: headers(),
          body :: body(),
          pid :: pid(),
          peer :: inet:ip_address()
}).
