
-type header() :: {Key::binary(), Value::binary() | string()}.
-type headers() :: [header()].
-type body() :: binary().

-type response_code() :: 200 | 404 | 500.

-define(l2i(L), list_to_integer(L)).
-define(i2l(I), integer_to_list(I)).

-record(state, {socket,
                acceptors = 0,
                open_reqs = 0,
                callback
}).

-record(req, {
          method,
          path,
          args,
          version,
          headers,
          body,
          pid
}).
