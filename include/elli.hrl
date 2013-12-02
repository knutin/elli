
-record(req, {
          method :: elli:http_method(),
          path :: [binary()],
          args :: [{binary(), any()}],
          raw_path :: binary(),
          version :: elli:version(),
          headers :: elli:headers(),
          body :: elli:body(),
          pid :: pid(),
          socket :: inet:socket()
}).

-define(EXAMPLE_CONF, [{callback, elli_example_callback},
                       {callback_args, []}]).
