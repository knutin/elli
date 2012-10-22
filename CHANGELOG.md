# CHANGELOG

## v0.3

 * Breaking change: Timeouts used in the HTTP protocol are now
   configurable. To implement this, changing a record and some
   callbacks was necessary. To upgrade, a restart of Elli is needed.

 * Elli now supports pipelining of any type of request. Some proxies
   or special clients (like ibrowse) will pipeline requests to reduce
   latency.

 * If there are no more file descriptors, Elli will shut down. This
   mimics the behaviour found in Yaws.

 * Chunked transfer responses will now exit the Elli process when the
   client closes the connection. Sending a synchronous chunk will
   return `{error, closed}` if client has closed the connection and
   the `chunk_complete` event is sent to your callback including which
   end closed the connection.

## v0.2.0

 * Breaking change: moved elli_access_log into a separate repository
   at github.com/wooga/elli_access_log. Thanks martinrehfeld.

## v0.1.3

 * Added elli_test which makes it easy to write unit tests for your
   callbacks. Thanks anha0825.

 * Added sendfile support. Thanks chrisavl.

## v0.1.2

 * Added option to specify listen IP address. Thanks hukl.

## v0.1.1

 * Don't look up the peer ip address on every request anymore, do it
   on demand using elli_request:peer/1.

## v0.1

 * Initial release.
