# CHANGELOG

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
