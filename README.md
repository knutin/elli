My first recreational webserver.

Goals:

 * As efficient as can be, without sacrificing
 * Robustness and correctness
 * Not use more processes or messages than absolutely required
 * Comet/long-polling
 * Well tested
 * Upgrade without restart
 * Traceability
 * Metrics, stats, hooks
 * Gzip compression for replies over a certain size

Non-goals:

 * SSL
 * HTTP compliance (Date headers, all verbs, pipelining, etc)
 * Normal webserver features like html templating, session handling
 * Virtual hosts, binding to ip addresses

