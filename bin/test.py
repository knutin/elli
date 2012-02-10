#!/usr/bin/python
URL = "http://localhost:8080/foo/bar/baz?ok=bar"

from asynchttp import Http

http = Http()

response, content = http.request(URL)
print response
print content

