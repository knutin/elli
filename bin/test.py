#!/usr/bin/python
import httplib2
URL = "http://localhost:8080/foo/bar/baz"

h = httplib2.Http()

for i in range(1, 3):
    resp, content = h.request(URL)

print h.request(URL, headers = {"Connection": "close"})

