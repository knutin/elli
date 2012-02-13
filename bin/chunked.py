#!/usr/bin/python

from tornado import httpclient

URL = "http://localhost:8080/chunked"

http_client = httpclient.HTTPClient()

def cb(data):
    print data


if __name__ == "__main__":
    http_client = httpclient.HTTPClient()
    response = http_client.fetch(URL, streaming_callback = cb, request_timeout = 10000.0, user_agent = "tornado")

    print response.body



