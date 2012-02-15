#!/usr/bin/python
import sys
import eventlet
import time
from eventlet.green import urllib2

URL = "http://localhost:8080/hello/world"
NUM_REQS = int(sys.argv[1])

def fetch():
    v = urllib2.urlopen(URL).read()
    return


pool = eventlet.GreenPool(size=25)

start = time.time()

map(lambda _: pool.spawn_n(fetch), range(1, NUM_REQS))

pool.waitall()

end = time.time()
duration = end - start


print NUM_REQS / duration, "rps"

