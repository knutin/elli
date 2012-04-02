#!/bin/bash

ab -c 50 -n 1000000 http://localhost:8080/hello?name=knut