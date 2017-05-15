#!/usr/bin/env bash


curl -i -d '["header", 0]' http://localhost:8040
curl -i -d '["headers", 1, 0]' http://localhost:8040
