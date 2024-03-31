#!/bin/bash
docker build -t mu4e-views-test-gui --platform linux/amd64 -f Dockerfile-gui .
#docker build -t mu4e-views-test -f Dockerfile .
