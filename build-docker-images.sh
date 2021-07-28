#!/bin/bash
docker build -t mu4e-views-test-gui -f Dockerfile-gui .
docker build -t mu4e-views-test -f Dockerfile .
