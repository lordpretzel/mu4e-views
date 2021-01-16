#!/bin/bash
docker run -d -p 5900:5900 -v "$(pwd)":/mu4e-views --rm mu4e-views-test-gui
