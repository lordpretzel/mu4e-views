#!/bin/bash
docker run -ti -v "$(pwd)":/mu4e-views --rm mu4e-views-test /bin/bash
