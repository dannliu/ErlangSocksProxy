#!/bin/sh
cd /Users/dyun/Program/GitHub/dyun/ERLSocks/GFW
erlc /Users/dyun/Program/GitHub/dyun/ERLSocks/GFW/gfw_client.erl
erl -noshell -s gfw_client start -detached
