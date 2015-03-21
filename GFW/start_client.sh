#!/bin/sh
erlc /Users/dyun/Program/GitHub/ERLSocks/GFW/gfw_client.erl
erl -noshell -s gfw_client start -detached
