#!/bin/sh
erlc gfw_socks.erl
erl -noshell -s gfw_socks start
