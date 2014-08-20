#!/bin/sh

erlc socks.erl
erl -noshell -s socks start
