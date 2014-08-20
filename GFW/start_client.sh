#!/bin/sh
erlc gfw_client.erl
erl -noshell -s gfw_client start
