#!/usr/bin/env sh

sed -i 's/, warnings_as_errors//' deps/poolboy/rebar.config
sed -i 's/warnings_as_errors, //' deps/meck/rebar.config
sed -i 's/warnings_as_errors, //' deps/riak_core/rebar.config

