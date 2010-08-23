#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s erlcraft -config erlcraft -sname erlcraft -setcookie erlcraft -kernel inet_dist_listen_min 9100 -kernel inet_dist_listen_max 9105
