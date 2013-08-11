#! /bin/bash

erl -pa ebin -pa deps/protobuffs/ebin -config server -K true -eval "application:start(sasl),application:start(codebattle)." -noshell -detached

exit $?
