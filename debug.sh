#!/bin/sh

BASE=`dirname $0`
THRIFT=$BASE/../thrift

erl -pa \
  $BASE/ebin \
  $BASE/gen/ebin \
  $THRIFT/ebin \
  -sname test \
  -setcookie ClueCon \
  -run conferencr_app start_all
