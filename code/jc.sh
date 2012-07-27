#!/bin/sh

for param in $@;
do
    sml @SMLload jc-compile $param;
done;
