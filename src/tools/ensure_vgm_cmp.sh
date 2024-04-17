#!/bin/sh
vgm_cmp $1 $2
if ! [ -f $2 ]; then
    cp $1 $2
fi
