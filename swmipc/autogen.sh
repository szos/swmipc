#!/bin/sh

autoheader -f
aclocal
autoconf -f
automake --add-missing -c
