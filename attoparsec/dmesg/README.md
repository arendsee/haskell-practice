# dmesg

This is just a simple parser for the output of dmesg.

The format of a dmesg line is:

```
line -> '[' time ']' program ':' description
time -> integer '.' integer
program -> many (not ':')
description -> many (not '\n')
```
