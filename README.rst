nettty
======

Network over (t̶t̶y̶) stdin/stdout

the problem
===========

Supose you need to hop in a machine [say A] in order to access a set
of machines [say B, C, D]. Suppose everything useful in SSH to make
this task easier [= portforward of any sort] is unavailable and
firewall is everywhere:
::


         +-- +                 +-------+
         | f |        +------->+   B   |
         | i |        |        |       |
         | r |    +---+---+    +-------+
         | e |    |   A   |     +-------+
         | w +<-->+       +---->+   C   |
         | a |    +---+---+     |       |
         | l |        |         +-------+
         | l |        |          +-------+
         +-- +        |          |   D   |
                      +--------->+       |
                                 +-------+  

This is problem:
::

  local $ ssh A
  A $ ssh B

This is an viable solution, but suppose is currently unavailable:
::

  local $ ssh -D 1080 -N -f A
  local $ tsocks ssh B
  local $ tsocks ssh C

This would work too, but is also unavailable:
::

  local $ ssh -L 2222:B:22 -L 2223:C:22 -N -f A
  local $ ssh -p 2222 localhost # B
  local $ ssh -p 2223 localhost # C

how it works
============

Instead of relying on ssh builtin port forward, we've built our own
version using the stdin/stdout:
::

  #              binds to local
  #              tcp port
  #              ~~~~~~~~~~~~~   command to spawn
  #                             ~~~~~~~~~~~~~~~~~~
  $ nettty-proxy 1080           ssh -N -f A nettty

  #                                    connects to
  #                                    local port
  #                                    ~~~~~~~~~~~
  $ ssh -o 'ProxyCommand nettty-connect 1080       tcp://%h:%p' B

  # now using netcat instead of our own client
  $ ssh -o 'ProxyCommand nc -XCONNECT -xlocalhost:1080 %h %p' C

The ``netty-proxy`` command spawns a process which is supposed to
start the ``nettty`` process. The later listens for commands on stdin
and replies back on stdout.

The ``nettty-connect`` is just like ``netcat``, but upon connection it
sends the endpoint you want to connect to. Holly crap!

You can also emulate SSH port forwarding feature. For instance, the
following:
::
  $ ssh -L 4443:B:443 A

can be accomplished with:
::
  $ nettty-proxy 1080 ssh A nettty
  $ socat TCP-LISTEN:4443 PROXY:localhost:B:443,proxyport=1080

nettty protocol
===============

It is a text protocol, with four messages:
::

  # creates a new connection (to a server)
  # -> nettty/open <channel[int32]> <endpoint[string]>

  # sends data to the server
  # -> nettty/send <channel> <data>

  # sends data to the client
  # -> nettty/recv <channel> <data>

  # closes the connection
  # -> nettty/term <channel>

Example:

This performs an ``GET / HTTP/1.0`` and closes the connection.
::

  $ nettty
  # -> nettty/ready
  # -> nettty/open 0 tcp://c0d3.xxx:80
  # -> nettty/send 0 R0VUIC8gSFRUUC8xLjANCg0K
  # -> nettty/recv 0 SFRUUC8xLjEgMjAwIE9LDQpTZXJ2ZXI6IG5naW54LzEuMi4xDQpEYXRlOiBXZWQsIDI4IEF1ZyAyMDEzIDEyOjI4OjIyIEdNVA0KQ29udGVudC1UeXBlOiB0ZXh0L2h0bWwNCkNvbnRlbnQtTGVuZ3RoOiAxNTENCkxhc3QtTW9kaWZpZWQ6IE1vbiwgMDQgT2N0IDIwMDQgMTU6MDQ6MDYgR01UDQpDb25uZWN0aW9uOiBjbG9zZQ0KQWNjZXB0LVJhbmdlczogYnl0ZXMNCg0KPGh0bWw+CjxoZWFkPgo8dGl0bGU+V2VsY29tZSB0byBuZ2lueCE8L3RpdGxlPgo8L2hlYWQ+Cjxib2R5IGJnY29sb3I9IndoaXRlIiB0ZXh0PSJibGFjayI+CjxjZW50ZXI+PGgxPldlbGNvbWUgdG8gbmdpbnghPC9oMT48L2NlbnRlcj4KPC9ib2R5Pgo8L2h0bWw+Cg==
  # -> nettty/term 0

The ``nettty-proxy`` simply exposes a TCP interface to this, which
``netty-connect`` makes use of: ::

  $ nettty-proxy 1080 nettty
  $ echo -ne "GET / HTTP/1.0\r\n\r\n" | nettty-connect /tmp/nettty tcp://c0d3.xxx:80
  HTTP/1.1 200 OK
  ...

That's the idea. :-)

license
=======

GPLv3

author
======

dgvncsz0f
