grinderl is an OTP application to run repeated tasks on (remote) Erlang nodes.

It can be used as a simple load testing tool if the repeated task is a server request.

More generally, grinderl:
  * repeat the execution of a task (with possibly different parameters for each execution)
  * following a scenario of repetition
  * on multiple Erlang nodes (may be multiple machine)