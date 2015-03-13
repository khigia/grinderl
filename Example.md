Goal of those examples is to write a complete documentation of _grinderl_ through some "real case" examples.

**!!! work in progress !!!**

# Principles of grinderl #

  * Basic action in grinderl is a **task**, which is quite similar with a function, except that a task embed the function code, the arguments it will receive, and a description of the results it will give.
  * A **test** wrap a task to define how this task is run on one node: it embed notion of sequence/concurrency and the number of time the task is repeated.
  * A **scenario** describes then when and where some tests are run.


# Problem 1 #

Here is a first example:
  * Let's 2 Erlang nodes N1 and N2.
  * Let's F a function which behave as a server client (send one request and wait one result).
    * F takes one argument, which is a server.
    * F return 2 values which are the time to got the reply, and a server result.
  * 3 server S1, S2, S3 are running.
  * Goal is to run the function F (make a request) 10 times for each server, with an equitable load balancing.


## Implementation of the tests ##

Let's create a test:
```
Test = #test{
    nick = "Run F on S1, S2, S3",
    task = #task{
        task_fun = F,
        arglst = [
            {sequence, [S1, S2, S3]}
        ],
        result_spec = [
            {mean, evaltime},
            {count, servresult}
        ]
    },
    mode = concurrent,
    repeat_n = 15 % 2 worker nodes * 15 function calls (3 servers, 10 requests each)
}.
```


## Run the tests ##
```
erl> grinderl:add_nodes([N1, N2]).
erl> grinderl:run_test(Test).
```


## Get the result ##
...



# Problem 2 #

Q: What if your test is a command line tool liek "wget"?
R: Use some helper to wrap the command line in a function in two steps: (1) make the call passing some parameters, and (2) interpret the raw output of your command.

## Implementation ##

Step 1: Make a task callable accepting one argument (the URL to retrieve)
```
Test = #test{
    nick = "My command line wget",
    task = #task{
        task_fun = grd_extcmd:command_2_taskfun(
            "wget -O -",
            1,
            fun interpret_wget_output/1 # defined bellow
        ),
        arglst = [
            {sequence, [URL1, URL2, URL3]}
        ],
        result_spec = [
            {acc,  wget_data},
            {mean, wget_time}
        ]
    },
    mode = concurrent,
    repeat_n = 15 % 2 worker nodes * 15 function calls (3 servers, 10 requests each)
}.
```

Step 2: Interpret wget output to get two information: HTTP response code and result data
```
interpret_wget_output({0, Data}) ->
    Time = ...,
    Result = ...,
    {ok, self(), [Time, Result]}.
interpret_wget_output({N, Data}) ->
    {error, self(), [N, Data]}.
```