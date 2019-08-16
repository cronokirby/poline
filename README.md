# poline
Poline is a toy programming language to illustrate how to implement green threading.

## Example Programs

There are two example programs, in the `examples` folder.
These demonstrate the various features of the language,
and how they behave.

These examples can be run with the following command:

```
cargo run -- examples/everything.poline
cargo run -- examples/spawn-loop.poline
```

## Features
Poline focuses on the green threading and message passing, so
the rest of the language is very very minimal.

The only primitive types in Poline are string litterals
and thread handles.

A poline program is composed of a series of function declarations,
one of which should be named `main`, and acts as the entry point
of the program. Each function declaration contains a series of
statements.

Functions are declared as follows:

```
fn name(arg1, arg2) {
 <statements>
}
```

The following are the different types of statements in poline.

### Print
This statement prints out a litteral, or the contents
of a given variable.

```
print name;
print "litteral";
```

### Call

This statement runs the code in a given function.

```
function_name("litteral_arg", named_arg);
```

### Spawn

This creates a new green thread, which will be running the
given function. This returns a handle we can use to
send messages to that thread.
```
spawn function("litteral", name) as handle;
```


### Send
This sends a message to a given thread.
This requires us to use one of the handles generated from `spawn`.

```
send name to handle;
send "litteral" to handle;
```

### Recv
This receives a message. This will preempt the current thread
if we need to wait for a message to arrive in our mailbox. If we
already have a buffer of messages, the thread will keep executing.

```
recv name;
```

## Shadowing
All variables in poline shadow. Variable names can't be mutated.

