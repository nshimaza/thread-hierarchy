# thread-hierarchy

[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/nshimaza/thread-hierarchy.svg?branch=master)](https://travis-ci.org/nshimaza/thread-hierarchy)
[![Hackage](https://img.shields.io/hackage/v/thread-hierarchy.svg?style=flat)](https://hackage.haskell.org/package/thread-hierarchy)
[![Stackage Nightly](http://stackage.org/package/thread-hierarchy/badge/nightly)](http://stackage.org/nightly/package/thread-hierarchy)
[![Stackage LTS](http://stackage.org/package/thread-hierarchy/badge/lts)](http://stackage.org/lts/package/thread-hierarchy)

Managing Haskell threads in hierarchical manner.

### Overview

This package provides parent-child association and guaranteed clean-up of children
over plain Haskell thread.  You can terminate all child threads and grand child threads by
just killing their parent thread.

### Motivation

Unlike Unix process, plain Haskell thread, created by forkIO, has no parent-child relation each other.
This means termination of parent thread doesn't result its children also terminated.
This is good design as it is low level API because it gives programmer greatest flexibility.
However, it also means managing entire lifecycle of thread is totally a responsibility of programmer.

Here one thing you need to be aware.  Garbage collection doesn't work on living thread.
When you lost reference to an object, garbage collector frees up the object for you.
However, even though you lost the thread ID of your child thread, Haskel runtime doesn't consider
the thread is orphaned.  The child thread continue running.

This is prone to create thread leakage.  You can accidentally lose thread ID of child thread
by crash of parent thread.  Now you no longer have way to kill orphaned child thread.
This is thread leakage.

The low level forkIO API requires you keep track and manage entire thread lifecycle
including accidental case like the above.  Hand crafting it might be painful.

This package is intended to provide simple replacement API over plain forkIO in case of when
all you need to do on parent termination is just terminating all its children.

If you need to keep your child running after parent terminated, this API is not for you.
 
### Usage 

Almost all you need to know is one data type and one function:  `ThreadMap` and `newChild`.

Threads created by `newChild` guarantee automatic cleanup on its exit
regardless normal exit or cancellation by asynchronous exception.

In order to that works properly, user must ensure following rules.

* User provided thread handler must accept `ThreadMap` as its first argument.
* When the user provided handler creates its child thread, it must use newChild with given `ThreadMap`.
* For 1st thread you create by newChild, give it a `ThreadMap` created by `newThreadMap`.

`ThreadMap` is house-keeping object for your thread.  It is a mutable map keeping track
living child threads of your thread.  When your thread handler started, you receives
an empty `ThreadMap` via 1st argument of your handler.  When you create a child thread
of your thread, `newChild` automatically update (mutate) your `ThreadMap` by adding
newly created child thread.  When your child thread terminated, it is automatically removed
from your `ThreadMap`.

Same manner is applied to child thread and grandchild thread as long as you follow the rule the above.

### How it works

When `newChild` creates a new thread, it installs two cleanup tasks for you.

* Killing all thread contained in `ThreadMap` of the new thread.  It kills all children of the new thread.
* Removing the new thread itself from parent's `ThreadMap`.

The cleanup tasks are called when the new thread exit normally or terminated by asynchronous exception.
By this mechanism, termination of top level thread is propagated down to all its children,
 children of children, and so on.

### Example

When you create the first thread managed by this module, create a new empty `ThreadMap` then call
newChild with it.  The newCall automatically install cleanup routine to the handler you provided.

```haskell
createRootThread :: IO ThreadId
createRootThread = do
    rootThreadMap <- newThreadMap
    threadID <- newChild rootThreadMap rootThreadHandler
    return threadID
```

When a thread managed by this module creates its child thread, call newChild with `TreadMap`
received via handlers argument.

```haskell
rootThreadHandler :: ThreadMap -> IO ()
rootThreadHandler myChildrenThreadMap = do
    void $ newChild myChildrenThreadMap $ \grandChildrenThreadMap -> do
        yourCode
        return ()
```

You can install your own cleanup routine using finally or both resource acquisition and cleanup
routine using bracket.

```haskell
    -- Forking a new child with your own cleanup
    void $ newChild childrenOfCurrentThread $ \childrenOfHandler ->
        yourThreadHandler `finally` yourCleanupRoutine

    -- Forking a new child with resource acquisition and cleanup
    void $ newChild childrenOfCurrentThread $ \childrenOfHandler ->
        bracket yourResourceAcquiringRoutine yourCleanupRoutine yourThreadHandler
```

### Limitation

Currently, unlike async function, this module is not designed to back any return value
from child thread to parent thread.  This module focuses on guaranteed cleanup on thread termination.
