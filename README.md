multischeme
===========

This repo provides a sample implementation of an approach to add preemptive
multitasking support to the Scheme programming language. This is meant as an
alternative and/or complement to SRFI 18.

Language Changes
----------------

Multitasking is provided via a new type called a "task". Tasks are preemptive
(they automatically yield control over to other tasks), but allow for
arbitrarily large, user defined atomic blocks. The support for atomic blocks
removes the need for additional types to synchronize multiple tasks, such as
semaphores and mutexes.

The following new primitive methods are provided:

    * (make-task <thunk>) - Creates a new, running task that invokes
      the given thunk
    * (task? <expr>) - Returns #t if the passed expression is a task,
      otherwise #f
    * (task-live? <expr>) - Returns #t if the passed expression is a
      live (running) task, otherwise #f
    * (task-killed? <expr>) - Returns #t if the passed expression is a
      task that has been killed, otherwise #f
    * (task-done? <expr>) - Returns #t if the passed expression is a
      task that exited normally, otherwise #f
    * (task-kill <task>) - Kills the specified task immediately. Any
      child tasks are also killed. It is an error to call this with
      a non-task value. It is a no-op to call this with an already
      killed task. The return value is the passed in task.

This also introduces the concept of an atomic block; which is evaluated
completely without any context switches occuring during its evaluation. The
general principle of atomic blocks is that they are expressions which are known
at compile time to only take a constant amount of time to evaluate, thus they
can be run to completion without the risk of starving other tasks.

The set of atomic blocks is defined recursively; in a similar manner to how the
language standard defines which calls are tail calls. The following expressions
form atomic blocks:

    * Constant expressions (numbers, characters, quoted expressions, etc.)
    * Begin statements, when all of the nested statements are atomic blocks.
    * Let statements, when the variable values and the body are all atomic
      blocks.
    * If statements, when the test, consequent, and (optional) alternate are
      all atomic blocks.
    * Lambda expressions (but not the application of a lambda).
    * Set expressions, when the value being set is an atomic block.
    * The application of certain primitive procedures, when the arguments
      are also atomic blocks. (The actual list of which procedures to
      consider to be atomic should include all procedures from the standard
      whose running time is not based on the size of its arguments).

Prerequisites
-------------

This implementation assumes the backing Scheme implementation supports
SRFI 9, which adds support for record types, and SRFI 34, which adds
support for exception handling.

Those assumptions are baked into this specific implementation, but the
same multitasking primitives could be implemented without requiring those
extensions.

Status
------

This implementation is still a long way from being compliant with any
version of the Scheme standard, but it is complete enough to bootstrap
itself, and the new primitive procedures all work.

Implementation
--------------

Multitasking is implemented here in terms of a compile time source
transformation.

Ideally, this should be performed some where in the middle
of a Scheme compiler; after macro expansion and the removal of syntactic
sugar, but before code generation. The file src/multitask.scm contains
the code that you would drop into an existing Scheme compiler in order
to add this feature in that way.

This repo also provides a simple compiler frontend to use with the
multitasking transformation, to act as both a proof of concept and for end
users who want to use this with an existing Scheme implementation out of
the box. The frontend is defined in the file src/main.scm, and an example
script showing how to use it with the Chicken Scheme compiler is provided
in the file multischemec.sh.

Gory Details
------------

The multitasking transformation is a variant on a continuation passing
style (CPS) transformation.

In a traditional CPS transformation, the control flow in a program is made
explicit by adding an additional continuation argument to every procedure.
Instead of returning directly, procedures pass their return value to the
continuation.

The important aspect of this is that the control flow in a CPS program is
explicit; every transfer of control within a program has a corresponding
procedure call. Since the transformation makes control flow explicit, we
can modify it slightly in order to allow us to inject arbitrary control
flow in a program.

This is done by making the procedures in the CPS program take two additional
arguments instead of just one; a continuation, and a scheduler. Instead of
passing its result to the continuation, a procedure passes its result and
continuation to the scheduler. The scheduler can then decide what to do
(e.g. continue processing the current task, or switch to another task).