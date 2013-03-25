Inliner
=======

A preprocessor for test files that want to include the content of other files, the output of other processes, etc.

Motivation
==========

Writing technical blog posts can be a pain. Frequently, they need to refer to code in separate source trees; they might even need to refer to the output of a chain of processes.

Inliner attempts to solve this by providing a simple syntax for the text file to declare what input it needs, *inline*.

So, a particular usage could be:

    It turns out that all we need to do is call std::move in the constructor:

    !inline(sed -n 5,11p \< move_constructor.cpp)

Let's say this lives in blog.md.il. If we run inliner over this file, it will *inline* the contents of `sed -n 5,11p < move_constructor.cpp`'s stdout in place of the declaration.

*N.B Commands are not executed in a bash shell! If you have trouble, try removing any single quotes from arguments to processes. Literal strings (with double quotes) should work fine if you need to specify an argument containing a space.

Syntax
======

You can use the !inline(...) syntax wherever you need it - it doesn't need to be on its own line, and you can use it many times per line. At the moment, you can even next declarations like so:

!inline(echo foo | !inline(grep foo))

'foo' will be inlined in the place of this declaration.

File references are relative to the current working directory. Programs are found within the inherited PATH.

Building
========

Apologies in advance - you're going to have to build the code locally right now.

Invoke `sbt test publish-local` 

I build with scala 2.9.1/sbt 0.12 at the moment - ymmv with other versions.

Invocation
==========
Once you've built, you'll want a script that looks like:

`java -classpath ~/.sbt/boot/scala-2.9.1/lib/scala-library.jar:~/.ivy2/local/inliner/inliner_2.9.1/0.1/jars/inliner_2.9.1.jar org.grumpysoft.Inliner`

Making this easier is quite near the top of the list. If you look at some of the evil implementation though, you'll see why it's not at the top.

Further examples
================

Please see the specifications in `src/test/`