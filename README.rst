=====
emaci
=====

.. image:: https://travis-ci.org/storax/emaci.svg?branch=master
   :target: https://travis-ci.org/storax/emaci

.. image:: https://coveralls.io/repos/github/storax/emaci/badge.svg?branch=master
   :target: https://coveralls.io/github/storax/emaci?branch=master

Have you ever been in a dire need for a CI-server but there was none around
and you couldn't setup one yourself (for whatever reason)?

Have you heard youself saying: "Screw it! I'm gonna write my own CI-Server, with blackjack and hookers!"?

Well, even then you probably didn't think: "Yeah, Emacs Lisp is perfect for a CI-Server"
And you are absolutly right. But nevertheless here it is:

  emacsi - a scheduler for shell commands in emacs

.. Note:: This is in a very early alpha. Also this project is just for fun. It doesn't even try to
          replace CI-solutions.

--------
Features
--------

* Queue arbitrary shell commands.
* Execute the queue automatically.

-------
Roadmap
-------

* Pause/Cancel Jobs
* Multiple queues
* Permanent build history
* Build Management buffer
* Convenient bash/zsh functions for submitting jobs from the command line
* Metrics
* Commit hook


----------
Quickstart
----------

After you installed emaci, load it via::

  (require 'emaci)

Use ``emaci/submit-job`` or ``emaci/submit-job-comint`` to queue shell commands.
The queue will get executed immediately. While the queue is running,
you can submit more jobs. Once a job had finished, it will execute the next in the queue.

------
Server
------

Emacs has a built-in server. To start the server call::

  (server-start)

Now you can start emacsclients in any shell, and the clients will
use the running server (e.g. to load faster).
This allows for emaci to be used like a CI-server.
To submit a job via the command line use::

  $ emacsclient --eval "(emaci/submit-job-comint \"$PWD\" \"echo Hello World\")"

.. Note:: You can also use emaci/submit-job and specify the mode of your compilation buffer
          yourself.

It is important that you use double-quotes so the current shell working directory is
replaced in the string. You could wrap this command in a shell function to make
it easier to submit commands.

Unfortunately the current environment is not used.
It's only possible by setting the environment in the shell command argument.
You could however automate this via a shell function.
