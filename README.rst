=====
emaci
=====

.. image:: https://travis-ci.org/storax/emaci.svg?branch=master
   :target: https://travis-ci.org/storax/emaci

.. image:: https://coveralls.io/repos/github/storax/emaci/badge.svg?branch=master
   :target: https://coveralls.io/github/storax/emaci?branch=master

A queue for shell commands in emacs

**Note**:

  This is in a very early alpha. Also this project is just for fun.

--------
Features
--------

* Queue arbitrary shell commands.
* Execute the queue automatically.
* Send commands from the command line
* Cancel/Kill Jobs

-------
Roadmap
-------

* Multiple queues
* Permanent build history
* Select git branches and stashes for each build
* Build Management buffer
* Translate zsh function to bash
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

Jobs are queued in ``emaci-queue`` and archived in ``emaci-history``.
You can cancel or kill the currently running job
with ``emaci/cancel-job`` or ``emaci/kill-job``. If there are more jobs in the queue,
you have to restart the queue by calling ``emaci/execute-next``.

----
Why?
----

Having a queue for shell commands has a lot of use cases.

I often find myself waiting for multiple code reviews to finish. Often they get completed in bulk and
I have to merge multiple feature branches. Before each merge, I want to merge the latest dev branch into my feature branch and run the tests one last time,
to see if it really does work. Because I have to wait for each test before I can test the next branch, scheduling comes in handy.
Also sometimes I have multiple configs/environments for one project but for various reasons (not in my power to change) can't test them in parallel.

There are already some solutions for emacs like `shell-command-queue.el <https://www.emacswiki.org/emacs/shell-command-queue.el>`_
or `command-queue.el <https://github.com/Yuki-Inoue/command-queue>`_.
But they lack some functionality to use emacs as a simple build management system, when you can't (or don't want to) use
a proper build/test server.

------
Server
------

Emacs has a built-in server. To start the server call::

  (server-start)

Now you can start emacsclients in any shell, and the clients will
use the running server (e.g. to load faster).
This allows for emaci to be used like a CI-server.

To use a convenient (zsh) shell function to send commands to emacs
source ``emaci.zsh`` in the emaci directory or copy and paste it in your ``.zshrc`` or
somewhere on your ``fpath``.

Then you can use::

  $ emaci echo Hello World

~~~~~~~~~
Breakdown
~~~~~~~~~

To submit a job via the command line use::

  $ emacsclient --eval "(emaci/submit-job-comint \"$PWD\" \"echo Hello World\")"

Note:

  You can also use ``emaci/submit-job`` and specify the mode of your compilation buffer yourself.

It is important that you use double-quotes so the current shell working directory is
replaced in the string. You could wrap this command in a shell function to make
it easier to submit commands.

Unfortunately the current environment is not used.
It's only possible by setting the environment in the shell command argument.
You could however automate this via a shell function.

zsh::

  $ emacienv () { echo -e $(declare -px | awk '{if (NR == 1) printf $0;else if ($0 !~ /^typeset -.*/ && last !~ /^typeset -ax.*/) printf "\\n"$0;else printf " && "$0;}{last=$0}')' && ' }

This uses ``declare`` to 'serialize' all environment variables to ``typeset`` commands. These can be evaluated to restore the exact same environment. ``awk`` is used to concatenate the list of commands with ``&&`` so we get a long one-liner. The ``awk`` command uses some logic to preserve newlines in environment variables.

Now we can use this environment replication mechanism to give us a nice command.

zsh::

  $ emaci () { emacsclient --eval "$(echo "(emaci/submit-job-comint \"$PWD\" \"$(emacienv)cd $PWD && $@\")")" }

Note:

  For some reason there is some weird behaviour with the working directory
  if we don't add the ``cd $PWD`` command.

Now we can use this to send shell commands to emacs::

  $ emaci echo Hello World
  $ emaci "./configure && make && make install"
  $ emaci 'echo $PWD'

I find it somehow amusing.
