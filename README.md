Indira, Mother of Daemons
=========================

Indira is an application for turning Erlang applications into unix daemons
with as little effort as possible.

Indira is intended for people writing command line interface for Erlang
applications. In the current landscape of Erlang-OS integration this means
mostly package maintainers and sysadmins who want to run their own Erlang
code.

The application is named after Indira Toledo, a character of an excellent S-F
novel titled "Mother of Demons" by Eric Flint.

Why would you want Indira?
--------------------------

Indira is a great help when you want Erlang application to cooperate with
operating system. This is best visible with deploying Erlang code as binary
packages (e.g. DEB or RPM), when the application needs to be stopped
gracefully on system shutdown. Erlang doesn't have support for signal
handlers, so an initscript needs tell the service to stop in a different way.
Unfortunately, configuring distributed Erlang so that the initscript can
connect while sysadmin retains ability to setup Erlang cluster is difficult.
Indira solves this problem by providing separate administrative channel that
can listen on unix socket.

You may also want to use Indira is if you want to configure an application
with INI/TOML/YAML files and command line options, but preserve Erlang's
original way of using application environment. Indira covers parsing command
line arguments and simplifies setting environment parameters from
configuration file.

The things above allow to lower the Erlang proficiency requirements for
sysadmins, give them convenient administrative interface, and distribute
Erlang applications in binary packages in easier and much more elegant way
than before.

Documentation
-------------

Indira is documented using EDoc. A local copy is generated with `make doc`
command to `./doc/` directory. An already generated online copy is available
at <http://dozzie.jarowit.net/api/erlang-indira/>.

Contact and License
-------------------

Indira was written by Stanislaw Klekot <dozzie at jarowit.net>.
The primary distribution point is <http://dozzie.jarowit.net/>, with
a secondary address on GitHub <https://github.com/dozzie/indira>.

Indira is distributed under 3-clause BSD license.
