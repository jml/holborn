{ host, port, name, password, runDirectory, keepAlive, maxDelay }:

''
import os

from buildslave.bot import BuildSlave
from twisted.application import service

basedir = '${runDirectory}'

# note: this line is matched against to check that this is a buildslave
# directory; do not edit it.
application = service.Application('buildslave')

# Log to syslog rather than a file somewhere.
from twisted.python import syslog
from twisted.python.log import ILogObserver
application.setComponent(ILogObserver, syslog.SyslogObserver('buildbot-worker').emit)

buildmaster_host = '${host}'
port = ${toString port}
slavename = '${name}'
passwd = '${password}'

keepalive = ${toString keepAlive}
usepty = False
umask = None
maxdelay = ${toString maxDelay}
# XXX: What does None mean?
numcpus = None
# XXX: maybe should be 'signal'?
allow_shutdown = None

s = BuildSlave(buildmaster_host, port, slavename, passwd, basedir,
               keepalive, usepty, umask=umask, maxdelay=maxdelay,
               numcpus=numcpus, allow_shutdown=allow_shutdown)
s.setServiceParent(application)
''
