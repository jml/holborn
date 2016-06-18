{ host, port, name, password, runDirectory, keepAlive, maxDelay }:

''
import os

from buildbot_worker.bot import Worker
from twisted.application import service

basedir = '${runDirectory}'

# note: this line is matched against to check that this is a buildslave
# directory; do not edit it.
application = service.Application('buildbot-worker')

# Log to syslog rather than a file somewhere.
from twisted.python import syslog
from twisted.python.log import ILogObserver
application.setComponent(ILogObserver, syslog.SyslogObserver('buildbot-worker').emit)

buildmaster_host = '${host}'
port = ${toString port}
worker_name = '${name}'
passwd = '${password}'

keepalive = ${toString keepAlive}
umask = None
maxdelay = ${toString maxDelay}
# XXX: What does None mean?
numcpus = None
# XXX: maybe should be 'signal'?
allow_shutdown = None

s = Worker(buildmaster_host, port, worker_name, passwd, basedir,
           keepalive, umask=umask, maxdelay=maxdelay,
           numcpus=numcpus, allow_shutdown=allow_shutdown)
s.setServiceParent(application)
''
