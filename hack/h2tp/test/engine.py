from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
import abc
import os
import subprocess

class Engine(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def environ_name(self):
        return

    def cmd(self):
        return os.environ.get(self.environ_name())

    def exists(self):
        return bool(self.cmd())

    def code(self, file_path):
        return "require_once('%s');" % file_path

    def execute_file(self, file_path):
        proc = subprocess.Popen([self.cmd(), '-r', self.code(file_path)],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            shell=False)
        res = proc.wait()
        output = '\n'.join([l for l in proc.stdout])
        return (res == 0, output)

    def parse(self, file_path):
        proc = subprocess.Popen([self.cmd(), '-l', file_path],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            shell=False)
        res = proc.wait()
        output = '\n'.join([l for l in proc.stdout])
        return (res == 0, output)


class PHP5(Engine):
    def __init__(self, preload):
        self.preload = preload

    def environ_name(self):
        return 'ZEND_PHP'

    def code(self, file_path):
        return self.preload + super(PHP5, self).code(file_path)

class HPHP(Engine):
    def environ_name(self):
        return 'HPHP'
