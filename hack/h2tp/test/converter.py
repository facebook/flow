from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
import subprocess

class Converter(object):
    def __init__(self, binary_path):
        self.binary_path = binary_path

    def convert(self, in_path, out_path, additional_options):
        command = [
            self.binary_path,
            in_path,
            out_path
        ] + additional_options
        proc = subprocess.Popen(command,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            shell=False)
        res = proc.wait()
        output = '\n'.join([l for l in proc.stdout])
        return (res == 0, output)
