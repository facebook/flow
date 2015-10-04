# @lint-avoid-pyflakes3
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
import json
import os

report = json.loads(raw_input())

error_files = set()

# Add all the error file name's base directories to the set to count the
# number of libraries which are not functional completely.

if not report['passed']:
    for error in report['errors']:
        for desc in error['message']:
            error_files.add(os.path.dirname(desc['path']))

print(len(error_files))
