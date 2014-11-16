from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals
from __future__ import print_function
from .engine import PHP5
from .converter import Converter
import unittest
import os
import tempfile
import difflib
import shutil
import glob
import re

class ConvertHHVMCollectionBase(unittest.TestCase):
    def setUp(self):
        self.converter = Converter(self.binary_path())
        self.engine = PHP5(self.execution_prefix())

    def verify(self):
        tmp = tempfile.mkdtemp()
        tmpInput = os.path.join(tmp, 'input')
        shutil.copytree(self.testsDir(), tmpInput)
        self.delete_unsupported_inputs(tmpInput)
        tmpOutput = os.path.join(tmp, 'output')

        (success, output) = self.converter.convert(tmpInput, tmpOutput, [])

        if success:
            files = (glob.glob(tmpOutput + '/*.php') +
                glob.glob(tmpOutput + '/**/*.php'))
            if self.engine.exists():
                self.check_files(tmpOutput, files)
            # if we get here without errors, delete the tree
            shutil.rmtree(tmp)
        else:
            self.fail("error converting hhvm collection tests\n" + output)

    def delete_unsupported_inputs(self, dir):
        all_files = [f for files in UNSUPPORTED_INPUTS.values() for f in files]
        all_files += [f for files in MISMATCHED_FILES.values() for f in files]
        for f in all_files:
            os.remove(os.path.join(dir, f))

    def normalize(self, string):
        string = string.lstrip()
        for (patt, repl) in NORMALIZING_TUPLES:
            string = re.sub(patt, repl, string)
        # add a trailing newline so that its easier to see the diff
        return string.rstrip() + "\n"

    def execute_file(self, f):
        (success, output) = self.engine.execute_file(f)
        output = self.normalize(output)
        return re.sub(SET_RE, fix_set_output, output)

    def check_files(self, outdir, files):
        for f in files:
            actual_output = self.execute_file(f)
            exp_f = f + '.expect' if os.path.isfile(f + '.expect') else (f +
                '.expectf')
            with open(exp_f, "r") as exp_fo:
                expected_output = self.normalize(exp_fo.read())
                if (actual_output != expected_output):
                    message = ''.join(difflib.unified_diff(
                        expected_output.splitlines(True),
                        actual_output.splitlines(True)))
                    self.fail(
                        "Output not expected at " +
                        f +
                        " :\n"
                        + message)

#non matching files
MISMATCHED_FILES = {
    "array functions operating on collections": [
        'array-diff-1.php',
        'array-diff-2.php',
        'array-intersect-1.php',
        'array-intersect-2.php',
        'array_fill_keys.php',
        'array-unshift.php',
        'array-filter.php',
        'array_reverse.php',
        'array-diff-key-1.php',
        'array-diff-key-2.php',
        'array-intersect-key-1.php',
        'array-intersect-key-2.php',
        'array-shift.php',
        'array_combine.php',
        'array_slice.php',
        '841.php',
        'call-user-func-array.php',
        'implode.php',
        'array-typehint.php',
        'set-sort.php',
        'array_flip.php',
        'array-pop.php',
        'array_push.php',
        'array_keys.php',
        'array_search_funcs.php',
        'array_chunk.php',
        'array-map.php',
        'frozen_vector/materialization.php',
        'frozen_vector/array_key_exists.php',
        'frozen_map/api.php',
        'frozen_map/constructors.php',
        'frozen_map/materialization.php',
        'array_values.php',
        'collection-isset.php'
    ],
    "dependence on internal implementation -- var_export, serialize": [
        '836.php',
        '809.php',
        'frozen_set/serialization.php',
        'frozen_set/cow_large.php',
        'migration/unserialize-pair.php',
        'migration/unserialize-set.php',
        'migration/unserialize-vector.php',
        'migration/unserialize-map.php',
        'frozen_vector/cow_large.php',
        'vector_map_private.php',
    ],
    "custom var_dump for string vs int keys": [
        'dup_keys.php',
        'set-brackets-at-get.php',
        'set-invalid-operations.php',
    ],
    "detection of indirect access": [
        'invalid-operations.php'
    ],
    "requires apc functions": [
        '839.php',
        '840.php'
    ],
}


#map of all files that we cannot convert at the moment
UNSUPPORTED_INPUTS = {
    'parse error': [
        '806.php',
        '807.php',
        '808.php',
        '813.php',
        '814.php',
        '815.php',
        '823.php',
        'concat.php',
        'empty_vector.php',
        'imm-iteration-bug.php',
    ],
    'PHP files are not supported': [
        'iterator-clone-bug.php',
        'preg-replace.php',
        'set-addall-bug.php',
    ],
    'c_user_attributes not supported': [
        'extend-collection-test.php'
    ],
    'instance initializers not supported': [
        'deep_copy.php',
        'prop_init_literal_bug.php',
        'prop_init_trait_bug.php'
    ]
}


def fix_set_output(m):
    middle = m.group(3).split('\n')
    middle = '\n'.join(middle[1::2])
    middle = '\n' + middle if middle != '' else middle
    return "object(" + m.group(1) + ")" + m.group(2) + middle + "\n}"


SET_RE = re.compile("object\((HH\\\\(?:Imm)?Set)\)(.*)\n([^\}]*)\}")
NORMALIZING_TUPLES = [
    # indirect access can only be prevented by regular php code, we cannot
    # detect it and throw errors. However, in that case the system will
    # trigger an error with the level E_NOTICE
    (re.compile("PHP Notice:  Illegal member variable name.*"), ""),
    # strip trailing spaces
    (re.compile("\s*\n"), "\n"),
    # replace object ids in var_dumps with the string ID
    (re.compile("(object\(.*\)#)(\d+)(.*)"), r"\1ID\3"),
    # convert warnings and fatal from the user level error strings to system
    # level error strings
    (re.compile("(?:PHP )?(Warning:|Fatal error:)(\s)+(.*in ).*"), r"\1 \3"),
    # delete stack traces
    (re.compile("Stack trace:.*", re.DOTALL), "")
]
