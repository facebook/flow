from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals
from __future__ import print_function
from os import listdir
from os.path import isfile, isdir
from string import Template
from .engine import PHP5, HPHP
from .converter import Converter
import unittest
import os
import difflib
import tempfile
import shutil
import re

class ConverterTestCase(unittest.TestCase):
    def setUp(self):
        self.tmpdir = tempfile.mkdtemp()
        self.PHP5 = PHP5(self.execution_prefix())
        self.HPHP = HPHP()
        self.converter = Converter(self.binary_path())

    def tearDown(self):
        shutil.rmtree(self.tmpdir)

    def compare_execution_files(self, in_tup, out_tup):
        (in_eng, in_file) = in_tup
        (out_eng, out_file) = out_tup
        if not(
                in_eng.exists() and
                out_eng.exists() and
                (os.path.splitext(out_file)[1] == '.php')):
            return
        (success, in_res) = in_eng.execute_file(in_file)
        if not success:
            msg = "error executing file %s \n" % in_file
            self.fail(msg + in_res)
        (success, out_res) = out_eng.execute_file(out_file)
        if not success:
            msg = "error executing file %s \n" % out_file
            self.fail(msg + out_res)
        if (in_res != out_res):
            message = ''.join(difflib.unified_diff(
                in_res.splitlines(True),
                out_res.splitlines(True)))
            self.fail("Generated code output does not match:\n" + message)

    def compare_execution_converted(self, in_path, out_path):
        if isfile(out_path + '.error'):
            return
        if isdir(in_path):
            files = [f for f in listdir(in_path)
                        if isfile(os.path.join(in_path, f))]
            for f in files:
                self.compare_execution_files_converted(
                    os.path.join(in_path, f),
                    os.path.join(out_path, f),
                )
        else:
            self.compare_execution_files_converted(
                in_path,
                out_path,
            )

    def compare_execution_files_converted(self, in_file, out_file):
        self.compare_execution_files((self.HPHP, in_file), (self.PHP5, out_file))

    def compare_execution_unparsed(self, in_file, out_file):
        if isfile(out_file + '.error'):
            return
        file_extension = os.path.splitext(out_file)[1]
        engine = self.PHP5 if file_extension == ".php" else self.HPHP
        self.compare_execution_files((self.HPHP, in_file), (engine, out_file))

    def compare(self, in_path, out_path, filename):
        file_extension = os.path.splitext(filename)[1]
        tmp_path = os.path.join(self.tmpdir, filename)

        options = self.additional_options()
        # hacky code to pass an additional option to the unparser
        # we use the test input extension as a cue for what option we
        # want to pass to the test_unparser
        if self.binary_name() == 'test_unparser':
            options.append("-output-type")
            output_type = "php" if file_extension == ".php" else "hack"
            options.append(output_type)

        (success, output) = self.converter.convert(in_path, tmp_path, options)
        if success:
            self.compare_output(tmp_path, in_path, out_path)
        else:
            self.verify_expected_failure(output, in_path, out_path)

    # this allows one level of nesting. i.e either we compare files,
    # or directories of depth 1. In principle this could be extended to
    # more nesting
    def compare_output(self, actual_out_path, in_path, expected_out_path):
        if isdir(in_path):
            files = [f for f in listdir(in_path)
                        if isfile(os.path.join(in_path, f))]
            for f in files:
                self.compare_output_files(
                    os.path.join(actual_out_path, f),
                    os.path.join(in_path, f),
                    os.path.join(expected_out_path, f)
                )
        else:
            self.compare_output_files(
                actual_out_path,
                in_path,
                expected_out_path
            )

    def compare_output_files(self, actual_out_path, in_path, expected_out_path):
        actual_output = ""
        actual_out_path = self.change_extension_maybe(actual_out_path)
        expected_out_path = self.change_extension_maybe(expected_out_path)
        with open(actual_out_path, "r") as f:
            actual_output = f.read()
            self.check_expected_output(
                actual_output,
                in_path,
                expected_out_path
            )
        file_extension = os.path.splitext(actual_out_path)[1]
        if self.PHP5.exists() and (file_extension == '.php'):
            (success, output) = self.PHP5.parse(actual_out_path)
            if not success:
                self.fail(output)

    def change_extension_maybe(self, path):
        if self.changeHH():
            (root, ext) = os.path.splitext(path)
            if ext == '.hh':
                return root + '.php'
        return path

    def check_expected_output(self, actual_output, in_path, out_path):
        expected_output = ""
        try:
            with open(out_path, "r") as f:
                expected_output = f.read()
        except IOError:
            self.fail("Expected output %s not provided/readable" % out_path)
        expected_output = self.normalize(
            Template(expected_output).safe_substitute(filename=in_path))
        actual_output = self.normalize(actual_output)
        if not actual_output:
            self.fail("No output provided")
        if (expected_output != actual_output):
            message = ''.join(difflib.unified_diff(
                expected_output.splitlines(True),
                actual_output.splitlines(True)))
            self.fail("Output not expected:\n" + message)

    #remove trailing newlines, and trailing spaces on each line
    def normalize(self, string):
        return re.sub("\s*\n", "\n", string).rstrip()

    def verify_expected_failure(self, actual_output, in_path, out_path):
        out_path = out_path + '.error'
        if not isfile(out_path):
            self.fail(actual_output)
            return
        self.check_expected_output(actual_output, in_path, out_path)
