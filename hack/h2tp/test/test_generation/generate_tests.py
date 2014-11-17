'''
Since the tool is tested as a black box, we could just write a single
test that runs over all the input files and verifies that converting them
produces the output files.

However we've chosen instead to generate tests. This has the advantage of
allowing us to have the test output interpreted by any CI system, run specific
tests and even run the tests in parallel.

This file generates all the tests by crawling through the input folder,
creating one testcase per folder and one test per file in these folders.

So specifically under input if you have this structure
input
   type_erasure
         class_constant.php
         class_function.php
         class_property.php
         top_level_function.php

then you get this

class Type_Erasure(ConverterTestCase):
    def test_class_constant_php:

    def test_class_function_php:

    def test_class_property_php:

    def test_top_level_function_php:


ConverterTestCase is our base class which contains the functionality of
actually invoking the tool and comparing it's output with the
expected output.


In order to temporarily skip a test, add a file with the same name as the test
with the extension "disabled". The text of the file is used as the reason to
skip the test.

'''

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals
from __future__ import print_function
import os
import optparse
from . import templates

class ConverterTestGenerator:
    def __init__(self, options, prefix, changeHH):
        self.options = options
        self.prefix = prefix
        self.compute_paths()
        self.changeHH = changeHH

    def compute_paths(self):
        self.input_dir = os.path.join(self.options.test_dir, self.prefix + 'input')
        self.output_dir = os.path.join(self.options.test_dir, self.prefix + 'output')
        self.bin_name = getattr(self.options, self.prefix + 'binary')
        self.bin_path = os.path.join(
            self.options.bin_dir,
            self.bin_name)

    def find_input_files(self):
        input_files = {}
        dir_data = os.walk(self.input_dir).next()
        for d in dir_data[1]:
            input_files[d] = {}
            d_path = os.path.join(self.input_dir, d)
            for f in os.listdir(d_path):
                (root, ext) = os.path.splitext(f)
                if (ext == '.disabled'):
                    filedata = input_files[d].get(root, {})
                    with open(os.path.join(d_path, f), "r") as dis_file:
                        filedata['disabled'] = dis_file.read()
                        input_files[d][root] = filedata
                else:
                    filedata = input_files[d].get(f, {})
                    input_files[d][f] = filedata
        for f in dir_data[2]:
            (root, ext) = os.path.splitext(f)
            if (ext == '.opts'):
                #special options for all files in this dir
                dirdata = input_files.get(root, {})
                with open(os.path.join(self.input_dir, f), "r") as opt_file:
                    dirdata['__opts'] = opt_file.read()
        return input_files


    def generate_test_class(self, dirname, files):
        class_name = (self.prefix + dirname).replace(' ', '_').title()
        class_str = templates.CLASS_TMPL.substitute(
            name=class_name,
            bin_path=self.bin_path,
            bin_name=self.bin_name,
            code_dir=self.options.code_dir,
            changeHH=self.changeHH,
            additional_opts=files.get('__opts', ''),
        )
        for f in files:
            if f == '__opts':
                continue
            func_name = os.path.basename(f).replace('.', '_').replace(' ', '_')
            in_path = os.path.relpath(
                os.path.join(self.input_dir, dirname, f),
                self.options.fbcode_dir)
            out_path = os.path.relpath(
                os.path.join(self.output_dir, dirname, f),
                self.options.fbcode_dir)
            if 'disabled' in files[f]:
                class_str += templates.SKIP_TMPL.substitute(
                    reason=files[f]['disabled']
                )

            template = getattr(templates, self.prefix + "FUNC_TMPL")
            class_str += template.substitute(
                name=self.prefix + func_name,
                in_path=os.path.join(self.options.fbcode_dir, in_path),
                out_path=os.path.join(self.options.fbcode_dir, out_path),
                filename=f)
        return class_str

    def generate(self):
        input_files = self.find_input_files()
        test_str = ""
        for d, files in input_files.iteritems():
            test_str += self.generate_test_class(d, files)
        return test_str

class HHVMCollectionTestGenerator:
    def __init__(self, options):
        self.options = options

    def generate(self):
        bin_path = os.path.join(
            self.options.bin_dir,
            self.options.binary)
        test_input_dir = os.path.join(
            self.options.fbcode_dir,
            "hphp/test/slow/collection_classes/")

        return templates.HHVM_COLL_TEST_TMPL.substitute(
            bin_path=bin_path,
            code_dir=self.options.code_dir,
            test_input_dir=test_input_dir)

class HackTestInputTestGenerator:
    def __init__(self, options):
        self.options = options

    def generate(self):
        bin_path = os.path.join(
            self.options.bin_dir,
            self.options.binary)
        test_input_dir = os.path.join(
            self.options.fbcode_dir,
            "hphp/hack/test/typecheck/")

        return templates.HACK_TEST_INPUT_TEST_TMPL.substitute(
            bin_path=bin_path,
            code_dir=self.options.code_dir,
            test_input_dir=test_input_dir)


def parse_args():
    parser = optparse.OptionParser()
    parser.add_option('--fbcode_dir')
    parser.add_option('--code_dir')
    parser.add_option('--install_dir')
    parser.add_option('--binary')
    parser.add_option('--gen_file_name')
    parser.add_option('--unparser_binary')
    (options, args) = parser.parse_args()
    return options

def compute_additional_paths(options):
    options.code_dir = os.path.join(
        options.fbcode_dir,
        options.code_dir
    )
    options.test_dir = os.path.join(options.code_dir, 'test')
    options.gen_file_dir = "%s/test/" % options.install_dir
    options.gen_file_path = os.path.join(
        options.gen_file_dir,
        options.gen_file_name
    )
    options.bin_dir = os.path.join(
        options.fbcode_dir,
        '_bin',
        os.path.relpath(options.code_dir, options.fbcode_dir)
    )

def mkdir_safe(dirname):
    if not(os.path.isdir(dirname)):
        os.mkdir(dirname)


if __name__ == '__main__':
    options = parse_args()
    compute_additional_paths(options)
    generators = [
        ConverterTestGenerator(options, "", True),
        ConverterTestGenerator(options, "unparser_", False),
        HHVMCollectionTestGenerator(options),
        HackTestInputTestGenerator(options),
    ]
    mkdir_safe(options.gen_file_dir)
    f = open(options.gen_file_path, 'w')
    print(templates.FILE_STR, file=f)
    for g in generators:
        print(g.generate(), file=f)
    f.close()
