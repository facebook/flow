from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals
from __future__ import print_function
from .engine import PHP5
from .converter import Converter
import unittest
import subprocess
import tempfile
import os
import shutil
import glob
import re

class ConvertHackTestInputsBase(unittest.TestCase):
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
        if not success:
            self.fail(output)
        files = glob.glob(tmpOutput + '/*.php')
        if self.engine.exists():
            unparseable = []
            for f in files:
                fname = os.path.basename(f)
                if fname not in UNPARSEABLE_OUTPUT:
                    (success, output) = self.engine.parse(f)
                    if not success:
                        unparseable.append(fname)
                    msg = "The following files could not be parsed\n" + "\n".join(unparseable)
            self.assertFalse(unparseable, msg)

    def files_with_parse_errors(self, directory):
        proc = subprocess.Popen([
                "grep",
                '-rn',
                r"Parsing\[1002\]",
                "--include",
                "*.exp",
                directory
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            shell=False)
        res = proc.wait()
        output = '\n'.join([l for l in proc.stdout])
        self.assertTrue(res == 0, "grep command failed with error\n" + output)
        lines = output.split("\n")
        get_file_re = re.compile(re.escape(directory) + r"\/([^:]*)\.exp:")
        return [m.group(1) for m in (get_file_re.match(line)
                        for line in lines if line.strip() != '')]

    def delete_unsupported_inputs(self, directory):
        all_files = self.files_with_parse_errors(directory)
        all_files += [f for files in UNSUPPORTED_FILES_UNPARSER.values()
                        for f in files]
        all_files += [f for files in UNSUPPORTED_FILES_CONVERTER.values()
                        for f in files]
        all_files += [f for files in INTERNAL_ERRORS.values()
                        for f in files]
        all_files += OTHER_UNPARSEABLE_FILES
        for f in set(all_files):
            os.remove(os.path.join(directory, f))


UNSUPPORTED_FILES_UNPARSER = {
    "c_is_xhp": [
        'backtick_xhp.php',
        'decl_alias.php',
        'extra_scope.php',
        'functional_xhp.php',
        'typing_ok_xhp.php',
        'xhp_deep_parse.php',
        'xhp_enum_decl.php',
        'xhp_reserved_keywords.php',
        'xhpchild.php',
        'xhpchild_implicit_return.php',
        'xhp_colon.php',
        'xhp_disallowed.php',
    ],
    "Xml": [
        'attr_children.php',
        'dollar_xhp.php',
        'global_const15.php',
        'hh_fixme12.php',
        'hh_fixme13.php',
        'string_expressions12.php',
        'xhp_parse.php',
    ],
    "Call with splat": [
        'parent_construct1.php',
        'unpack_call1.php',
        'unpack_call2.php',
        'unpack_call3.php',
        'unpack_call4.php',
        'unpack_call5.php',
        'unpack_call6.php',
        'unpack_call7.php',
    ]
}

UNSUPPORTED_FILES_CONVERTER = {
    "Unsupported collection type Tuple (Other[0])": [
        "tuple-literal-1.php"
    ],
    "Traits implementing interfaces are currently not supported.  (Other[0])": [
        'IUseDynamicYield_trait1.php',
        'IUseDynamicYield_trait2.php',
        'IUseDynamicYield_trait3.php',
        'IUseDynamicYield_trait4.php',
        'IUseDynamicYield_trait5.php',
        'IUseDynamicYield_trait7.php',
        'IUseDynamicYield_trait8.php',
        'abstract_implement3.php',
        'abstract_implement4.php',
        'abstract_implement5.php',
        'iface_require_incompat6.php',
        'static_in_trait.php',
        'trait_implements.php',
    ],
    "Unsupported collection type \\Map (Other[0])": [
        "namespace_collections1.php",
        "namespace_collections2.php",
    ],
    "Collection initializers in instance variables are currently not supported. (Other[0])": [
        "reset_after_foreach.php",
        "static_collection_init.php"
    ],
    "Unsupported collection type StableMap (Other[0])": [
        "collection_literals.php",
    ],
    "async is currently not supported. (Other[0])": [
        'DynamicYield1.php',
        'DynamicYield2.php',
        'DynamicYield4.php',
        'DynamicYield7.php',
        'DynamicYield_final1.php',
        'DynamicYield_final2.php',
        'DynamicYield_gen1.php',
        'DynamicYield_gen2.php',
        'DynamicYield_gen3.php',
        'DynamicYield_inherit1.php',
        'DynamicYield_inherit2.php',
        'DynamicYield_inherit5.php',
        'DynamicYield_inherit6.php',
        'DynamicYield_inherit7.php',
        'IUseDynamicYield_interface1.php',
        'IUseDynamicYield_interface2.php',
        'IUseDynamicYield_interface3.php',
        'IUseDynamicYield_interface5.php',
        'IUseDynamicYield_interface6.php',
        'IUseDynamicYield_interface7.php',
        'IUseDynamicYield_trait1.php',
        'IUseDynamicYield_trait2.php',
        'IUseDynamicYield_trait3.php',
        'IUseDynamicYield_trait4.php',
        'IUseDynamicYield_trait5.php',
        'IUseDynamicYield_trait6.php',
        'IUseDynamicYield_trait7.php',
        'IUseDynamicYield_trait8.php',
        'anon_yield.php',
        'array_option_unify.php',
        'array_option_unify2.php',
        'async_anon_function_explicit_return.php',
        'async_anon_function_explicit_return_wrong_type.php',
        'async_function_explicit_null_return_wrong_type.php',
        'async_function_explicit_return.php',
        'async_function_explicit_return_nothing.php',
        'async_function_implicit_return.php',
        'async_function_implicit_return2.php',
        'async_function_implicit_return_wrong_type.php',
        'async_function_implicit_return_wrong_type2.php',
        'async_function_explicit_return_type_error.php',
        'async_function_return_types_are_infectious.php',
        'async_function_untyped1.php',
        'async_function_untyped2.php',
        'async_function_untyped3.php',
        'async_function_untyped4.php',
        'async_function_untyped5.php',
        'async_function_untyped6.php',
        'asyncgen1.php',
        'asyncgen10.php',
        'asyncgen11.php',
        'asyncgen2.php',
        'asyncgen3.php',
        'asyncgen4.php',
        'asyncgen5.php',
        'asyncgen6.php',
        'asyncgen7.php',
        'asyncgen8.php',
        'async_lambda.php',
        'async_lambda2.php',
        'async_lambda3.php',
        'await_on_async_function.php',
        'await_on_awaitable.php',
        'await_on_illegal_value.php',
        'await_on_new.php',
        'await_preserves_nullability.php',
        'await_twice.php',
        'awaitable_subtype_error.php',
        'awaitable_yield_result_error.php',
        'awaitable_yield_result_noerror1.php',
        'awaitable_yield_waitfor_error.php',
        'awaitable_yield_waitfor_noerror1.php',
        'continuation_is_not_awaitable1.php',
        'continuation_return.php',
        'fake_members9.php',
        'foreach_kv_list_async.php',
        'foreach_kv_list_async2.php',
        'foreach_list_async.php',
        'foreach_list_async2.php',
        'functional_generator.php',
        'generator_ok_throw.php',
        'genva_preserves_type_signatures1.php',
        'iface_require_extends5.php',
        'instantiate_unresolved2.php',
        'mixed_option.php',
        'namespace_async_1.php',
        'namespace_async_2.php',
        'preparable.php',
        'this_tparam.php',
        'this_tparam2.php',
        'this5.php',
        'try_generator.php',
        'typing_ok_sub_type_continuation.php',
        'unused_awaitable.php',
        'unused_awaitable2.php',
        'unused_awaitable3.php',
        'unused_awaitable4.php',
        'unused_awaitable5.php',
        'unused_awaitable6.php',
        'void_option2.php',
        'weird.php',
        'yield_blank_result.php',
        'yield_implicit_result.php',
        'yield_jan_test_case.php',
        'yield_return3.php',
        'yield_return4.php',
        'yield_wait_forv.php',
        'yield_wait_forv_bad1.php',
        'yield_wait_forv_bad2.php',
        'yield_wait_forv_result.php',
        'yield_wait_forv_result_bad1.php',
        'yield_wait_forv_result_bad2.php',
        'yield_wait_forv_result_bad3.php',
        'yield_wait_forva.php',
        'yield_wait_forva_bad1.php',
        'yield_wait_forvar_bad1.php',
        'yield_wait_forvr.php',
        'yield_wait_forvr_result_bad1.php',
    ],
    "await is currently not supported. (Other[0])": [
        'DynamicYield_inherit6.php',
        'IUseDynamicYield_interface1.php',
        'IUseDynamicYield_interface2.php',
        'IUseDynamicYield_interface3.php',
        'IUseDynamicYield_interface5.php',
        'IUseDynamicYield_trait1.php',
        'IUseDynamicYield_trait2.php',
        'IUseDynamicYield_trait3.php',
        'IUseDynamicYield_trait4.php',
        'IUseDynamicYield_trait5.php',
        'IUseDynamicYield_trait6.php',
        'IUseDynamicYield_trait7.php',
        'IUseDynamicYield_trait8.php',
        'async_function_implicit_return2.php',
        'asyncgen1.php',
        'asyncgen2.php',
        'asyncgen3.php',
        'asyncgen4.php',
        'asyncgen6.php',
        'await_on_async_function.php',
        'await_on_awaitable.php',
        'await_on_awaitable_type_mismatch.php',
        'await_on_illegal_value.php',
        'await_on_new.php',
        'await_on_unresolved.php',
        'await_preserves_nullability.php',
        'await_twice.php',
        'awaitable_subtype_error.php',
        'awaitable_yield_waitfor_error.php',
        'awaitable_yield_waitfor_noerror1.php',
        'fake_members9.php',
        'forbid_await_in_sync_functions1.php',
        'forbid_await_in_sync_functions2.php',
        'forbid_await_in_sync_functions3.php',
        'forbid_await_in_sync_functions4.php',
        'forbid_await_in_sync_functions5.php',
        'functional_generator.php',
        'genva_allows_awaiting_on_multiple_awaitables.php',
        'iface_require_extends5.php',
        'instantiate_unresolved2.php',
        'instantiate_unresolved3.php',
        'preparable.php',
        'this_tparam.php',
        'this_tparam2.php',
        'weird.php',
        'yield_blank_result.php',
        'yield_jan_test_case.php',
        'yield_wait_forv.php',
        'yield_wait_forv_bad1.php',
        'yield_wait_forv_bad2.php',
        'yield_wait_forv_result.php',
        'yield_wait_forv_result_bad1.php',
        'yield_wait_forv_result_bad2.php',
        'yield_wait_forv_result_bad3.php',
        'yield_wait_forva.php',
        'yield_wait_forva_bad1.php',
        'yield_wait_forvar_bad1.php',
        'yield_wait_forvr.php',
        'yield_wait_forvr_result_bad1.php'
    ],
}

INTERNAL_ERRORS = {
    "Naming Error during lambda variable capture": [
        'lambda09.php'
    ]
}

OTHER_UNPARSEABLE_FILES = [
    'capitalization2.php',
    'class_abstract_final_1.php',
    'class_abstract_final_3.php',
    'class_abstract_final_10.php',
    'class_abstract_final_2.php',
    'class_abstract_final_4.php',
    'class_abstract_final_8.php',
    'class_abstract_final_6.php',
    'class_abstract_final_7.php',
    'class_abstract_final_5.php',
    'class_abstract_final_9.php',
    'concat_untyped_with_string.php',
    'covariance7.php',
    'covariance8.php',
    'dynamic_new1.php',
    'dynamic_new3.php',
    'dynamic_new5.php',
    'dynamic_new7.php',
    'enum_3.php',
    'enum_5.php',
    'enum_6.php',
    'fc_enum_case_10.php',
    'fc_enum_case_4.php',
    'hh_fixme3.php',
    'hh_fixme7.php',
    'instanceof_static_with_reqs.php',
    'namespace_global_class1.php',
    'namespace_global_class2.php',
    'namespace_global_function1.php',
    'namespace_global_function2.php',
    'namespace_global_function3.php',
    'namespace_global_function4.php',
    'namespace_global_qualified.php',
    'namespace_use.php',
    'namespace_newtype.php',
    'namespace_use_outside_namespace.php',
    'newtype1.php',
    'newtype2.php',
    'newtype3.php',
    'newtype4.php',
    'newtype5.php',
    'newtype6.php',
    'newtype7.php',
    'newtype_dreeves1.php',
    'nowdoc.php',
    'partial_in_strict.php',
    'printf_crash.php',
    'relaxed_strict_array1.php',
    'shapes_cc_10.php',
    'shapes_cc_11.php',
    'shapes_cc_9.php',
    'shapes_cc_8.php',
    'strict_call_tany1.php',
    'strict_call_tany2.php',
    'strict_call_tany3.php',
    'strict_call_tany4.php',
    'strict_method_call.php',
    'typedef_as6.php',
    'typedef_string_concat1.php',
    'typedef_as5.php',
    'unsafeexpr.php',
    'unsafeexpr2.php',
    'unsafeexpr3.php',
    'variadic_args1.php',
]

UNPARSEABLE_OUTPUT = set([
    'abstract_constructor3.php',
    'array_filter1.php',
    'capitalization1.php',
    'class_extends_trait.php',
    'closure.php',
    'compile_test_cast.php',
    'foreach_ref3.php',
    'global_const10.php',
    'global_const11.php',
    'global_const13.php',
    'global_const14.php',
    'global_const16.php',
    'global_const18.php',
    'global_const20.php',
    'global_const24.php',
    'global_const25.php',
    'global_const26.php',
    'global_const4.php',
    'global_const5.php',
    'global_const6.php',
    'global_const7.php',
    'global_const8.php',
    'global_const9.php',
    'hh_fixme5.php',
    'hhvm_lambda8.php',
    'interface.php',
    'interface_instance_variable.php',
    'interface_static_variable.php',
    'override_final.php',
    'override_final2.php',
    'override_final3.php',
    'override_final_class.php',
    'printf.php',
    'printf2.php',
    'printf3.php',
    'printf4.php',
    'printf5.php',
    'printf7.php',
    'printf_opt_1.php',
    'printf_opt_2.php',
    'recursive_type_expansion.php',
    'storoman1.php',
    'storoman2.php',
    'storoman3.php',
    'string_expressions15.php',
    'subst_implement.php',
    'test_consts4.php',
    'test_consts6.php',
    'test_to_sort_test2.php',
    'this_forbid.php',
    'trait_require_inherit.php',
    'typedef14.php',
    'typing_fail_implement_abstract.php',
    'typing_fail_inherit1.php',
    'typing_fail_inherit2.php',
    'typing_fail_interface_extend_class.php',
    'typing_fail_magic1.php',
    'void_cast.php',
    'yield_return2.php',
    'yield_send1.php',
    'yield_send2.php',
    'yield_send3.php',
    'yield_send4.php',
    'yield_send5.php',
])
