#!/bin/bash
if [[ "$OSTYPE" == "darwin"* ]]; then
  _canonicalize_file_path() {
    local dir file
    dir=$(dirname -- "$1")
    file=$(basename -- "$1")
    (cd "$dir" 2>/dev/null && printf '%s/%s\n' "$(pwd -P)" "$file")
  }
  FLOW=$(_canonicalize_file_path "$1")
else
  FLOW=$(readlink -f "$1")
fi
cd "$(dirname "${BASH_SOURCE[0]}")" || exit 1
passed=0
failed=0
skipped=0
filter="$2"
for dir in tests/*/
do
    dir=${dir%*/}
    cd "$dir" || exit 1
    name=${dir##*/}
    exp_file="${name}.exp"
    if [ -e ".flowconfig" ] && [ -e "$exp_file" ] &&
        [[ -z $filter || $name =~ $filter ]]
    then
        # check this dir
        echo "Testing directory: ${name}"
        out_file="${name}.out"

        # Assumption here is that "flow convert" is always successful and we just to check
        # if the files produced are "correct". So we test the newly created declaration files
        # with "flow check" and see if the errors reported are the expected errors.

        "$FLOW" convert  --r . >/dev/null 2>&1
        "$FLOW" check --all --strip-root --show-all-errors --old-output-format 1> "$out_file"
        diff_file="${name}.diff"
        diff -u "$exp_file" "$out_file" > "$diff_file"
        cd declarations || exit 1
        for filename in *.js; do
            diff -u "$filename" "${filename}.exp" >> "../$diff_file"
        done
        cd ..
        if [ -s "$diff_file" ]
        then
            (( failed++ ))
            echo "FAILED: ${name}"
            if [ -t 1 ] ; then
                esc=$(echo -e "\x1b")
                sed \
                    "s/^-/${esc}[31m-/;s/^+/${esc}[32m+/;s/^@/${esc}[35m@/;s/$/${esc}[0m/"
                    < "$diff_file"
            else
                cat "$diff_file"
            fi
        else
            (( passed++ ))
            echo "PASSED: ${name}"
            rm -f "$out_file"
            rm -f "$diff_file"
        fi
    else
        (( skipped++ ))
        echo "Skipping directory: ${name}"
    fi
    cd ../.. || exit 1
done
echo
echo "Passed: ${passed}, Failed: ${failed}, Skipped: ${skipped}"
exit ${failed}
