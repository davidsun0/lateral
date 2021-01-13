#!/usr/bin/python3
import subprocess
import sys
import itertools

def run_suite(cases):
    inputs = [i for i, e in cases]
    expected = [e for i, e in cases]

    p = subprocess.Popen([path], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            universal_newlines=True)
    outs, errs = p.communicate(input='\n'.join(inputs), timeout=1)

    # collect response lines from interpreter
    results = []
    working = ''
    for line in outs.split('\n')[1:]:
        if line.startswith('user>'):
            results.append(working)
            working = ''
        else:
            working += line

    passed = 0
    total = 0

    print('Running test suite...')
    for i, r, e in itertools.zip_longest(inputs, results, expected):
        total += 1
        if r != e:
            # print('===== TEST FAILED =====')
            print('input: ', i)
            print('expected: ', e)
            print('actual: ', r)
            print()
        else:
            passed += 1

    print('=======================')
    if total != passed:
        print('tests failing')
    else:
        print('all tests passed! :)')
    print('{}/{} cases passed'.format(passed, total))
    print('\n\n\n')

def run_test_file(filename):
    with open(filename, "r") as testfile:
        tests = []
        code_lines = []
        expected = None
        
        for line in testfile:
            if line == "\n":
                # join finished test case to list
                tests.append(("".join(code_lines), expected))
                code_lines = []
            elif line[:2] == "; ":
                # cut off ": " and newline char from expected result
                expected = line[2:-1]
            elif line[:2] == ";;":
                pass
            else:
                # cut off newline char from line of code
                code_lines.append(line[:-1])
        tests.append(("".join(code_lines), expected))

        run_suite(tests)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: {} <interpreter path>', sys.argv[0])
        exit(1)
    else:
        global path
        path = sys.argv[1]

    # run_test_file("simple.lisp")
    for testfile in sys.argv[2:]:
        run_test_file(testfile)
