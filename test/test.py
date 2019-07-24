#!/usr/bin/python3
import subprocess
import sys
import itertools

def runSuite(cases):
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
            print('===== TEST FAILED =====')
            print('input: ', i)
            print('expected: ', e)
            print('actual: ', r)
            # print('=======================')
        else:
            passed += 1

    print('=======================')
    if total != passed:
        print('tests failing')
    else:
        print('all tests passed! :)')
    print('{}/{} cases passed'.format(passed, total))
    print('\n\n\n')

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: {} <interpreter path>', sys.argv[0])
        exit(1)

    global path
    path = sys.argv[1]
    arith = [
            ('12345', '12345'),
            ('(+ 1 1)', '2'),
            ('(+ 1 2 3 4 5)', '15'),
            ('(+ 1 (+ 1 1))', '3'),
            ]

    envir = [
            ('(def x (+ 100 200))', '300'),
            ('x', '300'),
            ('(def x 31415)', '31415'),
            ('x', '31415')
            ]
    
    runSuite(arith)
    runSuite(envir)
    # runTest(['(+ 1 1)'], ['2'])
