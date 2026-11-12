#!/usr/bin/env python3
"""Run the test suites against every mutant.

usage: run_mutants.py SRC_DIR MUTANTS_JSON RESULTS_CSV [--tclsh PATH] [--jobs N] [--only 1,2,3]
                      [--timeout SEC] [--env NAME=VALUE ...]

SRC_DIR holds colon-1.0.tm, calc.test and colon_test.tcl (the files from the zip).
A mutant is KILLED if the output of calc.test or colon_test.tcl differs from the
output of the unmodified module, SURVIVED if neither does. A mutant whose module
will not even load is STILLBORN and is left out of the score. A mutant that makes
a suite hang past the timeout counts as KILLED (timeout).
"""
import argparse, concurrent.futures, csv, hashlib, json, os, re, shutil, subprocess, sys, tempfile, time

ap = argparse.ArgumentParser()
ap.add_argument('src'); ap.add_argument('mutants'); ap.add_argument('results')
ap.add_argument('--tclsh', default='tclsh')
ap.add_argument('--jobs', type=int, default=2)
ap.add_argument('--timeout', type=int, default=30)
ap.add_argument('--only', default='')
ap.add_argument('--env', action='append', default=[])
ap.add_argument('--progress', default='')
ap.add_argument('--resume', action='store_true', help='skip mutants already in RESULTS.partial')
args = ap.parse_args()

env = dict(os.environ)
for kv in args.env:
    k, v = kv.split('=', 1); env[k] = v

TIMING = re.compile(r'microseconds|^after \d|^expr |^proc  |^proc= ')

def strip_timing(text):
    # the timing section at the end of colon_test.tcl is only noise for this purpose
    a = text.find('puts "\\n========== timing tests ========== "')
    b = text.find('} ;# end doalltests')
    if a >= 0 and b > a:
        text = text[:a] + text[b:]
    return text

def normalise(out):
    keep = []
    for l in out.splitlines():
        if TIMING.search(l): continue
        l = re.sub(r'0x[0-9a-f]{6,}', '0xADDR', l)
        keep.append(l)
    return '\n'.join(keep)

def run_suite(d, script):
    try:
        p = subprocess.run([args.tclsh, script], cwd=d, env=env, capture_output=True, text=True,
                           timeout=args.timeout, errors='replace')
        return normalise(p.stdout + '\n--stderr--\n' + p.stderr), False
    except subprocess.TimeoutExpired:
        return 'TIMEOUT', True

def loads(d):
    p = subprocess.run([args.tclsh], input='source colon-1.0.tm\n', cwd=d, env=env,
                       capture_output=True, text=True, timeout=args.timeout)
    return p.returncode == 0 and not p.stderr.strip()

def prepare(d, module_text):
    shutil.copy(os.path.join(args.src, 'calc.test'), d)
    t = open(os.path.join(args.src, 'colon_test.tcl')).read()
    open(os.path.join(d, 'colon_test.tcl'), 'w').write(strip_timing(t))
    open(os.path.join(d, 'colon-1.0.tm'), 'w').write(module_text)

def signature(d):
    a, ta = run_suite(d, 'calc.test')
    b, tb = run_suite(d, 'colon_test.tcl')
    return a, b, ta, tb

def first_diff(base, got):
    bl, gl = base.splitlines(), got.splitlines()
    for x in gl:
        if x not in bl:
            return x.strip()[:100]
    return ''

root = tempfile.mkdtemp(prefix='mut-')
module = open(os.path.join(args.src, 'colon-1.0.tm')).read().split('\n')

# baseline, three times, must be stable
base = None
for i in range(3):
    d = os.path.join(root, 'base%d' % i); os.makedirs(d)
    prepare(d, '\n'.join(module))
    s = signature(d)
    if base is None: base = s
    elif s[:2] != base[:2]:
        print('baseline output is not stable between runs, cannot continue'); sys.exit(2)
print('baseline stable;', len(base[0].splitlines()), 'and', len(base[1].splitlines()), 'lines of output')

mutants = json.load(open(args.mutants))
if args.only:
    want = set(int(x) for x in args.only.split(','))
    mutants = [m for m in mutants if m['id'] in want]

def do(m):
    lines = list(module)
    for k, v in m['edits'].items():
        lines[int(k)-1 if False else int(k)] = v
    text = '\n'.join(lines)
    d = os.path.join(root, 'm%d' % m['id']); os.makedirs(d)
    prepare(d, text)
    t0 = time.time()
    try:
        if not loads(d):
            status, by = 'STILLBORN', ''
        else:
            a, b, ta, tb = signature(d)
            if ta or tb:
                status, by = 'KILLED', 'timeout'
            elif a != base[0]:
                status, by = 'KILLED', 'calc.test ' + first_diff(base[0], a)
            elif b != base[1]:
                status, by = 'KILLED', 'colon_test.tcl ' + first_diff(base[1], b)
            else:
                status, by = 'SURVIVED', ''
    except Exception as e:
        status, by = 'ERROR', str(e)
    shutil.rmtree(d, ignore_errors=True)
    return m, status, by

rows, done, t_start = [], 0, time.time()
counts = {}
partial = args.results + '.partial'
if args.resume and os.path.exists(partial):
    seen = set()
    for line in open(partial):
        r = json.loads(line)
        seen.add(r[0]); rows.append(tuple(r)); counts[r[4]] = counts.get(r[4], 0) + 1
    done = len(rows)
    mutants = [m for m in mutants if m['id'] not in seen]
elif os.path.exists(partial):
    os.remove(partial)
total_all = len(mutants) + done
with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as ex:
    futs = [ex.submit(do, m) for m in mutants]
    for f in concurrent.futures.as_completed(futs):
        m, status, by = f.result()
        done += 1
        counts[status] = counts.get(status, 0) + 1
        rows.append((m['id'], m['line'], m['kind'], m['desc'], status, by))
        with open(partial, 'a') as pf:
            pf.write(json.dumps(rows[-1]) + '\n')
        if args.progress and (done % 20 == 0 or done == total_all):
            open(args.progress, 'w').write(json.dumps({'done': done, 'total': total_all, 'counts': counts,
                                                       'elapsed': int(time.time() - t_start)}))
rows.sort()
with open(args.results, 'w', newline='') as fh:
    w = csv.writer(fh)
    w.writerow(['id', 'line', 'kind', 'mutation', 'status', 'killed_by'])
    w.writerows(rows)
shutil.rmtree(root, ignore_errors=True)
k = counts.get('KILLED', 0); s = counts.get('SURVIVED', 0)
print('done', counts, 'score %.1f%% of %d valid mutants' % (100.0 * k / max(1, k + s), k + s))
