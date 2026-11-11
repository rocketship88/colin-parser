#!/usr/bin/env python3
"""Generate mutants of colon-1.0.tm.

usage: mutate.py colon-1.0.tm mutants.json

Each mutant is one small change to one line (or one statement):
  swap    opcode / operator swapped for its opposite (add<->sub, lt<->gt, ...)
  num     integer literal +1 or -1
  cmp     Tcl comparison or logic operator changed (== -> !=, && -> ||, eq -> ne ...)
  del     a whole statement removed
  neg     if / elseif / while condition negated
  incr    "incr x" changed to "incr x -1"
  opre    one alternative removed from the tokenizer regex
Comment lines, blank lines and the Calc::debug helper are not mutated.
"""
import json, re, sys

SWAPS = [("add","sub"),("mult","div"),("mod","div"),("expon","mult"),("lt","gt"),("le","ge"),
         ("lt","le"),("gt","ge"),("eq","neq"),("bitand","bitor"),("bitxor","bitor"),
         ("lshift","rshift"),("uminus","uplus"),("not","bitnot"),("jumpTrue","jumpFalse"),
         ("dup","pop"),("loadStk","storeStk"),("storeStk","loadStk"),("listIndexImm","listLength"),
         ("incrStkImm","incrImm"),("load","store"),("store","load"),("push","pop"),("over","dup"),
         ("true_res","false_res"),("sub","add")]
CMP = [(" == "," != "),(" != "," == "),(" <= "," < "),(" >= "," > "),(" < "," <= "),(" > "," >= "),
       (" && "," || "),(" || "," && "),(" eq "," ne "),(" ne "," eq "),(" in "," ni "),(" ni "," in ")]
DEL_RE = re.compile(r'^\s*(incr|append|set|lappend|return|error|variable|regsub|continue|break|unset|array set|dict set)\b')

def split_code(line):
    """split a physical line into (code, tail) at a trailing ';#' comment"""
    i = line.find(';#')
    if i >= 0:
        return line[:i], line[i:]
    return line, ''

def brace_group_end(s, start):
    """index of the } matching the { at s[start], or -1"""
    depth = 0
    i = start
    while i < len(s):
        ch = s[i]
        if ch == '\\':
            i += 2; continue
        if ch == '{': depth += 1
        elif ch == '}':
            depth -= 1
            if depth == 0: return i
        i += 1
    return -1

def main(src, out):
    lines = open(src).read().split('\n')
    # skip the debug helper (external diff viewer) entirely
    skip = set()
    for n, l in enumerate(lines):
        if l.startswith('proc debug '):
            j = n
            while j < len(lines) and lines[j] != '}': j += 1
            skip.update(range(n, j+1))
    mutants, seen = [], set()
    def add(n, kind, desc, newlines):
        key = (n, tuple(newlines.items()) if isinstance(newlines, dict) else newlines)
        k = json.dumps([n, sorted(newlines.items())])
        if k in seen: return
        seen.add(k)
        mutants.append({"id": len(mutants)+1, "line": n+1, "kind": kind, "desc": desc,
                        "edits": {str(a): b for a, b in newlines.items()}})
    n = 0
    while n < len(lines):
        line = lines[n]
        st = line.strip()
        if n in skip or not st or st.startswith('#'):
            n += 1; continue
        code, tail = split_code(line)
        # logical statement = physical lines joined by trailing backslashes
        last = n
        while lines[last].rstrip().endswith('\\') and last+1 < len(lines):
            last += 1
        # --- swap
        for a, b in SWAPS:
            for m in re.finditer(r'(?<![\w$:])' + re.escape(a) + r'(?![\w:(])', code):
                add(n, 'swap', f'{a} -> {b}', {n: code[:m.start()] + b + code[m.end():] + tail})
        # --- num
        for m in re.finditer(r'(?<![\w$.\-:])(\d+)(?![\w.])', code):
            v = int(m.group(1))
            add(n, 'num', f'{v} -> {v+1}', {n: code[:m.start()] + str(v+1) + code[m.end():] + tail})
            if v > 0:
                add(n, 'num', f'{v} -> {v-1}', {n: code[:m.start()] + str(v-1) + code[m.end():] + tail})
        # --- cmp
        for a, b in CMP:
            pos = 0
            while True:
                i = code.find(a, pos)
                if i < 0: break
                add(n, 'cmp', f'{a.strip()} -> {b.strip()}', {n: code[:i] + b + code[i+len(a):] + tail})
                pos = i + 1
        # --- del (whole logical statement)
        if DEL_RE.match(code) and last == n:
            probe = code.strip()
            if probe.count('{') == probe.count('}') and not probe.endswith('{'):
                indent = line[:len(line)-len(line.lstrip())]
                add(n, 'del', f'delete: {probe[:60]}', {n: indent + ';# MUTANT deleted'})
        elif DEL_RE.match(code) and last > n:
            ok = True
            blk = '\n'.join(lines[n:last+1])
            if blk.count('{') == blk.count('}'):
                indent = line[:len(line)-len(line.lstrip())]
                edits = {n: indent + ';# MUTANT deleted'}
                for q in range(n+1, last+1): edits[q] = ''
                add(n, 'del', f'delete (multi-line): {code.strip()[:60]}', edits)
        # --- neg
        for m in re.finditer(r'\b(if|elseif|while)\s*\{', code):
            b0 = m.end() - 1
            e = brace_group_end(code, b0)
            if e > 0:
                cond = code[b0+1:e]
                add(n, 'neg', f'negate {m.group(1)} condition', {n: code[:b0] + '{!(' + cond + ')}' + code[e+1:] + tail})
        # --- incr
        m = re.search(r'\bincr (\w+)\s*(?=;|$|\}|\])', code)
        if m and 'incr ' + m.group(1) + ' ' not in code:
            add(n, 'incr', f'incr {m.group(1)} -> incr {m.group(1)} -1', {n: code[:m.end(1)] + ' -1' + code[m.end(1):] + tail})
        # --- tokenizer regex alternatives
        if 'set op_re' in code:
            m = re.search(r'\{(.*)\}', code)
            body = m.group(1)
            alts = re.split(r'(?<!\\)\|', body)
            for k in range(len(alts)):
                rest = alts[:k] + alts[k+1:]
                add(n, 'opre', f'tokenizer: drop alternative {alts[k]}', {n: code[:m.start(1)] + '|'.join(rest) + code[m.end(1):] + tail})
        n += 1
    json.dump(mutants, open(out, 'w'), indent=1)
    kinds = {}
    for m in mutants: kinds[m['kind']] = kinds.get(m['kind'], 0) + 1
    print(len(mutants), 'mutants', kinds)

if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
