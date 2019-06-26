# -*- coding: utf-8 -*-
"""
Created on Tue Jan 15 11:53:47 2019

@author: BOkola
@title : Difflib library - Helpers for computing deltas
"""

"""
Classes

1. class difflib.SequenceMatcher()

- compares pairs of sequences of any type as longs as the sequence elements are hashable

2. class difflib.Differ

- comparing seduences of lines of text, and producing human-readable differences of deltas.Uses `Sequncematcher` both to compare sequences of lines, and to compare sequences of characters with similar (near matching) lines.

3. class difflib.HtmlDiff()

- creates HTML table(or a complete HTML file containing the table) of a side by side, line by line comparison of text with inter-line and intra-line change highlights utilizing constructors below:
    
    a. _init_(tabsize=8, wrapcolumn=None, linejunk=None, charjunk=IS_CHARACTER_JUNK)
    
    - tabsize an optional argument specifying tab stop spacing defaulting at 8
    - wrapcolumn an optional keyword to specify column number where lines are broken and wrapped, defaults to `None`
    - linejunk and charjunk are optional keyword arguments passed into ndiff() (used by Htmldiff to generate side by side HTML differences). 
The following methods are public:

make_file(fromlines, tolines, fromdesc='', todesc='', context=False, numlines=5, *, charset='utf-8')
 - compares fromlines and tolines(list of stings) and returns a string which is a complete HTML file containing a table showing line by line differences with inter-line and intra-line changes highlighted.
 
 - fromdesc and todesc are optional keyword arguments to specify from/to file column header strings (both default an empty string)
- context and numlines are both optional keyword arguments. Set context to True when contextual differences are to be shown, else the default is False to show the full files

    b. make_table(fromlines, tolines, fromdesc='', todesc='', context=False, numlines=5)

- compares fromlines and tolines (lists of strings) and returns a string which is a complete HTML table showing line by line differences with inter-line and intra-line changes highlighted.

4. difflib.context_diff(a, b, fromfile= ", tofile=", fromfiledate=", tofiledate=", n=3, lineterm = '\n')

    comparea a and b (lists of strings); return a delta (a generator            
    generating the delta lines) in context diff format.
    Context diffs are a compact way of showing just the lines that
    have changed plus a few lines of context.
    The context diff format normally has a header for filenames and
    modification times. Any or all these may be specified using
    strings for fromfile, tofile,fromfiledate and tofiledate.

5. difflib.get_close_matches(word, possibilities, n=3, cutoff=0.6)

    Return a list of the best "good enough" matches. word is a  
    sequnce for which close matches are desired (typically as
    string)    
    and possibilities a list of sequences against which to match
    word (typically a list of strings).
    Optional argument n(default 3, must be greater than 0) is the
    maximum number of close matches to return.
    Optional argument cutoff(default 0.6) is a float in the range
    [0,1].Possibilities that do not score at least that similar
    to word are ignored
    
6. difflib.ndiff(a, b, linejunk=None, charjunk=IS_CHARACTER_JUNK)

    Compare a and b (lists of strings);Optional parameters linejunk     
    and charjunk are filtering functions.
    
7.difflib.restore(sequence, which)
    Return one of the two sequences that generated a delta

8. difflib.unified_diff(a, b, fromfile=", tofile=", fromfiledate=", tofiledate=", n=3, lineterm='\n')

    Compare a and b (lists of strings); return a delta (as  
    generator generating the delta lines) in a unified format; as
    way of just showing the lines that have changed plus a few
    lines of context. The changes are shown in an inline style
    By default, the diff control lines (those with `---`, `+++`, or
   `@@`) are created with a trailing newline. Set the lineterm
   tofile `""` for inputs that do not have trailing newlines.

9. difflib_diff_bytes(dfunc, a, b, fromfile=b", fofile=b", fromfiledate=b", tofiledate=b",n=3, linetermb='\n' )

    compares a and b (lists of bytes objects) using dfunc, yield a
    sequence of delta lines (also bytes) in the format returned by  
    dfunc. dfunc must be callable, either `unified_diff()` or
    `context_diff()`
    
10. difflib.IS_LINE_JUNK(line)

    Return true for ignorable lines (blank lines or lines with a
    single) `#`, otherwise it's not ignorable.
    
11. difflib.IS_CHARACTER_JUNK(ch)

    Returns true for ignorable characters. The character ch is
    ignorable if ch is a space or tab, otherwise not ignorable

SequenceMatcher Objects

1.class difflib_SequenceMatcher(isjunk=None, a=", b=", autojunk=True)

    isjunk must be `None`(the default) or a one-argument function -     
    takes a sequence element and returns true iff thw element is
    'junk' and needs ignoring. Passing `None` is same as passing
    'lambda x: 0`; i.e. no elements are ignored. For example pass
    `lambda x: x in " \t" if you're comparing lines as seq of chars
    and don't want to sync up on blanks or hard tabs.
    a and b are the sequences to be compared, default to empty
    strings
 SequenceMAtcher objects have the following methods

1. set_seqs(a, b) - set the two sequences to be compared.
2. set_seq1(a) - set first seq to be compared, 2nd seq not changed
3. set_seq2(b) - set 2nd seq to be compared, 1st seq not changed
4. find_longest_match(alo,ahi,blo,bhi) - find longest matching block in a[alo:ahi] and b[blo:bhi]

    

  """  





import glob
import pandas as pd
import csv
import glob
import os
import itertools
from itertools import combinations
from difflib import context_diff
from difflib import get_close_matches
import sys

path = r'F:\data\Neonatal'
root, _, rel_filenames = next(os.walk(path))
full_filenames = [os.path.join(root, f) for f in rel_filenames]

"""

compare files


"""

for (file1, file2) in combinations(full_filenames, 2):
    with open(file1) as f1, open(file2) as f2:
         fileone = f1.readlines()
         filetwo = f2.readlines()
         """ 
         1. context_diff(a,b, fromfile=", tofile=", fromfiledaate=",        
         tofiledate=")
 
        """
         sys.stdout.writelines(context_diff(fileone, filetwo,   fromfile='before.py', tofile='after.py'))

"""
2. difflib.get_close_matches(word, possibilities, n=3, cutoff=0.6)
"""
get_close_matches('appel', ['ape', 'apple', 'peach', 'puppy'])
import keyword
?keyword.kwlist
get_close_matches('wheel', keyword.kwlist)
get_close_matches('pineapple', keyword.kwlist)
get_close_matches('accept', keyword.kwlist)

"""
3.ndiff(a, b, linejunk = None, charjunk = IS_CHARACTER_JUNK)
4. difflib.restore(sequence, whichs)
"""
from difflib import ndiff
diff = ndiff('one\ntwo\nthree\n'.splitlines(keepends = True), 'ore\nthre\nemu\n'.splitlines(keepends =True))
print(''.join(diff), end = "")
from difflib import restore 
diff = list(diff)   
print(''.join(restore(diff, 1)), end = "")
print(''.join(restore(diff, 2)), end = "")

"""
5. difflib.unified_diff(a, b, fromfile, tofile)

"""
from difflib import unified_diff
s1 = ['bacon\n', 'eggs\n', 'ham\n', 'guido\n']
s2 = ['python\n', 'eggy\n', 'hamster\n', 'guido\n']
sys.stdout.writelines(unified_diff(s1, s2, fromfile='before.py', tofile='after.py'))

"""
6. sequenceMatcher
"""
from difflib import SequenceMatcher
s = SequenceMatcher(None, " abcd", "abcd abcd")
s.find_longest_match(0, 5, 0, 9)

