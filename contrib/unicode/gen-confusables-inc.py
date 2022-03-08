#!/usr/bin/env python3
#
# Script to generate confusables.inc from confusables.txt
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# http://www.gnu.org/licenses/.  */

#from pprint import pprint
from collections import namedtuple
import re
import sys

class Confusable(namedtuple('Confusable', ['src_chr', 'dst_chrs', 'comment'])):
    pass

class Confusables:
    def __init__(self):
        self.date = None
        self.version = None
        self.items = []

    @staticmethod
    def from_stream(f_in):
        c = Confusables()
        for line in f_in.readlines():
            c.parse_line(line)
        return c

    def parse_line(self, line):
        '''
        Parse a line of confusables.txt as specified in
        http://www.unicode.org/reports/tr39/#Confusable_Detection
        '''
        #print(repr(line))
        m = re.match(r'# Date: (.+)', line)
        if m:
            self.date = m.group(1)
            return
        hexdigits_group = r'([0-9A-F]+)'
        hexdigits_groups = r'([0-9A-F ]+)'
        opt_ws = r'\s*'
        pattern = ('^' + hexdigits_group + opt_ws + ';'
                   + opt_ws + hexdigits_groups + opt_ws + ';'
                   + opt_ws + 'MA' + opt_ws + '#(.*)$')
        m = re.match(pattern, line)
        if m:
            # Convert from hex-encoded codepoint to a unicode character:
            src_chr = chr(int(m.group(1), 16))

            # Convert from space-separated hex-encoded codepoints to a
            # unicode string:
            dst_chrs = ''.join([chr(int(hstr, 16))
                                for hstr in m.group(2).split()])
            comment = m.group(3)
            self.items.append(Confusable(src_chr, dst_chrs, comment))
            #print(repr(self.items[-1]))
            return
        # Verify that we parsed all items
        m = re.match(r'# total: ([0-9]+)', line)
        if m:
            total = int(m.group(1))
            if total != len(self.items):
                raise ValueError('mismatching total: %i; len(self.items): %i'
                                 % (total, len(self.items)))

    def write_as_inc(self, f_out):
        f_out.write('/* Generated from unicode confusables.txt\n'
                    '   Date: %s\n'
                    '   Version: %s */\n' % (self.date, self.version))
        f_out.write('/* Define the following macros:\n'
                    '     BEGIN_SRC(CODEPOINT) : a handler for the codepoint.\n'
                    '     DST(CODEPOINT): used to emit each destination codepoint.\n'
                    '     END_SRC: finish this src codepoint.\n  */')
        for item in self.items:
            f_out.write('/* %s */\n' % item.comment)

            # Verify that the only remappings we see from ASCII are the
            # ones we expect
            if (ord(item.src_chr) < 0x80
                # U+0022 QUOTATION MARK
                and ord(item.src_chr) != 0x22
                # U+0025 PERCENT SIGN
                and ord(item.src_chr) != 0x25
                # U+0030 DIGIT ZERO
                and ord(item.src_chr) != 0x30
                # U+0031 DIGIT ONE
                and ord(item.src_chr) != 0x31
                # U+0049 LATIN CAPITAL LETTER I
                and ord(item.src_chr) != 0x49
                # U+0060 GRAVE ACCENT
                and ord(item.src_chr) != 0x60
                # U+006D LATIN SMALL LETTER M
                and ord(item.src_chr) != 0x6d
                # U+007C VERTICAL LINE
                and ord(item.src_chr) != 0x7c):
                raise ValueError('unexpected remapping from ASCII (0x%x)\n'
                                 % (ord(item.src_chr)))

            # But we ignore all of the ASCII confusables
            if ord(item.src_chr) >= 0x80:
                f_out.write('BEGIN_SRC (0x%x)\n' % (ord(item.src_chr)))
                for dst_chr in item.dst_chrs:
                    f_out.write('  DST (0x%x);\n' % ord(dst_chr))
                f_out.write('END_SRC\n\n')

with open('confusables.txt') as f_in:
    c = Confusables.from_stream(f_in)
with open('../../gcc/confusables.inc', 'w') as f_out:
    c.write_as_inc(f_out)
