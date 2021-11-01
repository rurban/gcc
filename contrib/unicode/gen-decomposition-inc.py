from pprint import pprint
import re

from from_glibc.unicode_utils import fill_attributes, UNICODE_ATTRIBUTES

def parse_decomp(buf):
    result = []
    for hexdigits_group in buf.split():
        ch = chr(int(hexdigits_group, 16))
        result.append(ch)
    return result

fill_attributes('UnicodeData.txt')

with open('../../gcc/decomposition.inc', 'w') as f_out:
    f_out.write('/* Generated from UnicodeData.txt\n'
                '   Define the following macros:\n'
                '     BEGIN_SRC(CODEPOINT) : a handler for the codepoint.\n'
                '     DST(CODEPOINT): used to emit each destination codepoint.\n'
                '     END_SRC: finish this src codepoint.  */\n\n')

    for codepoint in UNICODE_ATTRIBUTES:
        attribs = UNICODE_ATTRIBUTES[codepoint]
        if 0:
            pprint(attribs)
        if attribs['decomposition']:
            #print('%r: %r: %r' % (codepoint, attribs['name'], attribs['decomposition']))
            m = re.match(r'(<[a-zA-Z]+>)(.*)', attribs['decomposition'])
            if m:
                # print('has tag: %r' % m.group(1))
                #compat_decomp = parse_decomp(m.group(2))
                # Tagged decomposition mappings are compatibility; skip these
                continue
            else:
                # Untagged decomposition mappings are canonical; write these
                decomp = parse_decomp(attribs['decomposition'])
            f_out.write('/* %s */\n' % attribs['name'])
            f_out.write('BEGIN_SRC (0x%x)\n' % codepoint)
            for dst_chr in decomp:
                f_out.write('  DST (0x%x);\n' % ord(dst_chr))
            f_out.write('END_SRC\n\n')
