/* String pool for GCC.
   Copyright (C) 2000-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* String text, identifier text and identifier node allocator.
   Identifiers are uniquely stored in a hash table.

   We use cpplib's hash table implementation.  libiberty's
   hashtab.c is not used because it requires 100% average space
   overhead per string, which is unacceptable.  Also, this algorithm
   is faster.  */

#include "config.h"
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "cpplib.h"
#include "pretty-print.h"
#include "selftest.h"
#include "diagnostic.h"
#include "diagnostic-metadata.h"
#include "gcc-rich-location.h"

struct ht *ident_hash;
struct ht *ident_hash_extra;

static hashnode alloc_node (cpp_hash_table *);
static void stringpool_on_new_hashnode (hashnode);
static void stringpool_on_existing_hashnode (hashnode);
static int mark_ident (struct cpp_reader *, hashnode, const void *);
static void maybe_warn_on_homoglyph (tree id);

static void *
stringpool_ggc_alloc (size_t x)
{
  return ggc_alloc_atomic (x);
}

/* Initialize the string pool.  */
void
init_stringpool (void)
{
  /* Clean up if we're called more than once.
     (We can't make this idempotent since identifiers contain state) */
  if (ident_hash)
    ht_destroy (ident_hash);
  if (ident_hash_extra)
    ht_destroy (ident_hash_extra);

  /* Create with 16K (2^14) entries.  */
  ident_hash = ht_create (14);
  ident_hash->alloc_node = alloc_node;
  ident_hash->alloc_subobject = stringpool_ggc_alloc;
  ident_hash->on_new_node = stringpool_on_new_hashnode;
  ident_hash->on_existing_node = stringpool_on_existing_hashnode;

  /* Create with 64 (2^6) entries.  */
  ident_hash_extra = ht_create (6);
  ident_hash_extra->alloc_node = [] (cpp_hash_table *)
  {
    return HT_NODE (ggc_cleared_alloc<cpp_hashnode_extra> ());
  };
  ident_hash_extra->alloc_subobject = stringpool_ggc_alloc;
}

/* Allocate a hash node.  */
static hashnode
alloc_node (cpp_hash_table *table ATTRIBUTE_UNUSED)
{
  return GCC_IDENT_TO_HT_IDENT (make_node (IDENTIFIER_NODE));
}

/* Allocate and return a string constant of length LENGTH, containing
   CONTENTS.  If LENGTH is -1, CONTENTS is assumed to be a
   nul-terminated string, and the length is calculated using strlen.  */

const char *
ggc_alloc_string (const char *contents, int length MEM_STAT_DECL)
{
  if (length == -1)
    length = strlen (contents);

  if (!length)
    return "";

  char *result = (char *) ggc_alloc_atomic (length + 1);
  memcpy (result, contents, length);
  result[length] = '\0';

  return (const char *) result;
}

/* Print CH to PP; if non-ASCII or non-printable, escape it as \uXXXX
   or \UXXXXXXXX where X are hexadecimal digits.  */

static void
print_escaped_codepoint (pretty_printer *pp, cppchar_t ch)
{
  if (ch < 0x80 && ISPRINT (ch))
    pp_character (pp, ch);
  else if (ch <= 0xffff)
    {
      pp_string (pp, "\\u");
      for (int j = 3; j >= 0; j--)
	pp_character (pp, "0123456789abcdef"[(ch >> (4 * j)) & 0xF]);
    }
  else
    {
      pp_string (pp, "\\U");
      for (int j = 7; j >= 0; j--)
	pp_character (pp, "0123456789abcdef"[(ch >> (4 * j)) & 0xF]);
    }
}

/* Attempt to parse a hexadecimal number from BUF of up to length LEN,
   skipping leading non-digits, and potentially skipping trailing non-digits.
   This is the format within columns of
   https://www.unicode.org/Public/UNIDATA/NormalizationTest.txt

   Return the number of bytes consumed.
   Write the result (if any) to *OUT.  */

static size_t
parse_hex (const char *buf, size_t len, cppchar_t *out)
{
  size_t chars_consumed = 0;
  cppchar_t ch = 0;
  unsigned num_digits = 0;
  while (len > 0)
    {
      unsigned digit = 0;
      switch (buf[0])
	{
	default:
	  /* Not a hex digit.
	     Skip until we we a hex digit, then consume them.  */
	  buf++;
	  chars_consumed++;
	  len--;
	  if (num_digits == 0)
	    continue;
	  else
	    /* First non-hex-digit after a hex digit.
	       Terminate.  */
	    {
	      *out = ch;
	      return chars_consumed;
	    }

	/* Hex digits.  */
	case '0': digit = 0; break;
	case '1': digit = 1; break;
	case '2': digit = 2; break;
	case '3': digit = 3; break;
	case '4': digit = 4; break;
	case '5': digit = 5; break;
	case '6': digit = 6; break;
	case '7': digit = 7; break;
	case '8': digit = 8; break;
	case '9': digit = 9; break;
	case 'a': case 'A': digit = 10; break;
	case 'b': case 'B': digit = 11; break;
	case 'c': case 'C': digit = 12; break;
	case 'd': case 'D': digit = 13; break;
	case 'e': case 'E': digit = 14; break;
	case 'f': case 'F': digit = 15; break;
	}
      /* Handling hex digits.  */
      ch <<= 4;
      ch += digit;
      num_digits++;
      buf++;
      chars_consumed++;
      len--;
    }
  *out = ch;
  return chars_consumed;
}

/* A class for manipulating UTF-32 strings.  */

class utf32_string
{
 public:
  /* Construct an empty utf32_string, preallocated to hold ALLOC_LEN
     codepoints.  */
  utf32_string (size_t alloc_len)
  : m_buf ((cppchar_t *)xmalloc (alloc_len * sizeof (cppchar_t))),
    m_alloc_len (alloc_len),
    m_len (0)
  {
  }

  ~utf32_string () { free (m_buf); }

  size_t get_length () const { return m_len; }

  cppchar_t operator[] (size_t idx) const
  {
    gcc_assert (idx < m_len);
    return m_buf[idx];
  }

  bool operator== (const utf32_string &other) const
  {
    if (m_len != other.m_len)
      return false;
    return memcmp (m_buf, other.m_buf, m_len * sizeof (cppchar_t)) == 0;
  }

  bool operator!= (const utf32_string &other) const
  {
    return !(*this == other);
  }

  void dump_to_pp (pretty_printer *pp) const
  {
    pp_character (pp, '"');
    for (size_t idx = 0; idx < m_len; idx++)
      print_escaped_codepoint (pp, m_buf[idx]);
    pp_character (pp, '"');
    // TODO: length, alloc sz, etc
  }

  DEBUG_FUNCTION void dump () const
  {
    pretty_printer pp;
    pp.buffer->stream = stderr;
    dump_to_pp (&pp);
    pp_newline (&pp);
    pp_flush (&pp);
  }

  static utf32_string
  from_identifier (tree id)
  {
   return from_utf8 ((const uchar *)IDENTIFIER_POINTER (id),
		     IDENTIFIER_LENGTH (id));
  }

  static utf32_string
  from_utf8 (const char *utf8)
  {
    return from_utf8 ((const unsigned char *)utf8, strlen (utf8));
  }

  static utf32_string
  from_utf8 (const unsigned char *buf, size_t len)
  {
    utf32_string result (len);
    size_t idx = 0;
    while (idx < len)
      {
	/* Compute the length of the src UTF-8 codepoint.  */
	int ucn_len = 0;
	uchar ch = buf[idx++];
	for (uchar t = ch; t & 0x80; t <<= 1)
	  ucn_len++;

	cppchar_t src_utf32 = ch & (0x7F >> ucn_len);
	for (int ucn_len_c = 1; ucn_len_c < ucn_len; ucn_len_c++)
	  src_utf32 = (src_utf32 << 6) | (buf[idx++] & 0x3F);

	result.append (src_utf32);
      }
    return result;
  }

  static utf32_string
  from_cppchar_t (cppchar_t ch)
  {
    utf32_string result (1);
    result.append (ch);
    return result;
  }

  /* Convert from space-separated hex-encoded codepoints, as seen
     in the columns of
       https://www.unicode.org/Public/UNIDATA/NormalizationTest.txt
     for example.  */

  static utf32_string
  from_hex (const char *buf, size_t buf_len)
  {
    utf32_string result (3);

    while (buf_len > 0)
    {
      cppchar_t next_ch;
      size_t consumed = parse_hex (buf, buf_len, &next_ch);
      if (consumed > 0)
	result.append (next_ch);
      buf += consumed;
      buf_len -= consumed;
    }

    return result;
  }

  void append (cppchar_t ch)
  {
    ensure_space (m_len + 1);
    m_buf[m_len++] = ch;
  }

  /* Move ctor.  */

  utf32_string (utf32_string &&other)
  : m_buf (other.m_buf),
    m_alloc_len (other.m_alloc_len),
    m_len (other.m_len)
  {
    other.m_buf = NULL;
  }

  /* Move assignment.  */

  utf32_string &operator= (utf32_string &&other)
  {
   free (m_buf);
   m_buf = other.m_buf;
   m_alloc_len = other.m_alloc_len;
   m_len = other.m_len;
   other.m_buf = NULL;
   return *this;
  }

  utf32_string convert_to_nfd () const;

 private:
  void ensure_space (size_t new_len)
  {
    if (m_alloc_len < new_len)
      {
	m_alloc_len = new_len * 2;
	m_buf = (cppchar_t *)xrealloc (m_buf, m_alloc_len * sizeof (cppchar_t));
      }
  }

  void append_decomposition (cppchar_t ch);
  void sort_by_combining_class (size_t start_idx, size_t end_idx);

  cppchar_t *m_buf;
  size_t m_alloc_len;
  size_t m_len;
};

/* A class for constructing UTF-8 encoded strings.
   These are not NUL-terminated.  */

class utf8_string
{
 public:
  utf8_string (size_t alloc_sz)
  : m_buf ((uchar *)xmalloc (alloc_sz)), m_alloc_sz (alloc_sz), m_len (0)
  {
  }
  utf8_string (const utf32_string &other)
  {
    m_alloc_sz = (other.get_length () * 6) + 1;
    m_buf = (uchar *)xmalloc (m_alloc_sz);
    m_len = 0;
    for (size_t idx = 0; idx < other.get_length (); idx++)
      {
	cppchar_t c = other[idx];

	/* Adapted from libcpp/charset.c: one_cppchar_to_utf8.  */
	if (c < 0x80)
	  quick_append (c);
	else
	  {
	    static const uchar masks[6]
	      =  { 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
	    static const uchar limits[6]
	      = { 0x80, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE };
	    size_t nbytes = 1;
	    uchar buf[6], *p = &buf[6];
	    do
	      {
		*--p = ((c & 0x3F) | 0x80);
		c >>= 6;
		nbytes++;
	      }
	    while (c >= 0x3F || (c & limits[nbytes-1]));
	    *--p = (c | masks[nbytes-1]);
	    while (p < &buf[6])
	      quick_append (*p++);
	  }
      }
  }

  ~utf8_string () { free (m_buf); }

  /* Move ctor.  */

  utf8_string (utf8_string &&other)
  : m_buf (other.m_buf),
    m_alloc_sz (other.m_alloc_sz),
    m_len (other.m_len)
  {
    other.m_buf = NULL;
  }

  void dump_to_pp (pretty_printer *pp) const
  {
    pp_character (pp, '"');
    for (size_t idx = 0; idx < m_len; idx++)
      {
	uchar byte = m_buf[idx];
	if (byte < 0x80 && ISPRINT (byte))
	  pp_character (pp, byte);
	else
	  {
	    pp_string (pp, "\\x");
	    for (int j = 1; j >= 0; j--)
	      pp_character (pp,
			    "0123456789abcdef"[(byte >> (4 * j)) & 0xF]);
	  }
      }
    pp_character (pp, '"');
  }

  DEBUG_FUNCTION void dump () const
  {
    pretty_printer pp;
    pp.buffer->stream = stderr;
    dump_to_pp (&pp);
    pp_newline (&pp);
    pp_flush (&pp);
  }

  const uchar *get_buffer () const { return m_buf; }
  size_t get_length () const { return m_len; }

  void quick_append (uchar ch)
  {
    m_buf[m_len++] = ch;
  }

 private:
  uchar  *m_buf;
  size_t m_alloc_sz;
  size_t m_len;
};

/* Append the canonical decomposition of CH to this string.
   Note that this is recursive. For example,
     1E14 has decomposition: 0112 0300
   but
     0112 (LATIN CAPITAL LETTER E WITH MACRON) has decomposition: 0045 0304
   we need to recursively decompose from:
     1e14 to 0112 0300 to (0045 0304) 0300

   FIXME: should we unroll the recursion in the data, or do
   the recursion here?
   Here we're doing it recursively in code (is this necessary,
   to allow for the Hangul cases, or could we unroll it in the data?)  */

void
utf32_string::append_decomposition (cppchar_t ch)
{
  /* Hangul has its own algorithmic rules for decomposition,
     so first deal with these as a special case.

     See 'Hangul Syllable Decomposition' within section
     '3.12 Conjoining Jamo Behavior' of the Unicode standard
     (pp143-145 of version 14.0).

     These variable names are taken directly from the reference
     algorithm in the Unicode standard, hence we slightly diverge
     from GNU variable naming standards for the sake of clarity.  */
  {
    const cppchar_t SBase = 0xAC00;
    const cppchar_t LBase = 0x1100;
    const cppchar_t VBase = 0x1161;
    const cppchar_t TBase = 0x11A7;
    const cppchar_t LCount = 19;
    const cppchar_t VCount = 21;
    const cppchar_t TCount = 28;
    const cppchar_t NCount = 588;
    STATIC_ASSERT (NCount == VCount * TCount);
    const cppchar_t SCount = 11172;
    STATIC_ASSERT (SCount == LCount * NCount);
    if (ch >= SBase && ch < (SBase + SCount))
      {
	const cppchar_t SIndex = ch - SBase;
	const cppchar_t L = LBase + SIndex / NCount;
	const cppchar_t V = VBase + (SIndex % NCount) / TCount;
	const cppchar_t T = TBase + SIndex % TCount;
	append_decomposition (L);
	append_decomposition (V);
	if (T != TBase)
	  append_decomposition (T);
	return;
      }
  }

  /* Otherwise, use the decomposition mappings from the
     Unicode Character Database.  */

  switch (ch)
    {
    default:
      append (ch);
      break;

#define BEGIN_SRC(SRC_CODEPOINT)		\
    case SRC_CODEPOINT:			\
      {

#define DST(CODEPOINT) \
	do { append_decomposition (CODEPOINT); } while (0)

#define END_SRC					\
      }						\
      break;

#include "decomposition.inc"

#undef BEGIN_SRC
#undef DST
#undef END_SRC
    }
}

/* Return the string that results from converting this string to NFD.  */

utf32_string
utf32_string::convert_to_nfd () const
{
  /* Two-pass algorithm.
     FIXME: Should we convert it to a single-pass algorithm?  */
  utf32_string result (m_len);

  /* First pass: replace each canonical composite with
     its canonical decomposition.  */
  for (size_t idx = 0; idx < m_len; idx++)
    {
      const cppchar_t ch = m_buf[idx];
      result.append_decomposition (ch);
    }

  /* Second pass: sort sequences of combining marks by
     combining class.  */
  for (size_t start_idx = 0; start_idx < result.get_length (); )
    {
      if (cpp_combining_class (result[start_idx]) == 0)
	start_idx++;
      else
	{
	  /* Find run of followup chars that also have a nonzero
	     combining class.
	     Specifically, beyond_idx will be the first index after
	     such a run.  */
	  size_t beyond_idx;
	  for (beyond_idx = start_idx + 1;
	       (beyond_idx < result.get_length ()
		&& cpp_combining_class (result[beyond_idx]) != 0);
	       beyond_idx++)
	    {
	      /* Empty.  */
	    }
	  /* Sort this run of combining marks.  */
	  if (beyond_idx > start_idx + 1)
	    result.sort_by_combining_class (start_idx, beyond_idx);
	  start_idx = beyond_idx;
	}
    }

  return result;
}

/* Comparator callback for sorting cppchar_t by combining class.  */

static int
cmp_combining_class (const void *p1, const void *p2)
{
  cppchar_t c1 = *(const cppchar_t *)p1;
  cppchar_t c2 = *(const cppchar_t *)p2;
  return ((int)cpp_combining_class (c1)) - ((int)cpp_combining_class (c2));
}

/* Subroutine of utf32_string::convert_to_nfd: Sort the characters
   in the half-open range [START_IDX, BEYOND_IDX) by combining class.  */

void
utf32_string::sort_by_combining_class (size_t start_idx, size_t beyond_idx)
{
  gcc_stablesort (m_buf + start_idx, beyond_idx - start_idx,
		  sizeof (cppchar_t), cmp_combining_class);
}

/* Subroutine of get_tr39_skeleton, for implementing step 2
   of skeleton(X) from
   http://www.unicode.org/reports/tr39/#Confusable_Detection  */

static utf32_string
convert_homoglyphs_to_exemplars (const utf32_string &in_str)
{
  utf32_string result (in_str.get_length ());
  for (size_t idx = 0; idx < in_str.get_length (); idx++)
    {
      const cppchar_t ch = in_str[idx];
      switch (ch)
	{
	default:
	  result.append (ch);
	  break;

#define BEGIN_SRC(SRC_CODEPOINT)		\
	  case SRC_CODEPOINT:			\
	    {

#define DST(CODEPOINT) \
	  do { result.append (CODEPOINT); } while (0)

#define END_SRC					\
	    }					\
	    break;

#include "confusables.inc"

#undef BEGIN_SRC
#undef DST
#undef END_SRC
	}
    }
  return result;
}

/* Implementation of skeleton(X) from
   http://www.unicode.org/reports/tr39/#Confusable_Detection  */

static utf32_string
get_tr39_skeleton (const utf32_string &in_str)
{
  utf32_string result (in_str.get_length ());
  result = in_str.convert_to_nfd ();
  result = convert_homoglyphs_to_exemplars (result);
  result = result.convert_to_nfd ();
  return result;
}

/* Return an IDENTIFIER_NODE whose name is TEXT (a null-terminated string).
   If an identifier with that name has previously been referred to,
   the same node is returned this time.  */

#undef get_identifier

tree
get_identifier (const char *text)
{
  hashnode ht_node = ht_lookup (ident_hash,
				(const unsigned char *) text,
				strlen (text), HT_ALLOC);

  /* ht_node can't be NULL here.  */
  return HT_IDENT_TO_GCC_IDENT (ht_node);
}

/* Identical to get_identifier, except that the length is assumed
   known.  */

tree
get_identifier_with_length (const char *text, size_t length)
{
  hashnode ht_node = ht_lookup (ident_hash,
				(const unsigned char *) text,
				length, HT_ALLOC);

  /* ht_node can't be NULL here.  */
  return HT_IDENT_TO_GCC_IDENT (ht_node);
}

/* If an identifier with the name TEXT (a null-terminated string) has
   previously been referred to, return that node; otherwise return
   NULL_TREE.  */

tree
maybe_get_identifier (const char *text)
{
  hashnode ht_node;

  ht_node = ht_lookup (ident_hash, (const unsigned char *) text,
		       strlen (text), HT_NO_INSERT);
  if (ht_node)
    return HT_IDENT_TO_GCC_IDENT (ht_node);

  return NULL_TREE;
}

/* Information about the first time a skeleton is used.  */

struct first_use
{
  first_use (tree identifier, location_t loc)
  : m_identifier (identifier), m_loc (loc)
  {}

  /* The first identifier we saw that uses the skeleton.  */
  tree m_identifier;

  /* The location of the identifier.  */
  location_t m_loc;
};

static hash_map<tree, tree> map_identifier_to_skeleton;
static hash_map<tree, first_use> map_skeleton_to_first_use;

/* A class for use when warning about homoglyphs: generate a copy of the
   identifier, but escaping non-printable-ASCII bytes in the UTF-8
   representation as \xNN.  */

class escaped_identifier
{
 public:
  escaped_identifier (tree id)
  {
    gcc_assert (TREE_CODE (id) == IDENTIFIER_NODE);
    utf32_string utf32 (utf32_string::from_identifier (id));
    for (size_t idx = 0; idx < utf32.get_length (); idx++)
      print_escaped_codepoint (&m_pp, utf32[idx]);
  }
  const char *get_str ()
  {
    return pp_formatted_text (&m_pp);
  }

 private:
  pretty_printer m_pp;
};

/* Called when an identifier is first created, and on every identifier
   lookup.

   Ensure that ID has a skeleton, as per
     http://www.unicode.org/reports/tr39/#Confusable_Detection

   The first time we see a new ID, generate the skeleton, and
   if the skeleton is already in use by a different ID, issue
   a -Whomoglyph diagnostic.  */

static void
maybe_warn_on_homoglyph (tree id)
{
  if (!warn_homoglyph)
    return;

  /* If we've already got the skeleton for ID, bail out.
     This ensures that we only warn for the first occurence
     of the identifier.  */
  /* FIXME: this is likely to be slow; is there somewhere we can stash
     this in the identifier itself?  */
  if (map_identifier_to_skeleton.get (id))
    return;

  utf32_string str
    (utf32_string::from_utf8 ((const unsigned char *)IDENTIFIER_POINTER (id),
			      IDENTIFIER_LENGTH (id)));
  utf32_string skel = get_tr39_skeleton (str);
  utf8_string utf8_skel (skel);

  tree skel_id;

  /* The common case is that SKELETON(ID) is ID.  */
  if (utf8_skel.get_length () == IDENTIFIER_LENGTH (id)
      && memcmp (IDENTIFIER_POINTER (id),
		 utf8_skel.get_buffer (),
		 utf8_skel.get_length ()) == 0)
    skel_id = id;
  else
    {
      /* Otherwise, recursively look up the identifier for the skeleton.
	 Get an identifier for the skeleton, but without checking
	 for homoglyphs (avoiding recursion issues).  */
      ident_hash->on_new_node = NULL;
      ident_hash->on_existing_node = NULL;
      skel_id = HT_IDENT_TO_GCC_IDENT (ht_lookup (ident_hash,
						  utf8_skel.get_buffer (),
						  utf8_skel.get_length (),
						  HT_ALLOC));
      ident_hash->on_new_node = stringpool_on_new_hashnode;
      ident_hash->on_existing_node = stringpool_on_existing_hashnode;
    }
  map_identifier_to_skeleton.put (id, skel_id);
  if (first_use *slot = map_skeleton_to_first_use.get (skel_id))
    {
      /* This skeleton has already been used by a diferent identifier;
	 issue a diagnostic.

	 If we simply print both identifiers, the resulting diagnostics
	 are themselved confusing, as they are visually identical in
	 the diagnostics.  Hence we also print escaped versions of the
	 identifiers in the diagnostics for the cases where the identifier
	 is non-equal to the skeleton (which could be one or both of them).  */

      auto_diagnostic_group d;

      /* CWE-1007: 'Insufficient Visual Distinction of Homoglyphs
	 Presented to User' */
      diagnostic_metadata m;
      m.add_cwe (1007);

      gcc_rich_location richloc (input_location);
      richloc.set_escape_on_output (true);
      bool warned;
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif
      if (id == skel_id)
	{
	  warned = warning_meta (&richloc, m, OPT_Whomoglyph,
				 "identifier %qs...",
				 IDENTIFIER_POINTER (id));
	}
      else
	{
	  escaped_identifier escaped_id (id);
	  warned = warning_meta (&richloc, m, OPT_Whomoglyph,
				 "identifier %qs (%qs)...",
				 IDENTIFIER_POINTER (id),
				 escaped_id.get_str ());
	}
#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif
      if (warned)
	{
	  gcc_rich_location slot_richloc (slot->m_loc);
	  slot_richloc.set_escape_on_output (true);
	  if (slot->m_identifier == skel_id)
	    {
	      inform (&slot_richloc,
		      "...confusable with non-equal identifier %qs here",
		      IDENTIFIER_POINTER (slot->m_identifier));
	    }
	  else
	    {
	      escaped_identifier escaped_first_use (slot->m_identifier);
	      inform (&slot_richloc,
		      "...confusable with non-equal identifier %qs (%qs) here",
		      IDENTIFIER_POINTER (slot->m_identifier),
		      escaped_first_use.get_str ());
	    }
	}
    }
  else
    {
      /* Otherwise, this is the first use of this skeleton.  */
      map_skeleton_to_first_use.put (skel_id, first_use (id, input_location));
    }
}

/* Callback for handling insertions into the identifier hashtable.  */

static void stringpool_on_new_hashnode (hashnode ht_node)
{
  tree t = HT_IDENT_TO_GCC_IDENT (ht_node);
  maybe_warn_on_homoglyph (t);
}

/* Callback for handling reuse of identifiers within the
   identifier hashtable.  */

static void stringpool_on_existing_hashnode (hashnode ht_node)
{
  tree t = HT_IDENT_TO_GCC_IDENT (ht_node);
  maybe_warn_on_homoglyph (t);
}

/* Report some basic statistics about the string pool.  */

void
stringpool_statistics (void)
{
  ht_dump_statistics (ident_hash);
}

/* Mark an identifier for GC.  */

static int
mark_ident (struct cpp_reader *pfile ATTRIBUTE_UNUSED, hashnode h,
	    const void *v ATTRIBUTE_UNUSED)
{
  gt_ggc_m_9tree_node (HT_IDENT_TO_GCC_IDENT (h));
  return 1;
}

/* Return true if an identifier should be removed from the table.  */

static int
maybe_delete_ident (struct cpp_reader *pfile ATTRIBUTE_UNUSED, hashnode h,
		    const void *v ATTRIBUTE_UNUSED)
{
  return !ggc_marked_p (HT_IDENT_TO_GCC_IDENT (h));
}

/* Mark the trees hanging off the identifier node for GGC.  These are
   handled specially (not using gengtype) because identifiers are only
   roots during one part of compilation.  */

void
ggc_mark_stringpool (void)
{
  ht_forall (ident_hash, mark_ident, NULL);
  ht_forall (ident_hash_extra,
	     [] (cpp_reader *, hashnode h, const void *)
	     {
	       gt_ggc_m_18cpp_hashnode_extra (h);
	       return 1;
	     }, nullptr);
}

/* Purge the identifier hash of identifiers which are no longer
   referenced.  */

void
ggc_purge_stringpool (void)
{
  ht_purge (ident_hash, maybe_delete_ident, NULL);
  ht_purge (ident_hash_extra,
	    [] (cpp_reader *, hashnode h, const void *) -> int
	    {
	      return !ggc_marked_p (h);
	    }, nullptr);
}

/* Pointer-walking routine for strings (not very interesting, since
   strings don't contain pointers).  */

void
gt_pch_p_S (void *obj ATTRIBUTE_UNUSED, void *x ATTRIBUTE_UNUSED,
	    gt_pointer_operator op ATTRIBUTE_UNUSED,
	    void *cookie ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* PCH pointer-walking routine for strings.  */

void
gt_pch_n_S (const void *x)
{
  gt_pch_note_object (CONST_CAST (void *, x), CONST_CAST (void *, x),
		      &gt_pch_p_S);
}

void
gt_pch_n_S2 (const void *x, size_t string_len)
{
  gt_pch_note_object (CONST_CAST (void *, x), CONST_CAST (void *, x),
		      &gt_pch_p_S, string_len);
}


/* User-callable entry point for marking string X.  */

void
gt_pch_nx (const char *& x)
{
  gt_pch_n_S (x);
}

void
gt_pch_nx (char *& x)
{
  gt_pch_n_S (x);
}

void
gt_pch_nx (unsigned char *& x)
{
  gt_pch_n_S (x);
}

void
gt_pch_nx (unsigned char& x ATTRIBUTE_UNUSED)
{
}

void
gt_pch_nx (unsigned char *x, gt_pointer_operator op, void *cookie)
{
  op (x, NULL, cookie);
}

/* Handle saving and restoring the string pool for PCH.  */

/* SPD is saved in the PCH file and holds the information needed
   to restore the string pool.  */

struct GTY(()) string_pool_data {
  ht_identifier_ptr *
    GTY((length ("%h.nslots"),
	 nested_ptr (union tree_node, "%h ? GCC_IDENT_TO_HT_IDENT (%h) : NULL",
		     "%h ? HT_IDENT_TO_GCC_IDENT (%h) : NULL")))
    entries;
  unsigned int nslots;
  unsigned int nelements;
};

struct GTY (()) string_pool_data_extra
{
  ht_identifier_ptr *
    GTY((length ("%h.nslots"),
	 nested_ptr (cpp_hashnode_extra, "%h ? HT_NODE (%h) : nullptr",
		     "(cpp_hashnode_extra *)%h")))
    entries;
  unsigned int nslots;
  unsigned int nelements;
};

static GTY(()) struct string_pool_data * spd;
static GTY(()) struct string_pool_data_extra *spd2;

/* Save the stringpool data in SPD.  */

void
gt_pch_save_stringpool (void)
{
  spd = ggc_alloc<string_pool_data> ();
  spd->nslots = ident_hash->nslots;
  spd->nelements = ident_hash->nelements;
  spd->entries = ggc_vec_alloc<ht_identifier_ptr> (spd->nslots);
  memcpy (spd->entries, ident_hash->entries,
	  spd->nslots * sizeof (spd->entries[0]));

  spd2 = ggc_alloc<string_pool_data_extra> ();
  spd2->nslots = ident_hash_extra->nslots;
  spd2->nelements = ident_hash_extra->nelements;
  spd2->entries = ggc_vec_alloc<ht_identifier_ptr> (spd2->nslots);
  memcpy (spd2->entries, ident_hash_extra->entries,
	  spd2->nslots * sizeof (spd2->entries[0]));
}

/* Return the stringpool to its state before gt_pch_save_stringpool
   was called.  */

void
gt_pch_fixup_stringpool (void)
{
}

/* A PCH file has been restored, which loaded SPD; fill the real hash table
   from SPD.  */

void
gt_pch_restore_stringpool (void)
{
  ht_load (ident_hash, spd->entries, spd->nslots, spd->nelements, false);
  ht_load (ident_hash_extra, spd2->entries, spd2->nslots, spd2->nelements,
	   false);
  spd = NULL;
  spd2 = NULL;
}

#include "gt-stringpool.h"

#if CHECKING_P

namespace selftest {

/* Implementation detail of ASSERT_DUMP_EQ.  */

static void
assert_dump_eq (const location &loc,
		const utf32_string &str,
		const char *expected)
{
  pretty_printer pp;
  str.dump_to_pp (&pp);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected);
}

static void
assert_dump_eq (const location &loc,
		const utf8_string &str,
		const char *expected)
{
  pretty_printer pp;
  str.dump_to_pp (&pp);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected);
}

/* Assert that STR.dump_to_pp () is EXPECTED.  */

#define ASSERT_DUMP_EQ(STR, EXPECTED) \
  SELFTEST_BEGIN_STMT					   \
  assert_dump_eq ((SELFTEST_LOCATION), (STR), (EXPECTED)); \
  SELFTEST_END_STMT

/* Verify that utf32_string::from_utf8 works as expected.
   Also verify that utf32_string::dump_to_pp works.
   Also verify that utf32 to utf8 works.  */

static void
test_utf32_from_utf8 (void)
{
  /* Empty str.  */
  {
    utf32_string s (utf32_string::from_utf8 (""));
    ASSERT_EQ (s.get_length (), 0);
    ASSERT_DUMP_EQ(s, "\"\"");

    utf8_string s8 (s);
    ASSERT_DUMP_EQ(s8, "\"\"");
  }

  /* Pure ASCII.  */
  {
    utf32_string s (utf32_string::from_utf8 ("hello world"));
    ASSERT_EQ (s.get_length (), 11);
    ASSERT_EQ (s[0], 'h');
    ASSERT_EQ (s[1], 'e');
    ASSERT_EQ (s[2], 'l');
    ASSERT_EQ (s[3], 'l');
    ASSERT_EQ (s[4], 'o');
    ASSERT_EQ (s[5], ' ');
    ASSERT_EQ (s[6], 'w');
    ASSERT_EQ (s[7], 'o');
    ASSERT_EQ (s[8], 'r');
    ASSERT_EQ (s[9], 'l');
    ASSERT_EQ (s[10], 'd');
    ASSERT_DUMP_EQ(s, "\"hello world\"");

    utf8_string s8 (s);
    ASSERT_DUMP_EQ(s8, "\"hello world\"");
  }

  /* 2 bytes per char: a string embedding U+03C0 GREEK SMALL LETTER PI
     which has UTF-8 encoding: 0xCF 0x80.  */
  {
    const char *utf8 = "Happy \xcf\x80 day!";
    utf32_string s (utf32_string::from_utf8 (utf8));
    ASSERT_EQ (s.get_length (), 12);
    ASSERT_EQ (s[0], 'H');
    ASSERT_EQ (s[1], 'a');
    ASSERT_EQ (s[2], 'p');
    ASSERT_EQ (s[3], 'p');
    ASSERT_EQ (s[4], 'y');
    ASSERT_EQ (s[5], ' ');
    ASSERT_EQ (s[6], 0x3c0);
    ASSERT_EQ (s[7], ' ');
    ASSERT_EQ (s[8], 'd');
    ASSERT_EQ (s[9], 'a');
    ASSERT_EQ (s[10], 'y');
    ASSERT_EQ (s[11], '!');
    ASSERT_DUMP_EQ(s, "\"Happy \\u03c0 day!\"");

    utf8_string s8 (s);
    ASSERT_DUMP_EQ(s8, "\"Happy \\xcf\\x80 day!\"");
  }

  /* 3 bytes per char: the Japanese word 'mojibake', written as the
     4 codepoints:
       U+6587 CJK UNIFIED IDEOGRAPH-6587
       U+5B57 CJK UNIFIED IDEOGRAPH-5B57
       U+5316 CJK UNIFIED IDEOGRAPH-5316
       U+3051 HIRAGANA LETTER KE.  */
  {
    const char *utf8 = (/* U+6587 CJK UNIFIED IDEOGRAPH-6587
			   UTF-8: 0xE6 0x96 0x87
			   C octal escaped UTF-8: \346\226\207.  */
			"\346\226\207"

			/* U+5B57 CJK UNIFIED IDEOGRAPH-5B57
			   UTF-8: 0xE5 0xAD 0x97
			   C octal escaped UTF-8: \345\255\227.  */
			"\345\255\227"

			/* U+5316 CJK UNIFIED IDEOGRAPH-5316
			   UTF-8: 0xE5 0x8C 0x96
			   C octal escaped UTF-8: \345\214\226.  */
			 "\345\214\226"

			 /* U+3051 HIRAGANA LETTER KE
			      UTF-8: 0xE3 0x81 0x91
			      C octal escaped UTF-8: \343\201\221.  */
			"\343\201\221");
    utf32_string s (utf32_string::from_utf8 (utf8));
    ASSERT_EQ (s.get_length (), 4);
    ASSERT_EQ (s[0], 0x6587);
    ASSERT_EQ (s[1], 0x5b57);
    ASSERT_EQ (s[2], 0x5316);
    ASSERT_EQ (s[3], 0x3051);
    ASSERT_DUMP_EQ(s, "\"\\u6587\\u5b57\\u5316\\u3051\"");

    utf8_string s8 (s);
    ASSERT_DUMP_EQ(s8, ("\""
			"\\xe6\\x96\\x87"
			"\\xe5\\xad\\x97"
			"\\xe5\\x8c\\x96"
			"\\xe3\\x81\\x91"
			"\""));
  }

  /* 4 bytes per char: the emoji U+1F602 FACE WITH TEARS OF JOY,
     which has UTF-8 encoding '\xf0\x9f\x98\x82'.  */
  {
    const char *utf8 = "pre \xf0\x9f\x98\x82 post";
    utf32_string s (utf32_string::from_utf8 (utf8));
    ASSERT_EQ (s.get_length (), 10);
    ASSERT_EQ (s[0], 'p');
    ASSERT_EQ (s[1], 'r');
    ASSERT_EQ (s[2], 'e');
    ASSERT_EQ (s[3], ' ');
    ASSERT_EQ (s[4], 0x1f602);
    ASSERT_EQ (s[5], ' ');
    ASSERT_EQ (s[6], 'p');
    ASSERT_EQ (s[7], 'o');
    ASSERT_EQ (s[8], 's');
    ASSERT_EQ (s[9], 't');
    ASSERT_DUMP_EQ(s, "\"pre \\U0001f602 post\"");

    utf8_string s8 (s);
    ASSERT_DUMP_EQ(s8, ("\"pre \\xf0\\x9f\\x98\\x82 post\""));
  }
}

/* Verify that utf32_string::from_hex works as expected.  */

static void
test_from_hex ()
{
  const char *buf = "11935 0334 11930";
  utf32_string result (utf32_string::from_hex (buf, strlen (buf)));
  ASSERT_EQ (result.get_length (), 3);
  ASSERT_DUMP_EQ (result, "\"\\U00011935\\u0334\\U00011930\"");
}

/* Verify that cpp_combining_class works as expected.  */

static void
test_combining_classes ()
{
  /* LATIN CAPITAL LETTER A.  */
  ASSERT_EQ (cpp_combining_class (0x41), 0);

  /* COMBINING ACUTE ACCENT.  */
  ASSERT_EQ (cpp_combining_class (0x301), 230);

  /* COMBINING CEDILLA.  */
  ASSERT_EQ (cpp_combining_class (0x327), 202);
}

/* Implementation detail of ASSERT_UTF32_EQ_AT.  */

static void
assert_utf32_eq_at (const location &code_loc,
		    const location &data_loc,
		    const utf32_string &str1,
		    const utf32_string &str2,
		    const char *desc1,
		    const char *desc2)
{
  if (str1 == str2)
    return;
  ::selftest::note_formatted (data_loc,
			      "ASSERT_UTF32_EQ_AT: on this data line");
  fprintf (stderr, "str1: ");
  str1.dump ();
  fprintf (stderr, "str2: ");
  str2.dump ();
  ::selftest::fail_formatted (code_loc,
			      "ASSERT_UTF32_EQ_AT: str1: %s, str2: %s",
			      desc1, desc2);
}

/* Assert that STR1 equals STR2, showing both SELFTEST_LOCATION, DATA_LOC,
   and a dump of the strings on failure.  */

#define ASSERT_UTF32_EQ_AT(DATA_LOC, STR1, STR2)		\
  SELFTEST_BEGIN_STMT							\
  assert_utf32_eq_at ((SELFTEST_LOCATION), (DATA_LOC),			\
		      (STR1), (STR2), (#STR1), (#STR2));		\
  SELFTEST_END_STMT

/* Handle one line from
     https://www.unicode.org/Public/UNIDATA/NormalizationTest.txt
   where PATH is the path to the file,
   LINE_NUM is the 1-based line number within the file.

   Verify that utf32_string::convert_to_nfd works as expected.  */

static void
test_normalization_line (const char *path, unsigned line_num,
			 const char *line_start, size_t line_len)
{
  /* Update LINE_LEN to ignore the first hash character,
     and anything after it.  */
  if (const char *hash = (const char *)memchr (line_start, '#', line_len))
    line_len = hash - line_start;

  if (line_len == 0)
    return;

  /* Ignore the '@PartN' lines.  */
  if (line_start[0] == '@')
    return;

  location data_loc (path, line_num, __FUNCTION__);

  /* Locate columns divided by semicolumn characters.  */
  auto_vec <std::pair <const char *, size_t>> columns (5);
  while (const char *sep = (const char *)memchr (line_start, ';', line_len))
    {
      size_t column_width = sep - line_start;
      columns.safe_push (std::pair <const char *, size_t> (line_start,
							   column_width));
      line_start = sep + 1;
      line_len -= column_width + 1;
    }

  /* We expect each line to have 5 columns.  */
  ASSERT_EQ (columns.length (), 5);

  utf32_string col1 (utf32_string::from_hex (columns[0].first,
					     columns[0].second));
  utf32_string col2 (utf32_string::from_hex (columns[1].first,
					     columns[1].second));
  utf32_string col3 (utf32_string::from_hex (columns[2].first,
					     columns[2].second));
  utf32_string col4 (utf32_string::from_hex (columns[3].first,
					     columns[3].second));
  utf32_string col5 (utf32_string::from_hex (columns[4].first,
					     columns[4].second));

  /* Verify NFD.  */
  /* 'c3 ==  toNFD(c1) ==  toNFD(c2) ==  toNFD(c3)'. */
  ASSERT_UTF32_EQ_AT (data_loc, col3, col1.convert_to_nfd ());
  ASSERT_UTF32_EQ_AT (data_loc, col3, col2.convert_to_nfd ());
  ASSERT_UTF32_EQ_AT (data_loc, col3, col3.convert_to_nfd ());
  /* 'c5 ==  toNFD(c4) ==  toNFD(c5)'. */
  ASSERT_UTF32_EQ_AT (data_loc, col5, col4.convert_to_nfd ());
  ASSERT_UTF32_EQ_AT (data_loc, col5, col5.convert_to_nfd ());
}

/* Call test_normalization_line on each line of a copy of the
   https://www.unicode.org/Public/UNIDATA/NormalizationTest.txt
   conformance test.  */

static void
test_normalization ()
{
  char *path = locate_file ("NormalizationTest.txt");
  char *data = read_file (SELFTEST_LOCATION, path);

  const char *line_start = data;
  unsigned line_num = 1;
  while (1)
    {
      const char *line_end = strchr (line_start, '\n');
      if (line_end == NULL)
	break;
      test_normalization_line (path, line_num,
			       line_start, line_end - line_start);
      line_start = line_end + 1;
      line_num++;
    }

  free (data);
  free (path);
}

/* Verify that get_tr39_skeleton works as expected.  */

static void
test_tr39_skeleton_1 ()
{
  utf32_string s1 (utf32_string::from_utf8 ("sayHello"));
  ASSERT_EQ (s1.get_length (), 8);
  ASSERT_EQ (s1[3], 'H');
  ASSERT_DUMP_EQ(s1, "\"sayHello\"");
  ASSERT_EQ (s1, s1);

  utf32_string skel1 = get_tr39_skeleton (s1);

  /* s1 should map to itself.  */
  ASSERT_EQ (s1, skel1);

  /* "CYRILLIC CAPITAL LETTER EN" (U+041D), with UTF-8 encoding 0xD0 0x9D,
     as opposed to ASCII "H" (U+0048).  */
  utf32_string s2 (utf32_string::from_utf8 ("say" "\xd0\x9d" "ello"));
  ASSERT_EQ (s2.get_length (), 8);
  ASSERT_EQ (s2[3], 0x41d);
  ASSERT_DUMP_EQ(s2, "\"say\\u041dello\"");
  ASSERT_EQ (s2, s2);

  ASSERT_NE (s1, s2);

  utf32_string skel2 = get_tr39_skeleton (s2);
  ASSERT_DUMP_EQ(skel2, "\"sayHello\"");
  ASSERT_NE (s2, skel2);

  /* Both should map to the same skeleton.  */
  ASSERT_EQ (skel1, skel2);
}

/* Example of a single codepoint mapping to multiple codepoints.  */

static void
test_tr39_skeleton_2 ()
{
  utf32_string s1 (utf32_string::from_utf8 ("(v)"));
  ASSERT_EQ (s1.get_length (), 3);
  ASSERT_DUMP_EQ(s1, "\"(v)\"");

  utf32_string skel1 = get_tr39_skeleton (s1);

  /* s1 should map to itself.  */
  ASSERT_EQ (s1, skel1);

  /* "PARENTHESIZED LATIN SMALL LETTER V" (U+24B1) should map to
     LEFT PARENTHESIS, LATIN SMALL LETTER V, RIGHT PARENTHESIS.  */
  utf32_string s2 (utf32_string::from_cppchar_t (0x24b1));
  ASSERT_DUMP_EQ(s2, "\"\\u24b1\"");
  utf32_string skel2 = get_tr39_skeleton (s2);
  ASSERT_DUMP_EQ(skel2, "\"(v)\"");

  /* Both should map to the same skeleton.  */
  ASSERT_EQ (skel1, skel2);
}

/* Both LATIN CAPITAL LETTER H WITH DESCENDER (U+2C67)
   and CYRILLIC CAPITAL LETTER EN WITH DESCENDER (U+04A2)
   should map to LATIN CAPITAL LETTER H, COMBINING VERTICAL LINE BELOW
   (U+0048, U+0329).  */

static void
test_tr39_skeleton_3 ()
{
  utf32_string s1 (utf32_string::from_cppchar_t (0x2c67));
  ASSERT_DUMP_EQ(s1, "\"\\u2c67\"");

  utf32_string s2 (utf32_string::from_cppchar_t (0x04a2));
  ASSERT_DUMP_EQ(s2, "\"\\u04a2\"");

  utf32_string skel1 = get_tr39_skeleton (s1);
  ASSERT_NE (s1, skel1);

  utf32_string skel2 = get_tr39_skeleton (s2);
  ASSERT_NE (s2, skel2);

  /* Both should map to the same skeleton.  */
  ASSERT_EQ (skel1, skel2);
  ASSERT_DUMP_EQ(skel2, "\"H\\u0329\"");
}

static void
test_tr39_skeleton_4 ()
{
  /* U+0124: LATIN CAPITAL LETTER H WITH CIRCUMFLEX
     has canonical decomposition 0048 0302.
     This verifies that we apply NFD to the skeleton.  */
  utf32_string s1 (utf32_string::from_cppchar_t (0x0124));
  ASSERT_DUMP_EQ(s1, "\"\\u0124\"");

  /* "CYRILLIC CAPITAL LETTER EN" (U+041D) as opposed to ASCII "H" (U+0048),
     followed by COMBINING CIRCUMFLEX ACCENT (U+0302).
     This is not in NFC form.  */
  utf32_string s2 (2);
  s2.append (0x041d);
  s2.append (0x0302);
  ASSERT_DUMP_EQ(s2, "\"\\u041d\\u0302\"");

  utf32_string skel1 = get_tr39_skeleton (s1);
  ASSERT_NE (s1, skel1);

  utf32_string skel2 = get_tr39_skeleton (s2);
  ASSERT_NE (s2, skel2);

  /* Both should map to the same skeleton.  */
  ASSERT_EQ (skel1, skel2);
  ASSERT_DUMP_EQ(skel2, "\"H\\u0302\"");
}

static void
test_tr39_skeleton_5 ()
{
  /* U+01C6: LATIN SMALL LETTER DZ WITH CARON
     (has compatibility decomposition 0064 017E).  */
  utf32_string s1 (utf32_string::from_cppchar_t (0x01c6));
  ASSERT_DUMP_EQ(s1, "\"\\u01c6\"");

  /* "LATIN SMALL LETTER D, LATIN SMALL LETTER Z, COMBINING CARON".
     This is not in NFC form.  */
  utf32_string s2 (3);
  s2.append (0x0064); /* LATIN SMALL LETTER D. */
  s2.append (0x007a); /* LATIN SMALL LETTER Z. */
  s2.append (0x030c); /* COMBINING CARON.  */
  ASSERT_DUMP_EQ(s2, "\"dz\\u030c\"");

  utf32_string skel1 = get_tr39_skeleton (s1);
  ASSERT_NE (s1, skel1);
  ASSERT_DUMP_EQ(skel1, "\"dz\\u030c\"");

  utf32_string skel2 = get_tr39_skeleton (s2);
  ASSERT_NE (s2, skel2);
  ASSERT_DUMP_EQ(skel2, "\"dz\\u0306\"");
  ASSERT_NE (skel1, skel2);
}

/* Run all of the selftests within this file.  */

void
stringpool_c_tests ()
{
  test_utf32_from_utf8 ();
  test_from_hex ();
  test_combining_classes ();
  test_normalization ();
  test_tr39_skeleton_1 ();
  test_tr39_skeleton_2 ();
  test_tr39_skeleton_3 ();
  test_tr39_skeleton_4 ();
  test_tr39_skeleton_5 ();
}

} // namespace selftest

#endif /* CHECKING_P */
