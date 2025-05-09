// { dg-do run { target c++17 } }

// Copyright (C) 2013-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// basic_string_view find

#include <string_view>
#include <testsuite_hooks.h>

void
test01()
{
  typedef std::string_view::size_type csize_type;
  typedef std::string_view::const_reference cref;
  typedef std::string_view::reference ref;
  csize_type npos = std::string_view::npos;
  csize_type csz01, csz02;

  const char str_lit01[] = "mave";
  const std::string_view str01("mavericks, santa cruz");
  std::string_view str02(str_lit01);
  std::string_view str03("s, s");
  std::string_view str04;

  // size_type find(const string_view&, size_type pos = 0) const;
  csz01 = str01.find(str01);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str01, 4);
  VERIFY( csz01 == npos );
  csz01 = str01.find(str02, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str02, 3);
  VERIFY( csz01 == npos );
  csz01 = str01.find(str03, 0);
  VERIFY( csz01 == 8 );
  csz01 = str01.find(str03, 3);
  VERIFY( csz01 == 8 );
  csz01 = str01.find(str03, 12);
  VERIFY( csz01 == npos );

  // An empty string_view consists of no characters
  // therefore it should be found at every point in a string_view,
  // except beyond the end
  csz01 = str01.find(str04, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str04, 5);
  VERIFY( csz01 == 5 );
  csz01 = str01.find(str04, str01.size());
  VERIFY( csz01 == str01.size() );
  csz01 = str01.find(str04, str01.size()+1);
  VERIFY( csz01 == npos );

  // size_type find(const char* s, size_type pos, size_type n) const;
  csz01 = str01.find(str_lit01, 0, 3);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str_lit01, 3, 0);
  VERIFY( csz01 == 3 );

  // size_type find(const char* s, size_type pos = 0) const;
  csz01 = str01.find(str_lit01);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str_lit01, 3);
  VERIFY( csz01 == npos );

  // size_type find(char c, size_type pos = 0) const;
  csz01 = str01.find('z');
  csz02 = str01.size() - 1;
  VERIFY( csz01 == csz02 );
  csz01 = str01.find('/');
  VERIFY( csz01 == npos );
}

constexpr bool
test02()
{
  typedef std::string_view::size_type csize_type;
  typedef std::string_view::const_reference cref;
  typedef std::string_view::reference ref;
  csize_type npos = std::string_view::npos;
  csize_type csz01 = 0, csz02 = 0;

  const char str_lit01[] = "mave";
  const std::string_view str01("mavericks, santa cruz");
  std::string_view str02(str_lit01);
  std::string_view str03("s, s");
  std::string_view str04;

#undef VERIFY
#define VERIFY(x) if(!(x)) return false

  // size_type find(const string_view&, size_type pos = 0) const;
  csz01 = str01.find(str01);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str01, 4);
  VERIFY( csz01 == npos );
  csz01 = str01.find(str02, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str02, 3);
  VERIFY( csz01 == npos );
  csz01 = str01.find(str03, 0);
  VERIFY( csz01 == 8 );
  csz01 = str01.find(str03, 3);
  VERIFY( csz01 == 8 );
  csz01 = str01.find(str03, 12);
  VERIFY( csz01 == npos );

  // An empty string_view consists of no characters
  // therefore it should be found at every point in a string_view,
  // except beyond the end
  csz01 = str01.find(str04, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str04, 5);
  VERIFY( csz01 == 5 );
  csz01 = str01.find(str04, str01.size());
  VERIFY( csz01 == str01.size() );
  csz01 = str01.find(str04, str01.size()+1);
  VERIFY( csz01 == npos );

  // size_type find(const char* s, size_type pos, size_type n) const;
  csz01 = str01.find(str_lit01, 0, 3);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str_lit01, 3, 0);
  VERIFY( csz01 == 3 );

  // size_type find(const char* s, size_type pos = 0) const;
  csz01 = str01.find(str_lit01);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str_lit01, 3);
  VERIFY( csz01 == npos );

  // size_type find(char c, size_type pos = 0) const;
  csz01 = str01.find('z');
  csz02 = str01.size() - 1;
  VERIFY( csz01 == csz02 );
  csz01 = str01.find('/');
  VERIFY( csz01 == npos );

  return true;
}


int
main()
{
  test01();
  static_assert( test02() );

  return 0;
}
