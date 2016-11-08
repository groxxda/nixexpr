AC_INIT([nixexpr], [0.1], [ried@mytum.de])
AC_CONFIG_HEADERS([config.h])

#AC_CONFIG_MACRO_DIRS([m4])

AM_INIT_AUTOMAKE([1.11 foreign subdir-objects])

AM_PROG_AR
AC_PROG_RANLIB
AC_PROG_CXX
AC_LANG_PUSH([C++])
AC_CONFIG_FILES([Makefile])


AX_CHECK_COMPILE_FLAG(["-std=c++1z"],
                      [ CPPFLAGS+=" -std=c++1z" ],
                      [ echo "C++ compiler cannot compile C++17 code"; exit -1 ])

AC_ARG_VAR(PEGTL, Use custom PEGTL instead of packaged one)

AS_IF([test "x$PEGTL" = "x"],
    [PEGTL=$srcdir/PEGTL])

CPPFLAGS+=" -I$PEGTL"

CXXFLAGS+=" -ftemplate-depth=821"

AC_CHECK_HEADER(pegtl.hh,, AC_MSG_ERROR([required header pegtl.hh is missing]))

AC_OUTPUT


