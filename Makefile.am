
noinst_LIBRARIES = libexpr.a

libexpr_a_SOURCES = expr.cc

TESTS = test/test
check_PROGRAMS = test/test
check_LIBRARIES = test/libcatch.a

test_test_SOURCES = test/test.cc
test_libcatch_a_SOURCES = test/catch.cc
test_test_LDADD = test/libcatch.a libexpr.a

bin_PROGRAMS = nixexpr
nixexpr_SOURCES = main.cc
nixexpr_LDADD = libexpr.a

