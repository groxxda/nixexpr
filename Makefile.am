
noinst_LIBRARIES = libexpr.a

libexpr_a_SOURCES = expr.cc

TESTS = test/parser
check_PROGRAMS = $(TESTS)
check_LIBRARIES = test/libcatch.a

test_libcatch_a_SOURCES = test/catch.cc
test_parser_SOURCES = test/parser.cc
test_parser_LDADD = test/libcatch.a libexpr.a

bin_PROGRAMS = nixexpr
nixexpr_SOURCES = main.cc
nixexpr_LDADD = libexpr.a

