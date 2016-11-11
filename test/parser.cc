/*
 * test.cc
 *
 *  Created on: Oct 24, 2016
 *      Author: groxxda
 */
#include "catch.hpp"

#include <pegtl/analyze.hh>
#include <iostream>
#include "ast_helper.hh"
#include "expr.hh"

bool parse_nocatch(const std::string& str, nix::parser::state::base& result) {
    bool res = nix::parser::parse_string(str, result);
    return res;
}

template <typename Str, typename... Args>
bool parse(Str&& str, Args&&... args) {
    try {
        auto res = parse_nocatch(str, args...);
        return res;
    } catch(pegtl::parse_error& e) {
        std::cerr << "Error parsing " << str << ":" << std::endl;
        std::cerr << e.what() << std::endl;
        return false;
    } catch(...) {
        std::cerr << "unknown error parsing " << str << ":" << std::endl;
        return false;
    }
}

template <typename R> R compare(nix::parser::state::base& state) {
    std::stringstream s;
    s << state.value;
    R r;
    s >> r;
    return r;
}

template <> std::string compare(nix::parser::state::base& state) {
    std::stringstream s;
    s << state.value;
    return s.str();
}


template <typename S, typename R> void check(S&& str, const R& expect) {
    SECTION(str) {
        nix::parser::state::base result;
        REQUIRE(parse(str, result));
        REQUIRE(compare<R>(result) == expect);
    }
}

template <typename S> void check(S&& str) { check<>(str, str); }

#define CHECK_AST(expr, ast)                                                   \
    SECTION(expr) {                                                            \
        nix::parser::state::base result;                                       \
        REQUIRE(parse(expr, result));                                          \
        REQUIRE(ast == result.value);                                          \
    }


#if 0
TEST_CASE("grammar analysis") {
    const size_t issues_found = pegtl::analyze<grammar>();
    CHECK(issues_found == 0);
}
#endif

TEST_CASE("comments") {
    check("1#single line comment", 1);
    check("4/* multi \n line \n comment */", 4);
    check("/* multi \n line \n comment */16", 16);
}

TEST_CASE("boolean expression") {
    check("true", true);
    check("false", false);
    CHECK_AST("!true", not_(boolean(true)));
    CHECK_AST("!\ttrue", not_(boolean(true)));
    CHECK_AST("!(!true)", not_(not_(boolean(true))));
    CHECK_AST("true", boolean(true));
    CHECK_AST("true || true", or_(boolean(true), boolean(true)));
    CHECK_AST("true || true || true",
              or_(or_(boolean(true), boolean(true)), boolean(true)));
    CHECK_AST("true && true", and_(boolean(true), boolean(true)));
    CHECK_AST("true && true && true",
              and_(and_(boolean(true), boolean(true)), boolean(true)));
    CHECK_AST("true -> true", impl(boolean(true), boolean(true)));
    CHECK_AST("true -> true -> true",
              impl(impl(boolean(true), boolean(true)), boolean(true)));
    CHECK_AST("true -> true -> true && true || true -> true && true && true || "
              "true && true",
              impl(impl(impl(1_b, 1_b), or_(and_(1_b, 1_b), 1_b)),
                   or_(and_(and_(1_b, 1_b), 1_b), and_(1_b, 1_b))));
}

TEST_CASE("equal compare") {
    CHECK_AST("1 == 1", eq(1_n, 1_n));
    CHECK_AST("[] == 1", eq(array(), 1_n));
    CHECK_AST("true != false", neq(1_b, 0_b));
}

TEST_CASE("order compare") {
    CHECK_AST("2 > 1", gt(2_n, 1_n));
    CHECK_AST("1 >= 1", geq(1_n, 1_n));
    CHECK_AST("1 < 2", lt(1_n, 2_n));
    CHECK_AST("1 <= 1", leq(1_n, 1_n));
}

TEST_CASE("strings") {
    check("\"\""s);
    check("\"shortstring\""s);
    check("\"shortstring with \\\" escape\""s,
          "\"shortstring with \" escape\""s);
    CHECK_AST("\"xyz\"", string("xyz"));
    check("\"shortstring with dollarcurly ${true}\""s);
    check("\"shortstring with dollarcurlys ${true}${true}\""s);
    check("\"shortstring with dollarcurly ${\"with inner string\"}\""s);
    check(
        "\"shortstring with nested dollarcurly ${\"[outer,${\"<inner>\"}]\"}\""s);
    check("''''"s);
    check("''longstring''"s);
    check("''longstring with ''' escape''"s, "''longstring with ' escape''"s);
}

/*
TEST_CASE("bla") {
    nix::parser::state::base result;
    SECTION("true") {
        REQUIRE(parse("-({a=1;}).a", result));
        std::cout << "parsed: " << result.data << std::endl;
    }
}*/

TEST_CASE("number") {
    check("1337", 1337);
    CHECK_AST("-1337", negate(1337_n));
    check("--1337", 1337);
}

TEST_CASE("arithmetic sum") {
    CHECK_AST("-23 + 42", add(negate(23_n), 42_n));
    check("23 - 42", "(23-42)"s);
    CHECK_AST("23 - -42", sub(23_n, negate(42_n)));
    check("1 + 2 + 3", "((1+2)+3)"s);
    CHECK_AST("1 + 2 + 3", add(add(1_n, 2_n), 3_n));
    check("1 - 2 - 3", "((1-2)-3)"s);
}

TEST_CASE("arithmetic product") {
    check("23 * 42", "(23*42)"s);
    CHECK_AST("-23 * 42", mul(negate(23_n), 42_n));
    CHECK_AST("23 * -42", mul(23_n, negate(42_n)));
    CHECK_AST("1 * 2 * 3", mul(mul(1_n, 2_n), 3_n));
    CHECK_AST("1 * -2 * 3", mul(mul(1_n, negate(2_n)), 3_n));
    CHECK_AST("1 / -2", div(1_n, negate(2_n)));
}

TEST_CASE("arithmetic mixed") {
    check("1 * 2 + 1", "((1*2)+1)"s);
    check("1 + 2 * 1", "(1+(2*1))"s);
}

TEST_CASE("table") {
    check("{ }"s);
    CHECK_AST("{}", table(false));
    check("rec { }"s);
    CHECK_AST("rec {}", table(true));
    check("{ a = 1; }"s);
    check("rec { a = 1; }"s);
    check("{ a = 1; b = \"c\"; }"s);
    check("{ a = 1; b = \"c\"; c = foobar; }"s);
    check("{ inherit a; }"s);
    check("{ inherit a b; }"s);
    check("{ inherit (a) b; }"s);
    check("{ inherit (a) b c; }"s);
    check("{ inherit (a) b; inherit c; inherit (d) e; }"s);
    CHECK_AST("{ a.b = 1; }"s, table(false, bind(attrpath("a"_n, "b"_n), 1_n)));
}

TEST_CASE("table merge") {
    CHECK_AST("{ } // {}", merge(table({}), table({})));
}

TEST_CASE("table lookup") {
    CHECK_AST("{}.a", lookup(table(false), "a"_n));
    CHECK_AST("rec {}.a", lookup(table(true), "a"_n));
    CHECK_AST("{}.a or b", lookup(table(false), "a"_n, "b"_n));
    CHECK_AST("{}.a or {}.b or c",
              lookup(table(false), "a"_n, lookup(table(false), "b"_n, "c"_n)));
}

TEST_CASE("attrtest") {
    CHECK_AST("{} ? a", attrtest(table({}), "a"_n));
    CHECK_AST("{} ? a.b", attrtest(table({}), attrpath("a"_n, "b"_n)));
    nix::parser::state::base res;
    CHECK_THROWS(parse_nocatch("{} ? a ? b", res));
}

TEST_CASE("function") {
    CHECK_AST("a: 1", function("a"_n, 1_n));
    CHECK_AST("a: b: 1", function("a"_n, function("b"_n, 1_n)));
}

TEST_CASE("function call") {
    CHECK_AST("import \"abc.nix\"", call("import"_n, "abc.nix"_s));
    CHECK_AST("(a: a) 1", call(function("a"_n, "a"_n), 1_n));
    CHECK_AST(
        "(a: b: a + b) 1 2",
        call(call(function("a"_n, function("b"_n, add("a"_n, "b"_n))), 1_n),
             2_n));
}

TEST_CASE("parameter list") {
    check("{ }: 1"s);
    check("{ a }: 1"s);
    check("{ a, b }: 1"s);
    check("{ ... }: 1"s);
    check("{ a, ... }: 1"s);
    check("{ a, b, ... }: 1"s);
    check("{ a ? true, b ? ''${a}'', c ? { }, ... }: 1"s);
}

TEST_CASE("array") {
    check("[]"s, "[ ]"s);
    check("[ ]"s);
    check("[ 1 ]"s);
    check("[ 1 \"b\" ]"s);
    check("[ 1 \"b\" c ]"s);
    CHECK_AST("[ 1 \"b\" c ]", array(1_n, "b"_s, "c"_n));
}

TEST_CASE("array merge") { CHECK_AST("[] ++ []", concat(array(), array())); }

TEST_CASE("let in") {
    check("let in 1"s);
    check("let x = 1; in 1"s);
    check("let x = 1; y = 2; in 1"s);
    check("let inherit x; in 1"s);
    check("let inherit (x); in 1"s);
    check("let inherit (x) y; in 1"s);
    check("let inherit (x) y z; in 1"s);
    check("let inherit ({ a = 1; }) b c; in 1"s);
}

TEST_CASE("assert") {
    CHECK_AST("assert true; a", assertion(1_b, "a"_n));
    CHECK_AST("assert true; assert true && true; a",
              assertion(1_b, assertion(and_(1_b, 1_b), "a"_n)));
}

TEST_CASE("with") { CHECK_AST("with true; a", with(1_b, "a"_n)); }

TEST_CASE("if then else") {
    check("if true then \"yes\" else \"false\""s);
    CHECK_AST("if true then \"yes\" else \"false\"",
              if_then_else(boolean(true), "yes"_s, "false"_s));
}
/*
TEST_CASE("complex") {
    check("let requiredVersion = import ./lib/minver.nix; in\n"
          "if ! builtins ? nixVersion || builtins.compareVersions "
          "requiredVersion builtins.nixVersion == 1 then \n"
          "  abort \"This version of Nixpkgs requires Nix >= "
          "${requiredVersion}, please upgrade! See "
          "https://nixos.org/wiki/"
          "How_to_update_when_Nix_is_too_old_to_evaluate_Nixpkgs\"\n"
          "else\n"
          "  import ./pkgs/top-level/impure.nix"s);
}
*/
