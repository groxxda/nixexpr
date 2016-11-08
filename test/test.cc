/*
 * test.cc
 *
 *  Created on: Oct 24, 2016
 *      Author: groxxda
 */
#include "catch.hpp"

#include <pegtl/analyze.hh>
#include <iostream>
#include "expr.hh"

using namespace std::literals;

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

template <typename R, typename A>
R compare_downcast(nix::parser::state::base& state) {
    auto downcast = dynamic_cast<A*>(state.value.get());
    if(!downcast) {
        std::stringstream msg("expected ");
        msg << typeid(A).name();
        msg << ", but got type=";
        if(state.value) {
            auto& val_ = *state.value;
            msg << typeid(val_).name();
        } else
            msg << "nullptr";
        msg << " with value=";
        msg << state.value;
        throw msg.str();
    }
    return downcast->data;
}

template <> int compare(nix::parser::state::base& state) {
    return compare_downcast<int, nix::ast::number>(state);
}

template <> bool compare(nix::parser::state::base& state) {
    return compare_downcast<bool, nix::ast::boolean>(state);
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

std::unique_ptr<nix::ast::base> boolean(bool v) {
    return std::make_unique<nix::ast::boolean>(v);
}
std::unique_ptr<nix::ast::base> operator""_b(unsigned long long v) {
    return std::make_unique<nix::ast::boolean>(v);
}
std::unique_ptr<nix::ast::base> string(std::string&& v) {
    std::vector<std::unique_ptr<nix::ast::base>> vals;
    vals.push_back(std::make_unique<nix::ast::string_literal>(std::move(v)));

    return std::make_unique<nix::ast::short_string>(std::move(vals));
}
std::unique_ptr<nix::ast::base> long_string(std::string&& v,
                                            unsigned long len) {
    std::vector<std::unique_ptr<nix::ast::base>> vals;
    vals.push_back(std::make_unique<nix::ast::string_literal>(std::move(v)));

    return std::make_unique<nix::ast::long_string>(std::move(vals),

                                                   len);
}
std::unique_ptr<nix::ast::base> operator"" _s(const char* v, size_t len) {
    return string(std::string(v, len));
}
std::unique_ptr<nix::ast::base> number(unsigned long long v) {
    return std::make_unique<nix::ast::number>(v);
}
std::unique_ptr<nix::ast::base> operator"" _n(unsigned long long v) {
    return number(v);
}
std::unique_ptr<nix::ast::base> name(std::string&& v) {
    return std::make_unique<nix::ast::name>(std::move(v));
}
std::unique_ptr<nix::ast::base> operator"" _n(const char* v, size_t len) {
    return name(std::string(v, len));
}
std::unique_ptr<nix::ast::base> not_(std::unique_ptr<nix::ast::base>&& v) {
    return std::make_unique<nix::ast::not_>(std::move(v));
}
std::unique_ptr<nix::ast::base> negate(std::unique_ptr<nix::ast::base>&& v) {
    return std::make_unique<nix::ast::negate>(std::move(v));
}
std::unique_ptr<nix::ast::base> eq(std::unique_ptr<nix::ast::base>&& lhs,
                                   std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::eq>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> neq(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::neq>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> add(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::add>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> sub(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::sub>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> mul(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::mul>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> div(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::div>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> concat(std::unique_ptr<nix::ast::base>&& lhs,
                                       std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::concat>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> merge(std::unique_ptr<nix::ast::base>&& lhs,
                                      std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::merge>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base>
attrtest(std::unique_ptr<nix::ast::base>&& lhs,
         std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::attrtest>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base>
attrpath(std::unique_ptr<nix::ast::base>&& lhs,
         std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::attrpath>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> or_(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::or_>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> and_(std::unique_ptr<nix::ast::base>&& lhs,
                                     std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::and_>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> impl(std::unique_ptr<nix::ast::base>&& lhs,
                                     std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::impl>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base>
assertion(std::unique_ptr<nix::ast::base>&& what,
          std::unique_ptr<nix::ast::base>&& expr) {
    return std::make_unique<nix::ast::assertion>(std::move(what),
                                                 std::move(expr));
}
std::unique_ptr<nix::ast::base> with(std::unique_ptr<nix::ast::base>&& what,
                                     std::unique_ptr<nix::ast::base>&& expr) {
    return std::make_unique<nix::ast::with>(std::move(what), std::move(expr));
}
std::unique_ptr<nix::ast::base>
function(std::unique_ptr<nix::ast::base>&& arg,
         std::unique_ptr<nix::ast::base>&& expr) {
    return std::make_unique<nix::ast::function>(std::move(arg),
                                                std::move(expr));
}
std::unique_ptr<nix::ast::base> bind(std::unique_ptr<nix::ast::base>&& name,
                                     std::unique_ptr<nix::ast::base>&& value) {
    return std::make_unique<nix::ast::binding_eq>(std::move(name),
                                                  std::move(value));
}
template <typename... T> std::unique_ptr<nix::ast::base> array(T&&... values) {
    std::unique_ptr<nix::ast::base> vals[] = {std::move(values)...};
    std::vector<std::unique_ptr<nix::ast::base>> vec;
    for(auto&& i : vals) vec.push_back(std::move(i));
    return std::make_unique<nix::ast::array>(std::move(vec));
}
template <typename... T>
std::unique_ptr<nix::ast::base> table(bool recursive, T&&... binds) {
    std::unique_ptr<nix::ast::base> vals[] = {std::move(binds)...};
    std::vector<std::unique_ptr<nix::ast::base>> vec;
    for(auto&& i : vals) vec.push_back(std::move(i));
    return std::make_unique<nix::ast::table>(
        std::make_unique<nix::ast::binds>(std::move(vec)), recursive);
}
std::unique_ptr<nix::ast::base>
if_then_else(std::unique_ptr<nix::ast::base>&& test,
             std::unique_ptr<nix::ast::base>&& then_expr,
             std::unique_ptr<nix::ast::base>&& else_expr) {
    return std::make_unique<nix::ast::if_then_else>(
        std::move(test), std::move(then_expr), std::move(else_expr));
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

TEST_CASE("comparison") {
    CHECK_AST("1 == 1", eq(1_n, 1_n));
    CHECK_AST("[] == 1", eq(array(), 1_n));
    CHECK_AST("true != false", neq(1_b, 0_b));
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
