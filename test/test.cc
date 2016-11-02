/*
 * test.cc
 *
 *  Created on: Oct 24, 2016
 *      Author: groxxda
 */
#include "catch.hpp"

#include <iostream>
#include <pegtl/analyze.hh>
#include "expr.hh"

using namespace std::literals;

template <typename Str, typename... Args>
bool parse(Str&& str, Args&&... args) {
    try {
//        nix::parser::state::base result;
        //auto res = pegtl::parse_string<grammar, nix::parser::action/*nix::parser::grammar_action*//*pegtl::nothing*/, /*pegtl::normal*/nix::parser::control::normal>(std::forward<Str>(str), std::forward<Str>(str), std::forward<Args>(args)...);
        auto res = pegtl::parse_string<nix::parser::grammar, nix::parser::action, /*pegtl::normal*/nix::parser::control::normal>(std::forward<Str>(str), std::forward<Str>(str), std::forward<Args>(args)...);
//        std::cout << "result: " << result.result << std::endl;
        return res;
    } catch (pegtl::parse_error& e) {
        std::cerr << "Error parsing " << str << ":" << std::endl;
        std::cerr << e.what() << std::endl;
        return false;
    } catch (...) {
        std::cerr << "unknown error parsing " << str << ":" << std::endl;
        return false;
    }
}

template<typename R>
R compare(nix::parser::state::expression& state) {
    std::stringstream s;
    s << state.value;
    R r;
    s >> r;
    return r;
}

template<>
std::string compare(nix::parser::state::expression& state) {
    std::stringstream s;
    s << state.value;
    return s.str();
}

template<typename R, typename A>
R compare_downcast(nix::parser::state::expression& state) {

    auto downcast = std::dynamic_pointer_cast<A>(state.value);
    if (!downcast) {
        auto val = state.value;
        std::stringstream msg("expected ");
        msg << typeid(A).name();
        msg << ", but got type=";
        if (val) {
            auto& val_ = *val;
            msg << typeid(val_).name();
        } else
            msg << "nullptr";
        msg << " with value=";
        msg << val;
        throw msg.str();
    }
    return downcast->data;
}

template<> int compare(nix::parser::state::expression& state) { return compare_downcast<int, nix::ast::number>(state); }

template<> bool compare(nix::parser::state::expression& state) { return compare_downcast<bool, nix::ast::boolean>(state); }

template<typename S, typename R>
void check(S&& str, const R& expect) {
    SECTION(str) {
        nix::parser::state::expression result;
        REQUIRE(parse(str, result));
        REQUIRE(compare<R>(result) == expect);
    }
}

template<typename S>
void check(S&& str) {
    check<>(str, str);
}

#define CHECK_AST(expr, ast) SECTION(expr) { nix::parser::state::expression result; REQUIRE(parse(expr, result)); REQUIRE(ast == result.value); }




std::shared_ptr<nix::ast::base> boolean(bool v) { return std::make_shared<nix::ast::boolean>(v); }
std::shared_ptr<nix::ast::base> string(std::string v) { return std::make_shared<nix::ast::string>(v); }
std::shared_ptr<nix::ast::base> number(long long v) { return std::make_shared<nix::ast::number>(v); }
std::shared_ptr<nix::ast::base> not_(std::shared_ptr<nix::ast::base> v) { return std::make_shared<nix::ast::not_>(v); }
std::shared_ptr<nix::ast::base> negate(std::shared_ptr<nix::ast::base> v) { return std::make_shared<nix::ast::negate>(v); }
std::shared_ptr<nix::ast::base> add(std::shared_ptr<nix::ast::base> lhs, std::shared_ptr<nix::ast::base> rhs) { return std::make_shared<nix::ast::add>(lhs, rhs); }
std::shared_ptr<nix::ast::base> sub(std::shared_ptr<nix::ast::base> lhs, std::shared_ptr<nix::ast::base> rhs) { return std::make_shared<nix::ast::sub>(lhs, rhs); }
std::shared_ptr<nix::ast::base> mul(std::shared_ptr<nix::ast::base> lhs, std::shared_ptr<nix::ast::base> rhs) { return std::make_shared<nix::ast::mul>(lhs, rhs); }
std::shared_ptr<nix::ast::base> div(std::shared_ptr<nix::ast::base> lhs, std::shared_ptr<nix::ast::base> rhs) { return std::make_shared<nix::ast::div>(lhs, rhs); }
std::shared_ptr<nix::ast::base> or_(std::shared_ptr<nix::ast::base> lhs, std::shared_ptr<nix::ast::base> rhs) { return std::make_shared<nix::ast::or_>(lhs, rhs); }



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

}

TEST_CASE("strings") {
    check("\"\""s);
    check("\"shortstring\""s);
    check("\"shortstring with \\\" escape\""s);
    CHECK_AST("\"xyz\"", string("xyz"));
//    check("\"shortstring with dollarcurly ${true}\""s);
//    check("\"shortstring with dollarcurlys ${true}${true}\""s);
//    check("\"shortstring with dollarcurly ${\"with inner string\"}\""s);
//    check("\"shortstring with nested dollarcurly ${\"[outer,${\"<inner>\"}]\"}\""s);
    check("''''"s, "\"\""s);
    check("''longstring''"s, "\"longstring\""s);
//    check("''longstring with ''' escape''"s, "\"longstring with ' escape\""s);
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
    CHECK_AST("-1337", negate(number(1337)));
    check("--1337", 1337);
}

TEST_CASE("arithmetic sum") {
    CHECK_AST("-23 + 42", add(negate(number(23)), number(42)));
    check("23 - 42", "(23-42)"s);
    CHECK_AST("23 - -42", sub(number(23), negate(number(42))));
    check("1 + 2 + 3", "((1+2)+3)"s);
    CHECK_AST("1 + 2 + 3", add(add(number(1), number(2)),number(3)));
    check("1 - 2 - 3", "((1-2)-3)"s);
}

TEST_CASE("arithmetic product") {
    check("23 * 42", "(23*42)"s);
    CHECK_AST("-23 * 42", mul(negate(number(23)), number(42)));
    CHECK_AST("23 * -42", mul(number(23), negate(number(42))));
    CHECK_AST("1 * 2 * 3", mul(mul(number(1), number(2)), number(3)));
    CHECK_AST("1 * -2 * 3", mul(mul(number(1), negate(number(2))), number(3)));
    CHECK_AST("1 / -2",  div(number(1), negate(number(2))));
}

TEST_CASE("arithmetic mixed") {
    check("1 * 2 + 1", "((1*2)+1)"s);
    check("1 + 2 * 1", "(1+(2*1))"s);
}


TEST_CASE("table") {
    check("{ }"s);
    check("rec { }"s);
    check("{ a = 1; }"s);
    check("rec { a = 1; }"s);
    check("{ a = 1; b = \"c\"; }"s);
    check("{ a = 1; b = \"c\"; c = foobar; }"s);
    check("{ inherit a; }"s, "{ a = a; }"s);
    check("{ inherit a b; }"s, "{ a = a; b = b; }"s);
    check("{ inherit (a) b; }"s, "{ b = a.b; }"s);
    check("{ inherit (a) b c; }"s, "{ b = a.b; c = a.c; }"s);
    check("{ inherit (a) b; inherit c; inherit (d) e; }"s, "{ b = a.b; c = c; e = d.e; }"s);
}

TEST_CASE("table merge") {
    //check("{ } // {}"s);
}


TEST_CASE("array") {
    check("[]"s, "[ ]"s);
    check("[ ]"s);
    check("[ 1 ]"s);
    check("[ 1 \"b\" ]"s);
    check("[ 1 \"b\" c ]"s);
}

TEST_CASE("array merge") {
    //check("[] ++ []"));
}

TEST_CASE("let in") {
    check("let in 1"s);
    check("let x = 1; in 1"s);
    check("let x = 1; y = 2; in 1"s);
    check("let inherit x; in 1"s, "let x = x; in 1"s);
    check("let inherit(x) y; in 1"s, "let y = x.y; in 1"s);
    check("let inherit (x) y z; in 1"s, "let y = x.y; z = x.z; in 1"s);
    check("let inherit ({a=1;}) a b; in 1"s, "let a = { a = 1; }.a; b = { a = 1; }.b; in 1"s);
}

//TEST_CASE("parameter") {
//    CHECK(parse("a: 1"));
//    CHECK(parse("a: b: 1"));
//}
//
//TEST_CASE("parameter list") {
//    CHECK(parse("{ }: 1"));
//    CHECK(parse("{ a }: 1"));
//    CHECK(parse("{ a, b }: 1"));
//    CHECK(parse("{ ... }: 1"));
//    CHECK(parse("{ a, ... }: 1"));
//    CHECK(parse("{ a, b, ... }: 1"));
//}
//
//TEST_CASE("assert") {
//    CHECK(parse("assert true; 1"));
//    CHECK(parse("assert 1; 1"));
//    CHECK(parse("assert true;assert false ; 1"));
//}
//
//TEST_CASE("with") {
//    CHECK(parse("with x; 1"));
//}

//
//TEST_CASE("if then else") {
//    CHECK(parse("if true then \"yes\" else \"false\""));
//    CHECK(parse("if (1) then (\"yes\") else (\"false\")"));
//}
//
//
//TEST_CASE("complex") {
//
//    CHECK(parse("let requiredVersion = import ./lib/minver.nix; in\n"
//            "if ! builtins ? nixVersion || builtins.compareVersions requiredVersion builtins.nixVersion == 1 then \n"
//            "  abort \"This version of Nixpkgs requires Nix >= ${requiredVersion}, please upgrade! See https://nixos.org/wiki/How_to_update_when_Nix_is_too_old_to_evaluate_Nixpkgs\"\n"
//            "else\n"
//            "  import ./pkgs/top-level/impure.nix"));
//}



