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

using namespace nix::parser;

template <typename Str, typename... Args>
bool parse(Str&& str, Args&&... args) {
    try {
        return pegtl::parse_string<grammar, pegtl::nothing, pegtl::tracer>(std::forward<Str>(str), std::forward<Str>(str), std::forward<Args>(args)...);
    } catch (pegtl::parse_error& e) {
        std::cerr << "Error parsing " << str << ":" << std::endl;
        std::cerr << e.what() << std::endl;
        return false;
    }
}


TEST_CASE("grammar analysis") {
    const size_t issues_found = pegtl::analyze<grammar>();
    CHECK(issues_found == 0);
}

TEST_CASE("comments") {
    CHECK(parse("1# single line string"));
    CHECK(parse("1/* multi \n line \n string */"));
    CHECK(parse("/* multi \n line \n string */1"));
}

TEST_CASE("strings") {
    CHECK(parse("\"\""));
    CHECK(parse("\"shortstring\""));
    CHECK(parse("\"shortstring with \\\" escape\""));
    CHECK(parse("\"shortstring with dollarcurly ${true}\""));
    CHECK(parse("\"shortstring with dollarcurlys ${true}${true}\""));
    CHECK(parse("\"shortstring with dollarcurly ${\"with inner string\"}\""));
    CHECK(parse("\"shortstring with nested dollarcurly ${\"[outer,${\"<inner>\"}]\"}\""));
    CHECK(parse("''''"));
    CHECK(parse("''longstring''"));
    CHECK(parse("''longstring with ''' escape''"));
}

TEST_CASE("number") {
    CHECK(parse("1337"));
    CHECK(parse("23 + 42"));
}

TEST_CASE("table") {
    CHECK(parse("{ }"));
    CHECK(parse("{ } // {}"));
    CHECK(parse("{ a = 1; }"));
    CHECK(parse("{ a = 1; b = \"c\"; }"));
    CHECK(parse("{ a = 1; b = \"c\"; c = foobar; }"));
    CHECK(parse("{ inherit a; }"));
    CHECK(parse("{ inherit a b; }"));
}

TEST_CASE("array") {
    CHECK(parse("[]"));
    CHECK(parse("[ ]"));
    CHECK(parse("[] ++ []"));
    CHECK(parse("[ 1 ]"));
    CHECK(parse("[ 1 \"b\" ]"));
    CHECK(parse("[ 1 \"b\" c ]"));
}

TEST_CASE("parameter") {
    CHECK(parse("a: 1"));
    CHECK(parse("a: b: 1"));
}

TEST_CASE("parameter list") {
    CHECK(parse("{ }: 1"));
    CHECK(parse("{ a }: 1"));
    CHECK(parse("{ a, b }: 1"));
    CHECK(parse("{ ... }: 1"));
    CHECK(parse("{ a, ... }: 1"));
    CHECK(parse("{ a, b, ... }: 1"));
}

TEST_CASE("assert") {
    CHECK(parse("assert true; 1"));
    CHECK(parse("assert 1; 1"));
    CHECK(parse("assert true;assert false ; 1"));
}

TEST_CASE("with") {
    CHECK(parse("with x; 1"));
}

TEST_CASE("let in") {
    CHECK(parse("let x=1; in 1"));
    CHECK(parse("let x = 1; in 1"));
    CHECK(parse("let x = 1; y=2; in 1"));
    CHECK(parse("let inherit x; in 1"));
    CHECK(parse("let inherit(x) y; in 1"));
    CHECK(parse("let inherit (x) y z; in 1"));
}

TEST_CASE("if then else") {
    CHECK(parse("if true then \"yes\" else \"false\""));
    CHECK(parse("if (1) then (\"yes\") else (\"false\")"));
}

TEST_CASE("boolean expression") {
    CHECK(parse("true"));
    CHECK(parse("false"));
    CHECK(parse("!true"));
    CHECK(parse("!\ttrue"));
    CHECK(parse("!(true)"));
    CHECK(parse("!(\"a\")"));
}

TEST_CASE("complex") {

    CHECK(parse("let requiredVersion = import ./lib/minver.nix; in\n"
            "if ! builtins ? nixVersion || builtins.compareVersions requiredVersion builtins.nixVersion == 1 then \n"
            "  abort \"This version of Nixpkgs requires Nix >= ${requiredVersion}, please upgrade! See https://nixos.org/wiki/How_to_update_when_Nix_is_too_old_to_evaluate_Nixpkgs\"\n"
            "else\n"
            "  import ./pkgs/top-level/impure.nix"));
}



