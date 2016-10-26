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
    return pegtl::parse_string<grammar, pegtl::nothing, pegtl::tracer>(std::forward<Str>(str), std::forward<Str>(str), std::forward<Args>(args)...);
}


TEST_CASE("grammar analysis") {
    const size_t issues_found = pegtl::analyze<grammar>();
    REQUIRE(issues_found == 0);
}

TEST_CASE("comments") {
    REQUIRE(parse("# single line string"));
    REQUIRE(parse("/* multi \n line \n string */"));
}

TEST_CASE("strings") {
    REQUIRE(parse("\"shortstring\""));
    REQUIRE(parse("''longstring''"));
}

TEST_CASE("number") {
    REQUIRE(parse("1337"));
}

TEST_CASE("table") {
    REQUIRE(parse("{ }"));
    REQUIRE(parse("{ a = 1; }"));
    REQUIRE(parse("{ a = 1; b = \"c\"; }"));
    REQUIRE(parse("{ a = 1; b = \"c\"; c = foobar; }"));
    REQUIRE(parse("{ inherit a; }"));
    REQUIRE(parse("{ inherit a b; }"));
}

TEST_CASE("array") {
    REQUIRE(parse("[]"));
    REQUIRE(parse("[ ]"));
    REQUIRE(parse("[ 1 ]"));
    REQUIRE(parse("[ 1 \"b\" ]"));
    REQUIRE(parse("[ 1 \"b\" c ]"));
}

TEST_CASE("parameter") {
    REQUIRE(parse("a: "));
    REQUIRE(parse("a: b: "));
}

TEST_CASE("parameter list") {
    REQUIRE(parse("{ }: "));
    REQUIRE(parse("{ a }: "));
    REQUIRE(parse("{ a, }: "));
    REQUIRE(parse("{ a, b }: "));
    REQUIRE(parse("{ a, b, }: "));
    REQUIRE(parse("{ ... }: "));
    REQUIRE(parse("{ a, ... }: "));
    REQUIRE(parse("{ a, b, ... }: "));
}
