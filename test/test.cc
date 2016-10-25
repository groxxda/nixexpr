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

template <typename... Args>
bool parse(Args&&... args) {
    return pegtl::parse_string<grammar, pegtl::nothing, control>(std::forward<Args>(args)...);
}

template <typename A>
A compare(nix::parser::result_state& result) {
    std::stringstream s;
    s << result.result;
    std::cout << result.result << std::endl;
    A x;
    s >> x;
    return x;
}
/*template<>
    bool compare<nix::parser::result_state, std::string, Catch::Internal::Operator::IsEqualTo>(const nix::parser::result_state& result, const std::string&& expected) {
return true;
    }*/


std::string to_string(result_state const& result) {
    std::stringstream s;
    s << result.result;
    return s.str();
}


TEST_CASE("grammar analysis") {
    const size_t issues_found = pegtl::analyze<grammar>();
    REQUIRE(issues_found == 0);
}

TEST_CASE("numbers") {
    result_state result;
    SECTION("constant") {
        REQUIRE(parse("1", "(string)", result));
        REQUIRE(compare<int>(result) == 1);
    }

}

TEST_CASE("if then else") {
    nix::parser::result_state result;
    SECTION ("true") {
        bool ok = pegtl::parse_string<nix::parser::grammar, pegtl::nothing, nix::parser::control>("if true then 1 else 2", "(string)", result);
        REQUIRE(ok);
        std::stringstream s;
        s << result.result;
        REQUIRE(s.str() == "if true then 1 else 2");
    }
}

TEST_CASE("string") {
    nix::parser::result_state result;
    SECTION("constant") {
        bool ok = pegtl::parse_string<nix::parser::grammar, pegtl::nothing, nix::parser::control>("\"test\"", "(string)", result);
        REQUIRE(ok);
        std::stringstream s;
        s << result.result;
        REQUIRE(s.str() == "\"test\"");
    }

    SECTION("concat") {
        bool ok = pegtl::parse_string<nix::parser::grammar, pegtl::nothing, nix::parser::control>("\"te\"+\"st\"", "(string)", result);
        REQUIRE(ok);
        std::stringstream s;
        s << result.result;
        REQUIRE(s.str() == "\"test\"");
    }
}

TEST_CASE("constants") {
    nix::parser::result_state result;

    SECTION("integer") {
        bool ok = pegtl::parse_string<nix::parser::grammar, pegtl::nothing, nix::parser::control>("2342", "(string)", result);
        REQUIRE(ok);
        std::stringstream s;
        s << result.result;
        REQUIRE(s.str() == "2342");
    }

    SECTION("list") {
        bool ok = pegtl::parse_string<nix::parser::grammar, pegtl::nothing, nix::parser::control>("[   2342 2343 \"abc\"  4223\t \"zyx\"\n\n 4222]", "(string)", result);
        REQUIRE(ok);
        std::stringstream s;
        s << result.result;
        REQUIRE(s.str() == "[ 2342 2343 \"abc\" 4223 \"zyx\" 4222 ]");
    }

    SECTION("object") {
        bool ok = pegtl::parse_string<nix::parser::grammar, pegtl::nothing, nix::parser::control>("{a=1;}", "(string)", result);
        REQUIRE(ok);
        std::stringstream s;
        s << result.result;
        REQUIRE(s.str() == "{ a = 1; }");
    }

    SECTION("object nested attribute path") {
        bool ok = pegtl::parse_string<nix::parser::grammar, pegtl::nothing, nix::parser::control>("{a.b=1;}", "(string)", result);
        REQUIRE(ok);
        std::stringstream s;
        s << result.result;
        REQUIRE(s.str() == "{ a.b = 1; }");
    }
    /*SECTION("string") {
        auto res = driver.parse (std::istringstream("    \"xx\""));
        REQUIRE(res == 0);
    }

    SECTION("list of ints") {
        auto res = driver.parse (std::istringstream("[ 1337 ]"));
        REQUIRE(res == 0);
    }*/

}
