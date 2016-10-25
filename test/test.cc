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
    return pegtl::parse_string<grammar>(std::forward<Args>(args)...);
}


TEST_CASE("grammar analysis") {
    const size_t issues_found = pegtl::analyze<grammar>();
    REQUIRE(issues_found == 0);
}

