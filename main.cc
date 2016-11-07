// Copyright (c) 2014-2016 Dr. Colin Hirsch and Daniel Frey
// Please see LICENSE for license or visit https://github.com/ColinH/PEGTL/
#define NDEBUG
#include <iostream>
#include <string>

#include "expr.hh"

int main(int argc, char** argv) {
    bool ok = pegtl::parse_string<nix::parser::grammar>("2342", "2342");
    assert(ok);

    for(int i = 1; i < argc; i++) {
        nix::parser::state::base result;
        if(getenv("DEBUG"))
            ok = 0;
        // ok = pegtl::parse_file<nix::parser::grammar, pegtl::nothing,
        // nix::parser::control::tracer>(argv[i]);
        else
            // ok = 0;
            ok = pegtl::parse_file<nix::parser::grammar, pegtl::nothing,
                                   nix::parser::control::normal>(argv[i],
                                                                 result);
        assert(ok);
        std::cout << "parsed :" << argv[i] << std::endl;
    }
}
