// Copyright (c) 2014-2016 Dr. Colin Hirsch and Daniel Frey
// Please see LICENSE for license or visit https://github.com/ColinH/PEGTL/
#include <iostream>
#include <string>

#include "expr.hh"

int main(int argc, char** argv) {
    nix::parser::state::base res;
    bool ok = nix::parser::parse_string("2342", res);
    assert(ok);

    for(int i = 1; i < argc; i++) {
        std::cout << "parsing " << argv[i] << ": ";
        nix::parser::state::base result;
        if(getenv("DEBUG")) ok = 0;
        // ok = pegtl::parse_file<nix::parser::grammar, pegtl::nothing,
        // nix::parser::control::tracer>(argv[i]);
        else
            // ok = 0;
            ok = nix::parser::parse_file(argv[i], result);
        assert(ok);
        std::cout << (ok ? "ok" : "fail") << std::endl;
    }
}
