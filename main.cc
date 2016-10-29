// Copyright (c) 2014-2016 Dr. Colin Hirsch and Daniel Frey
// Please see LICENSE for license or visit https://github.com/ColinH/PEGTL/

#include <string>
#include <iostream>

#include "expr.hh"

int main( int argc, char ** argv )
{
    bool ok = pegtl::parse_string<nix::parser::grammar>("2342", "2342");
    assert(ok);

    for (int i = 1; i < argc; i++) {
        if (getenv("DEBUG"))
            ok = 0; //pegtl::parse_file<nix::parser::grammar, pegtl::nothing, pegtl::tracer>(argv[i]);
        else
            ok = pegtl::parse_file<nix::parser::grammar, pegtl::nothing, pegtl::normal>(argv[i]);
        assert(ok);
        std::cout << "parsed :" << argv[i] << std::endl;
    }

}
