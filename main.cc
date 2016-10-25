// Copyright (c) 2014-2016 Dr. Colin Hirsch and Daniel Frey
// Please see LICENSE for license or visit https://github.com/ColinH/PEGTL/

#include <string>
#include <iostream>

#include "expr.hh"

int main( int argc, char ** argv )
{
    bool ok = pegtl::parse_string<nix::parser::grammar>("2342", "(string)");
    assert(ok);

    ok = pegtl::parse_string<nix::parser::grammar>("\"Hello, hurz!\"", "(string)");
    assert(ok);

}
