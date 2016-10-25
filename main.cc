// Copyright (c) 2014-2016 Dr. Colin Hirsch and Daniel Frey
// Please see LICENSE for license or visit https://github.com/ColinH/PEGTL/

#include <string>
#include <iostream>

#include "expr.hh"

int main( int argc, char ** argv )
{
    nix::parser::result_state result;
    bool ok = pegtl::parse_string<nix::parser::grammar, pegtl::nothing, nix::parser::control>("2342", "(string)", result);
    assert(ok);
    std::cout << "ok=" << ok << " result=" << result.result << std::endl;

    result.result.reset();
    ok = pegtl::parse_string<nix::parser::grammar, pegtl::nothing, nix::parser::control>("\"Hello, hurz!\"", "(string)", result);
    assert(ok);
    std::cout << "ok=" << ok << " result=" << result.result << std::endl;

}
