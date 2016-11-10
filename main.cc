#include <iostream>
#include <string>

#include "expr.hh"

int main(int argc, char** argv) {
    nix::parser::state::base res;
    bool ok = nix::parser::parse_string("2342", res);
    assert(ok);

    bool debug = getenv("DEBUG");

    for(int i = 1; i < argc; i++) {
        std::cout << "parsing " << argv[i] << ": ";
        nix::parser::state::base result;
        try {
            ok = nix::parser::parse_file(argv[i], result);
        } catch(...) {}
        if(!debug) { assert(ok); }
        std::cout << (ok ? "ok" : "fail") << std::endl;
    }
}
