/*
 * expr.cc
 *
 *  Created on: Nov 7, 2016
 *      Author: groxxda
 */

#include "expr.hh"
#include <pegtl.hh>

namespace nix {
namespace parser {


bool parse_file(const std::string& filename, nix::parser::state::base& res) {
    return pegtl::parse_file<nix::parser::grammar, pegtl::nothing,
                             nix::parser::control::normal>(filename, res);
}

bool parse_string(const std::string& data, nix::parser::state::base& res) {
    return pegtl::parse_string<nix::parser::grammar, pegtl::nothing,
                               nix::parser::control::normal>(data, data, res);
}

} // namespace parser
} // namespace nix
