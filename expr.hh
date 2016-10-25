#pragma once

#include <pegtl.hh>
#include <pegtl/trace.hh>
#include <pegtl/contrib/changes.hh>
#include <pegtl/contrib/unescape.hh>

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <iostream>


namespace nix {

namespace ast {
    struct base {
        virtual void stream(std::ostream&) const = 0;
    protected:
        explicit base() {}
        ~base() {}
    };

    inline std::ostream& operator<<(std::ostream& o, const base& b) {
        b.stream(o);
        return o;
    }

    inline std::ostream& operator<<(std::ostream& o, const std::shared_ptr<base>& b) {
        return b ? (o << *b) : (o << "NULL");
    }


} // namespace ast

namespace parser {

    struct grammar : pegtl::must<pegtl::eof> {};


} // namespace parser

} // namespace nix
