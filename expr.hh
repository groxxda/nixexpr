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
namespace keyword {
    struct str_if      : pegtl::string<'i', 'f'>                          {};
    struct str_then    : pegtl::string<'t', 'h', 'e', 'n'>                {};
    struct str_else    : pegtl::string<'e', 'l', 's', 'e'>                {};
    struct str_assert  : pegtl::string<'a', 's', 's', 'e', 'r', 't'>      {};
    struct str_with    : pegtl::string<'w', 'i', 't', 'h'>                {};
    struct str_let     : pegtl::string<'l', 'e', 't'>                     {};
    struct str_in      : pegtl::string<'i', 'n'>                          {};
    struct str_rec     : pegtl::string<'r', 'e', 'c'>                     {};
    struct str_inherit : pegtl::string<'i', 'n', 'h', 'e', 'r', 'i', 't'> {};
    struct str_or      : pegtl::string<'o', 'r'>                          {};

    struct str_any     : pegtl::sor<str_if, str_then, str_else, str_assert, str_with, str_let, str_in, str_rec, str_inherit, str_or> {};

    template<typename Key>
    struct key         : pegtl::seq<Key, pegtl::not_at<pegtl::identifier_other>> {};

    struct key_then    : key<str_then>    {};
    struct key_else    : key<str_else>    {};
    struct key_assert  : key<str_assert>  {};
    struct key_with    : key<str_with>    {};
    struct key_let     : key<str_let>     {};
    struct key_in      : key<str_in>      {};
    struct key_rec     : key<str_rec>     {};
    struct key_inherit : key<str_inherit> {};
    struct key_or      : key<str_or>      {};
    struct key_ellipsis: pegtl::string<'.', '.', '.'> {};

    struct any         : key<str_any>     {};

} // namespace keyword

    struct line_comment : pegtl::disable<pegtl::one<'#'>, pegtl::until<pegtl::eolf>> {};
    struct long_comment : pegtl::disable<pegtl::string<'/', '*'>, pegtl::until<pegtl::string<'*', '/'>>> {};
    struct comment : pegtl::sor<line_comment, long_comment> {};

    struct sep : pegtl::sor<pegtl::ascii::space, comment> {};
    struct seps : pegtl::star<sep> {};

    template<typename R>
    struct pad : pegtl::pad<R, sep> {};

    // XXX: actually nix allows ' and - after the first char aswell
    struct name : pegtl::seq<pegtl::not_at<keyword::any>, pegtl::identifier> {};

    // XXX: add string interpolation
    template<typename Q>
    struct any_string : pegtl::if_must<Q, pegtl::until<Q>> {};
    struct short_string : any_string<pegtl::one<'"'>> {};
    // XXX: add prefix stripping
    struct long_string : any_string<pegtl::string<'\'', '\''>> {};
    struct string : pegtl::sor<short_string, long_string> {};

    struct number : pegtl::plus<pegtl::digit> {};


    struct expression;

    // XXX: allow name ? default
    struct argument_field_list_nonempty : pegtl::seq<pegtl::list_tail<name, pegtl::one<','>, sep>, seps, pegtl::opt<keyword::key_ellipsis>> {};
    struct argument_field_list : pegtl::sor<keyword::key_ellipsis, argument_field_list_nonempty> {};
    struct argument_field_constructor : pegtl::seq<pegtl::one<'{'>, pegtl::pad_opt<argument_field_list, sep>, pegtl::one<'}'>> {};
    struct argument_constructor : pegtl::seq<pegtl::sor<name, argument_field_constructor>, seps, pegtl::if_must<pegtl::one<':'>, pegtl::plus<sep>>> {};

    struct table_field_assign : pegtl::if_must<pegtl::seq<name, seps, pegtl::one<'='>>, seps, expression> {};
    // XXX: allow specifying source object with (...)
    struct table_field_inherit : pegtl::if_must<keyword::key_inherit, seps, pegtl::list<name, seps>> {};
    struct table_field : pegtl::sor<table_field_assign, table_field_inherit> {};
    struct table_field_list : pegtl::plus<table_field, pad<pegtl::one<';'>>> {};
    struct table_constructor : pegtl::if_must<pegtl::one<'{'>, pegtl::pad_opt<table_field_list, sep>, pegtl::one<'}'>> {};

    struct array_field_list : pegtl::list<expression, seps> {};
    struct array_constructor : pegtl::if_must<pegtl::one<'['>, pegtl::pad_opt<array_field_list, sep>, pegtl::one<']'>> {};


    struct assert : pegtl::if_must<keyword::key_assert, sep, expression, pad<pegtl::one<';'>>> {};
    struct statement : pegtl::sor<assert> {};
    struct statement_list : pegtl::star<statement> {};




    //FIXME: only to test
    struct expression : pegtl::sor<string, number, pegtl::plus<argument_constructor>, table_constructor, array_constructor, name> {};

    // XXX: remove 'seps': empty expressions are invalid
    struct grammar : pegtl::must<seps, statement_list, pegtl::sor<expression, seps>, pegtl::eof> {};


} // namespace parser

} // namespace nix






