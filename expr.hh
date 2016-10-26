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

    struct key_if      : key<str_if>      {};
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
    template<typename R>
    struct padr : pegtl::seq<R, pegtl::star<sep>> {};

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

    // XXX: add string_attr
    struct attr : pegtl::sor<name> {};
    struct attrpath : pegtl::list_must<attr, pegtl::one<'.'>, sep> {};
    // XXX: can be empty in original
    struct attrs : pegtl::seq<attr, pegtl::star<pegtl::plus<sep>, attr>> {};


    struct expression;

    struct formal : pegtl::seq<name, seps, pegtl::opt<pegtl::if_must<pegtl::one<'?'>, seps, expression>>> {};
//    struct formals : pegtl::seq<pegtl::star<formal, seps, pegtl::one<','>>, pegtl::opt<seps, keyword::key_ellipsis>> {};
    struct formals_nonempty : pegtl::seq<pegtl::list<formal, pegtl::one<','>, sep>, pegtl::opt<seps, pegtl::if_must<pegtl::one<','>, seps, keyword::key_ellipsis>>> {};
    struct formals : pegtl::sor<keyword::key_ellipsis, formals_nonempty> {};

    // TODO: merge argument_set_prebind into argument_single
    struct argument_single : pegtl::seq<name, pegtl::one<':'>> {};
    struct argument_formals : pegtl::seq<pegtl::one<'{'>, pegtl::pad_opt<formals, sep>, pegtl::one<'}'>> {};
    struct argument_set_prebind : pegtl::seq<name, seps, pegtl::one<'@'>, seps, argument_formals> {};
    struct argument_set_postbind : pegtl::seq<argument_formals, pegtl::opt<seps, pegtl::one<'@'>, seps, name>> {};
    struct argument_set : pegtl::seq<pegtl::sor<argument_set_prebind, argument_set_postbind>, pegtl::one<':'>> {};
    struct arguments : pegtl::list<pegtl::sor<argument_set, argument_single>, seps> {};

    struct bind_eq : pegtl::seq<attrpath, pegtl::if_must<pad<pegtl::one<'='>>, expression>> {};
    struct bind_inherit_from : pegtl::if_must<pegtl::seq<seps, pegtl::one<'('>>, pad<expression>, pegtl::one<')'>, pad<attrs>> {};
    struct bind_inherit_attr : pegtl::if_must<sep, pad<attrs>> {};
    struct bind_inherit : pegtl::if_must<keyword::key_inherit, pegtl::sor<bind_inherit_from, bind_inherit_attr>> {};
    struct binds : pegtl::list<pegtl::seq<pegtl::sor<bind_eq, bind_inherit>, pegtl::one<';'>>, seps> {};

    struct table_field_assign : pegtl::if_must<pegtl::seq<name, seps, pegtl::one<'='>>, seps, expression> {};
    // XXX: allow specifying source object with (...)
    struct table_field_inherit : pegtl::if_must<keyword::key_inherit, seps, pegtl::list<name, seps>> {};
    struct table_field : pegtl::sor<table_field_assign, table_field_inherit> {};
    struct table_field_list : pegtl::plus<table_field, pad<pegtl::one<';'>>> {};
    struct table_constructor : pegtl::if_must<pegtl::one<'{'>, pegtl::pad_opt<table_field_list, sep>, pegtl::one<'}'>> {};

    struct array_field_list : pegtl::list<expression, seps> {};
    struct array_constructor : pegtl::if_must<pegtl::one<'['>, pegtl::pad_opt<array_field_list, sep>, pegtl::one<']'>> {};

    struct bracket_expr : pegtl::if_must<pegtl::one<'('>, seps, expression, seps, pegtl::one<')'>> {};

    struct function_call_head : pegtl::sor<name, bracket_expr> {};
    //struct function_call : pegtl::seq<function_call_head, pegtl::plus<pegtl::until<pegtl::seq<seps, function_call_tail>, seps, variable_tail>>> {};

    struct assert : pegtl::if_must<keyword::key_assert, sep, expression, pad<pegtl::one<';'>>> {};
    struct with : pegtl::if_must<keyword::key_with, sep, expression, pad<pegtl::one<';'>>> {};
    struct let : pegtl::if_must<keyword::key_let, pegtl::plus<sep>, binds, pegtl::plus<sep>, keyword::key_in> {};
    struct statement : pegtl::sor<assert, with, let, arguments> {};
    struct statement_list : pegtl::star<statement> {};




    //FIXME: only to test
    struct expression : pegtl::sor<string, number, table_constructor, array_constructor, name> {};

    // XXX: remove 'seps': empty expressions are invalid
    struct grammar : pegtl::must<seps,statement_list, pegtl::sor<expression, seps>, pegtl::eof> {};


} // namespace parser

} // namespace nix






