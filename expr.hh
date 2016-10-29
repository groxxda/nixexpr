#pragma once

#include <pegtl.hh>
#include <pegtl/trace.hh>
#include <pegtl/contrib/changes.hh>
#include <pegtl/contrib/unescape.hh>
#include <pegtl/contrib/uri.hh>

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

struct identifier_first : pegtl::identifier_first {};
struct identifier_other : pegtl::sor<pegtl::identifier_other, pegtl::one<'\'', '-'>> {};
struct identifier : pegtl::seq< identifier_first, pegtl::star< identifier_other > > {};

struct line_comment : pegtl::disable<pegtl::one<'#'>, pegtl::until<pegtl::eolf>> {};
struct long_comment : pegtl::disable<pegtl::string<'/', '*'>, pegtl::until<pegtl::string<'*', '/'>>> {};
struct comment : pegtl::sor<line_comment, long_comment> {};

struct dollarcurly_expr;

struct short_string_escaped : pegtl::seq<pegtl::one<'\\'>, pegtl::one<'"', '$', '\\'>> {};
struct short_string_content : pegtl::star<pegtl::sor<short_string_escaped, dollarcurly_expr, pegtl::not_one<'"'>>> {};
struct short_string : pegtl::if_must<pegtl::one<'"'>, short_string_content, pegtl::one<'"'>> {};
// XXX: add prefix stripping
struct long_string_escaped : pegtl::seq<pegtl::two<'\''>, pegtl::sor<pegtl::one<'\''>, pegtl::one<'$'>, pegtl::seq<pegtl::one<'\\'>, pegtl::one<'n', 'r', 't'>>>> {};
struct long_string_content : pegtl::star<pegtl::sor<long_string_escaped, dollarcurly_expr, pegtl::seq<pegtl::not_at<pegtl::two<'\''>>, pegtl::any>>> {};
struct long_string : pegtl::if_must<pegtl::two<'\''>, long_string_content, pegtl::two<'\''>> {};
struct string : pegtl::sor<short_string, long_string> {};

struct sep : pegtl::sor<pegtl::ascii::space, comment> {};
struct seps : pegtl::star<sep> {};

template<typename R>
struct pad : pegtl::pad<R, sep> {};
template<typename R>
struct padr : pegtl::seq<R, seps> {};

template<typename S, typename O>
struct left_assoc : pegtl::seq<S, pegtl::star<pegtl::if_must<O, S>>> {};
template<typename S, typename O>
struct right_assoc :  pegtl::seq< S, pegtl::opt< pegtl::if_must< O, right_assoc< S, O > > > > {};
template<typename S, typename O>
struct non_assoc : pegtl::seq<S, pegtl::opt<pegtl::if_must<O, S>>> {};
template< char O, char ... N >
struct op_one : pegtl::seq< pegtl::one< O >, pegtl::if_then_else<pegtl::plus<sep>, pegtl::success, pegtl::at< pegtl::not_one< N ... > >> > {};


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
    struct str_ellipsis: pegtl::string<'.', '.', '.'>                     {};
    struct str_true    : pegtl::string<'t', 'r', 'u', 'e'>                {};
    struct str_false   : pegtl::string<'f', 'a', 'l', 's', 'e'>           {};
    struct str_import  : pegtl::string<'i', 'm', 'p', 'o', 'r', 't'>      {};

    // we have to allow legacy attrname or... cran has an attribute named import
    struct str_forbidden:pegtl::sor<str_if, str_then, str_else, str_assert, str_with, str_let, str_in, str_rec, str_inherit, str_true, str_false> {};
    struct str_any     : pegtl::sor<str_forbidden, str_or> {};

    template<typename Key>
    struct key         : pegtl::seq<Key, pegtl::if_then_else<pegtl::plus<sep>, pegtl::success, pegtl::not_at<identifier_other>>> {};

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
    struct key_ellipsis: key<str_ellipsis>{};
    struct key_true    : key<str_true>    {};
    struct key_false   : key<str_false>   {};
    struct key_import  : key<str_import>  {};

    struct forbidden   : pegtl::seq<str_forbidden, pegtl::not_at<identifier_other>> {};
    struct any         : key<str_any>     {};

} // namespace keyword

    struct name : pegtl::seq<pegtl::at<identifier_first>, pegtl::not_at<keyword::forbidden>, identifier> {};

    struct path_char : pegtl::sor<pegtl::identifier_other, pegtl::one<'.', '-', '+'>> {};
    struct path : pegtl::seq<pegtl::star<path_char>, pegtl::plus<pegtl::one<'/'>, pegtl::plus<path_char>>> {};
    struct spath : pegtl::seq<pegtl::one<'<'>, pegtl::plus<path_char>, pegtl::star<pegtl::one<'/'>, pegtl::plus<path_char>> , pegtl::one<'>'>> {};

    // XXX: we cannot allow ; in uris...
    struct uri : pegtl::seq<pegtl::uri::scheme, pegtl::one<':'>, pegtl::star<pegtl::not_at<pegtl::sor<pegtl::one<';'>, pegtl::space, pegtl::eolf>>, pegtl::any>> {};

    struct number : pegtl::plus<pegtl::digit> {};
    struct boolean : pegtl::sor<keyword::key_true, keyword::key_false> {};

    struct attr : pegtl::sor<name> {};
    struct attrtail : pegtl::sor<name, string, dollarcurly_expr> {};

    struct attrpath : pegtl::list_must<attrtail, pegtl::one<'.'>, sep> {};


    struct expression;

    struct formal : pegtl::seq<padr<name>, pegtl::opt<pegtl::if_must<padr<pegtl::one<'?'>>, expression>>> {};
    struct formals_nonempty : pegtl::seq<pegtl::list<formal, padr<pegtl::one<','>>>, pegtl::opt<padr<pegtl::one<','>>, pegtl::opt<keyword::key_ellipsis>>> {};
    struct formals : pegtl::sor<keyword::key_ellipsis, formals_nonempty> {};

    // TODO: merge prefix of argument_set_prebind and argument_single
    struct argument_single : pegtl::seq<padr<name>, pegtl::one<':'>, padr<pegtl::sor<sep, pegtl::at<pegtl::one<'[', '(', '{','!'>>>>> {};
    struct argument_formals : pegtl::seq<pegtl::one<'{'>, pegtl::pad_opt<formals, sep>, pegtl::one<'}'>> {};
    struct argument_set_prebind : pegtl::seq<padr<name>, pegtl::if_must<padr<pegtl::one<'@'>>, padr<argument_formals>>> {};
    struct argument_set_postbind : pegtl::seq<padr<argument_formals>, pegtl::opt<padr<pegtl::one<'@'>>, padr<name>>> {};
    struct argument_set : pegtl::seq<pegtl::sor<argument_set_prebind, argument_set_postbind>, padr<pegtl::one<':'>>> {};
    struct arguments : pegtl::plus<pegtl::sor<argument_set, argument_single>> {};

    struct bind_eq : pegtl::seq<attrpath, pegtl::if_must<pad<pegtl::one<'='>>, expression>> {};
    struct bind_inherit_attrnames : pegtl::star<padr<attrtail>> {};
    struct bind_inherit_from : pegtl::opt<pegtl::if_must<padr<pegtl::one<'('>>, expression, padr<pegtl::one<')'>>>> {};
    struct bind_inherit : pegtl::if_must<keyword::key_inherit, pegtl::opt<bind_inherit_from>, bind_inherit_attrnames> {};
    struct binds : pegtl::plus<pegtl::seq<pegtl::sor<bind_eq, bind_inherit>, padr<pegtl::one<';'>>>> {};

    struct table_constructor : pegtl::if_must<pegtl::seq<pegtl::opt<keyword::key_rec>, padr<pegtl::one<'{'>>>, pegtl::opt<binds>, pegtl::one<'}'>> {};

    // TODO: bracket_expr, dollar_curly, atomics
    struct array_field_list : pegtl::star<expression> {};
    struct array_constructor : pegtl::if_must<padr<pegtl::one<'['>>, array_field_list, pegtl::one<']'>> {};

    struct dollarcurly_expr : pegtl::if_must<padr<pegtl::string<'$', '{'>>, expression, pegtl::one<'}'>> {};
    struct bracket_expr : pegtl::if_must<padr<pegtl::one<'('>>, expression, pegtl::one<')'>> {};

    struct assert : pegtl::if_must<keyword::key_assert, expression, padr<pegtl::one<';'>>> {};
    struct with : pegtl::if_must<keyword::key_with, expression, padr<pegtl::one<';'>>> {};
    struct let : pegtl::if_must<keyword::key_let, pegtl::opt<binds>, keyword::key_in> {};
    struct statement : pegtl::if_must<pegtl::sor<assert, with, let, arguments>, expression> {};
    struct statement_list;



    struct expr_thirteen;

    struct variable_tail_or : pegtl::if_must<keyword::key_or, expression> {};
    struct variable_tail : pegtl::seq<padr<pegtl::one<'.'>>, padr<pegtl::sor<attr, string, dollarcurly_expr>>, pegtl::opt<variable_tail_or>> {};

    struct expr_ten;
    struct expr_thirteen : pegtl::seq<pegtl::sor<bracket_expr, dollarcurly_expr, table_constructor, attr, statement_list>, pegtl::star<variable_tail>> {};
    struct expr_twelve : pegtl::sor<boolean, number, string, uri, expr_thirteen, array_constructor, path, spath> {};
    struct expr_ten : pegtl::plus<padr<expr_twelve>> {};

    struct expr_negate : pegtl::seq<pegtl::star<op_one<'-', '>'>>, expr_ten> {};

    struct expr_attrtest : non_assoc<expr_negate, padr<pegtl::one<'?'>>> {};

    struct expr_arrayconcat : right_assoc<expr_attrtest, padr<pegtl::two<'+'>>> {};

    struct operators_product : pegtl::sor<padr<pegtl::one<'*'>>, op_one<'/', '/'>> {};
    struct expr_product : left_assoc<expr_arrayconcat, operators_product> {};

    struct operators_sum : pegtl::sor<padr<pegtl::one<'+'>>, op_one<'-', '>'>> {};
    struct expr_sum : left_assoc<expr_product, operators_sum> {};

    struct expr_not : pegtl::seq<pegtl::star<padr<pegtl::one<'!'>>>, expr_sum> {};

    struct expr_setplus : right_assoc<expr_not, padr<pegtl::two<'/'>>> {};

    struct operators_ordering : padr<pegtl::sor<pegtl::string<'<', '='>, pegtl::string<'>', '='>, pegtl::one<'<', '>'>>> {};
    struct expr_ordering : left_assoc<expr_setplus, operators_ordering> {};

    struct operators_equality : padr<pegtl::sor<pegtl::two<'='>, pegtl::string<'!', '='>>> {};
    struct expr_equality : non_assoc<expr_ordering, operators_equality> {};

    struct expr_and : left_assoc<expr_equality, padr<pegtl::two<'&'>>> {};
    struct expr_or : left_assoc<expr_and, padr<pegtl::two<'|'>>> {};
    struct expr_impl : non_assoc<expr_or, padr<pegtl::string<'-', '>'>>> {};


    struct expr_if : pegtl::if_must<keyword::key_if, expression, keyword::key_then, expression, keyword::key_else, expression> {};

    struct expr_import : pegtl::if_must<keyword::key_import, expression> {};

    struct expression : pegtl::sor<statement_list, expr_impl> {};

    struct statement_list : pegtl::sor<statement, expr_import, expr_if> {};

    struct grammar : pegtl::must<seps, expression, pegtl::eof> {};


} // namespace parser

} // namespace nix






