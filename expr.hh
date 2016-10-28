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
//struct long_string : any_string<op_two<'\'', '\'', '\''>> {};
struct string : pegtl::sor<short_string, long_string> {};

struct sep : pegtl::sor<pegtl::ascii::space, comment> {};
struct seps : pegtl::star<sep> {};

template<typename R>
struct pad : pegtl::pad<R, sep> {};
template<typename R>
struct padr : pegtl::seq<R, seps> {};

template<typename S, typename O>
struct left_assoc : pegtl::seq<S, seps, pegtl::star<pegtl::if_must<O, seps, S, seps>>> {};
template<typename S, typename O>
struct right_assoc :  pegtl::seq< S, seps, pegtl::opt< pegtl::if_must< O, seps, right_assoc< S, O > > > > {};
template<typename S, typename O>
struct non_assoc : pegtl::seq<S, seps, pegtl::opt<pegtl::if_must<O, seps, S, seps>>> {};
template< char O, char ... N >
struct op_one : pegtl::seq< pegtl::one< O >, pegtl::at< pegtl::not_one< N ... > > > {};
template< char O, char P, char ... N >
struct op_two : pegtl::seq< pegtl::string< O, P >, pegtl::at< pegtl::not_one< N ... > > > {};

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
    struct key         : pegtl::seq<Key, pegtl::not_at<identifier_other>> {};

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

    struct forbidden   : key<str_forbidden>{};
    struct any         : key<str_any>     {};

} // namespace keyword



    struct name : pegtl::seq<pegtl::not_at<keyword::forbidden>, identifier> {};

    struct path_char : pegtl::sor<pegtl::identifier_other, pegtl::one<'.', '-', '+'>> {};
    struct path : pegtl::seq<pegtl::star<path_char>, pegtl::plus<pegtl::one<'/'>, pegtl::plus<path_char>>> {};
    struct spath : pegtl::seq<pegtl::one<'<'>, pegtl::plus<path_char>, pegtl::star<pegtl::one<'/'>, pegtl::plus<path_char>> , pegtl::one<'>'>> {};

    // XXX: we cannot allow ; in uris...
    struct uri : pegtl::seq<pegtl::uri::scheme, pegtl::one<':'>, pegtl::star<pegtl::not_at<pegtl::sor<pegtl::one<';'>, pegtl::space, pegtl::eolf>>, pegtl::any>> {};

    struct number : pegtl::plus<pegtl::digit> {};
    struct boolean : pegtl::sor<keyword::key_true, keyword::key_false> {};

    // XXX: add string_attr
    struct attr : pegtl::sor<name, string, dollarcurly_expr> {};
    struct attrpath : pegtl::list_must<attr, pegtl::one<'.'>, sep> {};
    //struct attrpath : pegtl::sor<attrpath_> {};
    // XXX: can be empty in original
    struct attrs : pegtl::seq<attr, pegtl::star<pegtl::plus<sep>, attr>> {};


    struct expression;

    struct formal : pegtl::seq<name, seps, pegtl::opt<pegtl::if_must<pegtl::one<'?'>, seps, expression>>> {};
//    struct formals : pegtl::seq<pegtl::star<formal, seps, pegtl::one<','>>, pegtl::opt<seps, keyword::key_ellipsis>> {};
    struct formals_nonempty : pegtl::seq<pegtl::list<formal, pegtl::one<','>, sep>, pegtl::opt<seps, pegtl::one<','>, seps, pegtl::opt<keyword::key_ellipsis>>> {};
    struct formals : pegtl::sor<keyword::key_ellipsis, formals_nonempty> {};

    // TODO: merge prefix of argument_set_prebind and argument_single
    struct argument_single : pegtl::seq<padr<name>, pegtl::one<':'>, padr<pegtl::sor<sep, pegtl::at<pegtl::one<'[', '(', '{','!'>>>>> {};
    struct argument_formals : pegtl::seq<pegtl::one<'{'>, pegtl::pad_opt<formals, sep>, pegtl::one<'}'>> {};
    struct argument_set_prebind : pegtl::seq<padr<name>, pegtl::if_must<padr<pegtl::one<'@'>>, padr<argument_formals>>> {};
    struct argument_set_postbind : pegtl::seq<padr<argument_formals>, pegtl::opt<padr<pegtl::one<'@'>>, padr<name>>> {};
    struct argument_set : pegtl::seq<pegtl::sor<argument_set_prebind, argument_set_postbind>, padr<pegtl::one<':'>>> {};
    struct arguments : pegtl::plus<pegtl::sor<argument_set, argument_single>> {};

    struct bind_eq : pegtl::seq<attrpath, pegtl::if_must<pad<pegtl::one<'='>>, expression>> {};
    struct bind_inherit_from : pegtl::if_must<pegtl::seq<seps, pegtl::one<'('>>, pad<expression>, pegtl::one<')'>, pegtl::opt<pad<attrs>>> {};
    struct bind_inherit_attr : pegtl::opt<pad<attrs>> {};
    struct bind_inherit : pegtl::if_must<keyword::key_inherit, pegtl::sor<bind_inherit_from, bind_inherit_attr>> {};
    struct binds : pegtl::list<pegtl::seq<pegtl::sor<bind_eq, bind_inherit>, pegtl::one<';'>>, seps> {};

    struct table_field_assign : pegtl::if_must<pegtl::seq<attrpath, seps, pegtl::one<'='>>, seps, expression> {};
    // XXX: allow specifying source object with (...)
    struct table_field_inherit : pegtl::if_must<keyword::key_inherit, pegtl::sor<bind_inherit_from, bind_inherit_attr>> {};
    //struct table_field_inherit : pegtl::if_must<keyword::key_inherit, seps, pegtl::list<name, seps>> {};
    struct table_field : pegtl::sor<table_field_assign, table_field_inherit> {};
    struct table_field_list : pegtl::plus<table_field, pad<pegtl::one<';'>>> {};
    struct table_constructor : pegtl::if_must<pegtl::seq<padr<pegtl::opt<keyword::key_rec>>, pegtl::one<'{'>>, pegtl::pad_opt<table_field_list, sep>, pegtl::one<'}'>> {};

    struct array_field_list : pegtl::list<expression, seps> {};
    struct array_constructor : pegtl::if_must<pegtl::one<'['>, pegtl::pad_opt<array_field_list, sep>, pegtl::one<']'>> {};

    struct dollarcurly_expr : pegtl::if_must<pegtl::string<'$', '{'>, pad<expression>, pegtl::one<'}'>> {};
    struct bracket_expr : pegtl::if_must<pegtl::one<'('>, pad<expression>, pegtl::one<')'>> {};

    //struct function_call_head : pegtl::sor<name, bracket_expr> {};
    //struct function_call : pegtl::seq<function_call_head, pegtl::plus<pegtl::until<pegtl::seq<seps, function_call_tail>, seps, variable_tail>>> {};

    struct assert : pegtl::if_must<keyword::key_assert, sep, expression, pad<pegtl::one<';'>>> {};
    struct with : pegtl::if_must<keyword::key_with, pad<expression>, padr<pegtl::one<';'>>> {};
    struct let : pegtl::if_must<keyword::key_let, pad<pegtl::opt<binds>>, padr<keyword::key_in>> {};
    struct statement : pegtl::if_must<pegtl::sor<assert, with, let, arguments>, seps, expression> {};
    struct expr_import;
    struct expr_if;
    struct statement_list : pegtl::sor<statement, expr_import, expr_if> {};


    // XXX: be more specific, support []!=[], support function call without parenthesis
    //struct negate_expression : pegtl::if_must<pegtl::plus<padr<pegtl::one<'!'>>>, pegtl::sor<boolean, bracket_expr>> {};
    //struct boolean_expression : pegtl::sor<boolean, negate_expression> {};
    //struct boolean_expression : pegtl::seq<pegtl::star<padr<pegtl::one<'!'>>>, pegtl::sor<boolean, bracket_expr>> {};

    //struct negate_expression : pegtl::seq<pegtl::star<padr<


    struct expr_thirteen;

    struct variable_tail_or : pegtl::if_must<padr<keyword::key_or>, expression> {};
    struct variable_tail : pegtl::seq<padr<pegtl::one<'.'>>, padr<pegtl::sor<attr, string, dollarcurly_expr>>, pegtl::opt<variable_tail_or>> {};

    struct unary_operators : pegtl::sor<op_one<'-', '>'>, op_one<'!', '='>> {};

    struct expr_ten;
    struct expr_thirteen : pegtl::seq<pegtl::sor<bracket_expr, dollarcurly_expr, table_constructor, attr, statement_list>, pegtl::star<seps, variable_tail>> {};
    struct expr_twelve : pegtl::sor<boolean, number, string, uri, expr_thirteen, array_constructor, path, spath> {};
    struct expr_eleven : pegtl::seq<expr_twelve, seps, pegtl::opt<expr_ten, seps>> {};
    struct unary_apply : pegtl::if_must<unary_operators, seps, expr_ten, seps> {};
    struct expr_ten : pegtl::sor<unary_apply, expr_eleven> {};

    struct expr_ninesix : non_assoc<expr_ten, op_one<'-', '>'>> {};

    struct expr_ninefour : non_assoc<expr_ninesix, pegtl::one<'?'>> {};

    struct expr_ninetwo : right_assoc<expr_ninefour, pegtl::two<'+'>> {};

    struct operators_nine : pegtl::sor<pegtl::one<'*'>, op_one<'/', '/'>> {};
    struct expr_nine : left_assoc<expr_ninetwo, operators_nine> {};

    struct operators_eight : pegtl::sor<pegtl::one<'+'>, op_one<'-', '>'>> {};
    struct expr_eight : left_assoc<expr_nine, operators_eight> {};

    //struct expr_five : left_assoc<expr_eight, pegtl::one<'!'>> {};

    struct expr_four : right_assoc<expr_eight, pegtl::two<'/'>> {};

    struct operators_three : pegtl::sor<pegtl::string<'<', '='>, pegtl::string<'>', '='>, pegtl::one<'<', '>'>> {};
    struct expr_three : left_assoc<expr_four, operators_three> {};

    struct operators_two : pegtl::sor<pegtl::two<'='>, pegtl::string<'!', '='>> {};
    struct expr_two : non_assoc<expr_three, operators_two> {};

    struct expr_one : left_assoc<expr_two, pegtl::two<'&'>> {};
    struct expr_zero : left_assoc<expr_one, pegtl::two<'|'>> {};
    struct expr_op : non_assoc<expr_zero, pegtl::string<'-', '>'>> {};


    // XXX: if[]!=[]then[]else[] should be valid....
    struct if_separator : pegtl::sor<pegtl::at<pegtl::one<'{', '(', '[', '"'>>, pegtl::plus<sep>> {};
    struct expr_if : pegtl::if_must<keyword::key_if, if_separator, expression, seps, keyword::key_then, if_separator, expression, seps, keyword::key_else, if_separator, expression> {};

    struct expr_import : pegtl::if_must<padr<keyword::key_import>, expression> {};

    struct expression : pegtl::sor<statement_list, expr_op> {};


    //FIXME: only to test
    //struct expression : pegtl::sor<if_then_else, boolean_expression, boolean, string, number, table_constructor, array_constructor, name> {};


    // XXX: remove 'seps': empty expressions are invalid
    struct grammar : pegtl::must<seps, expression, pegtl::eof> {};


} // namespace parser

} // namespace nix






