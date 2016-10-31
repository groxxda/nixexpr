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

    struct number : public base {
        explicit number(const long long in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << data; }
        unsigned long long data;
    };


} // namespace ast


namespace parser {

namespace state {
    struct base {
        base() = default;
        base(const base&) = delete;
        void operator=(const base&) = delete;
        std::shared_ptr<ast::base> result;
    };
} // namespace state

struct identifier_first : pegtl::identifier_first {};
struct identifier_other : pegtl::sor<pegtl::identifier_other, pegtl::one<'\'', '-'>> {};
struct identifier : pegtl::seq< identifier_first, pegtl::star< identifier_other > > {};

struct line_comment : pegtl::disable<pegtl::one<'#'>, pegtl::until<pegtl::eolf>> {};
struct long_comment : pegtl::disable<pegtl::string<'/', '*'>, pegtl::until<pegtl::string<'*', '/'>>> {};
struct comment : pegtl::sor<line_comment, long_comment> {};

struct string;

template<typename CTX = void>
struct dollarcurly_expr;

struct short_string_escaped : pegtl::seq<pegtl::one<'\\'>, pegtl::one<'"', '$', '\\'>> {};
struct short_string_content : pegtl::star<pegtl::sor<short_string_escaped, dollarcurly_expr<string>, pegtl::not_one<'"'>>> {};
struct short_string : pegtl::if_must<pegtl::one<'"'>, short_string_content, pegtl::one<'"'>> {};
// XXX: add prefix stripping
struct long_string_escaped : pegtl::seq<pegtl::two<'\''>, pegtl::sor<pegtl::one<'\''>, pegtl::one<'$'>, pegtl::seq<pegtl::one<'\\'>, pegtl::one<'n', 'r', 't'>>>> {};
struct long_string_content : pegtl::star<pegtl::sor<long_string_escaped, dollarcurly_expr<string>, pegtl::seq<pegtl::not_at<pegtl::two<'\''>>, pegtl::any>>> {};
struct long_string : pegtl::if_must<pegtl::two<'\''>, long_string_content, pegtl::two<'\''>> {};
struct string : pegtl::sor<short_string, long_string> {};

struct sep : pegtl::sor<pegtl::ascii::space, comment> {};
struct seps : pegtl::star<sep> {};

template<typename R>
struct pad : pegtl::pad<R, sep> {};
template<typename R>
struct padr : pegtl::seq<R, seps> {};

template<typename S, typename O, typename P = S>
struct left_assoc : pegtl::seq<S, pegtl::star<pegtl::if_must<O, P>>> {};
template<typename S, typename O, typename P = S>
struct right_assoc :  pegtl::seq< S, pegtl::opt< pegtl::if_must< O, right_assoc< P, O > > > > {};
template<typename S, typename O, typename P = S>
struct non_assoc : pegtl::seq<S, pegtl::opt<pegtl::if_must<O, P>>> {};
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
    // until<':',sor<reserved, unreserved>>
    struct uri : pegtl::seq<pegtl::uri::scheme, pegtl::one<':'>, pegtl::star<pegtl::not_at<pegtl::sor<pegtl::one<';'>, pegtl::space, pegtl::eolf>>, pegtl::any>> {};

    struct number : pegtl::plus<pegtl::digit> {};
    struct boolean : pegtl::sor<keyword::key_true, keyword::key_false> {};

    struct attr : pegtl::sor<name> {};
    struct attrtail : padr<pegtl::sor<name, string, dollarcurly_expr<string>>> {};

    struct attrpath : pegtl::list_must<attrtail, padr<pegtl::one<'.'>>> {};

    template<typename CTX = void>
    struct expression;

    // TODO: optimize with until<>
    struct formal : pegtl::seq<padr<name>, pegtl::opt<pegtl::if_must<padr<pegtl::one<'?'>>, expression<>>>> {};
    struct formals_nonempty : pegtl::seq<pegtl::list<formal, padr<pegtl::one<','>>>, pegtl::opt<padr<pegtl::one<','>>, pegtl::opt<keyword::key_ellipsis>>> {};
    struct formals : pegtl::opt<pegtl::sor<keyword::key_ellipsis, formals_nonempty>> {};

    struct argument_single : pegtl::seq<pegtl::one<':'>, padr<pegtl::sor<sep, pegtl::at<pegtl::one<'[', '(', '{','!'>>>>> {};
    struct argument_formals : pegtl::seq<padr<pegtl::one<'{'>>, formals, pegtl::one<'}'>> {};
    struct argument_set_prebind : pegtl::seq<pegtl::if_must<padr<pegtl::one<'@'>>, padr<argument_formals>>, padr<pegtl::one<':'>>> {};
    struct argument_set_postbind : pegtl::seq<padr<argument_formals>, pegtl::opt<padr<pegtl::one<'@'>>, padr<name>>, padr<pegtl::one<':'>>> {};
    struct argument_prebind : pegtl::seq<padr<name>, pegtl::sor<argument_set_prebind, argument_single>> {};
    struct arguments : pegtl::plus<pegtl::sor<argument_set_postbind, argument_prebind>> {};

    struct bind_eq : pegtl::seq<attrpath, pegtl::if_must<padr<pegtl::one<'='>>, expression<>>> {};
    struct bind_inherit_attrnames : pegtl::star<attrtail> {};
    struct bind_inherit_from : pegtl::opt<pegtl::if_must<padr<pegtl::one<'('>>, expression<>, padr<pegtl::one<')'>>>> {};
    struct bind_inherit : pegtl::if_must<keyword::key_inherit, pegtl::opt<bind_inherit_from>, bind_inherit_attrnames> {};
    struct binds : pegtl::seq<pegtl::sor<bind_eq, bind_inherit>, padr<pegtl::one<';'>>> {};

    struct table_constructor : pegtl::if_must<pegtl::seq<pegtl::opt<keyword::key_rec>, padr<pegtl::one<'{'>>>, pegtl::until<pegtl::one<'}'>, binds>> {};

    struct expr_applying_tail;
    struct array_constructor : pegtl::if_must<padr<pegtl::one<'['>>, pegtl::until<pegtl::one<']'>, expr_applying_tail>> {};

    template<typename CTX>
    struct dollarcurly_expr : pegtl::if_must<padr<pegtl::string<'$', '{'>>, expression<CTX>, pegtl::one<'}'>> {};
    template<typename CTX>
    struct bracket_expr : pegtl::if_must<padr<pegtl::one<'('>>, expression<CTX>, pegtl::one<')'>> {};




    struct variable_tail_or : pegtl::if_must<keyword::key_or, expr_applying_tail> {};
    struct variable_tail : pegtl::seq<padr<pegtl::one<'.'>>, padr<pegtl::sor<attr, string, dollarcurly_expr<string>>>, pegtl::opt<variable_tail_or>> {};

    template<typename CTX = void>
    struct expr_select : pegtl::seq<padr<pegtl::sor<bracket_expr<CTX>, dollarcurly_expr<CTX>, table_constructor, attr>>, pegtl::star<variable_tail>> {};
    struct expr_simple : pegtl::sor<boolean, number, string, array_constructor, spath, path, uri> {};
    struct expr_applying_tail : pegtl::sor<padr<expr_simple>, expr_select<>> {};
    template<typename CTX = void>
    struct expr_applying : pegtl::seq<expr_select<CTX>, pegtl::star<pegtl::not_at<pegtl::one<';', ','>>, expr_applying_tail>> {};
    template<typename CTX = void>
    struct expr_apply : pegtl::if_then_else<padr<expr_simple>, pegtl::success, expr_applying<CTX>> {};

    template<typename CTX>
    struct expr_negate : pegtl::if_must_else<pegtl::plus<op_one<'-', '>'>>, expr_apply<number>, expr_apply<>> {};
    template<> struct expr_negate<number> : pegtl::seq<pegtl::star<op_one<'-', '>'>>, expr_apply<number>> {};

    template<typename CTX>
    struct expr_attrtest : pegtl::seq<expr_negate<CTX>, pegtl::opt<pegtl::if_must<padr<pegtl::one<'?'>>, attrpath>>> {};
 //todo: we lose typesafety here
    template<> struct expr_attrtest<boolean> : pegtl::if_must_else<padr<boolean>, pegtl::success,
                                                pegtl::if_must_else<padr<table_constructor>, pegtl::seq<padr<pegtl::one<'?'>>, attrpath>,
                                                  pegtl::seq<expr_applying<>, pegtl::opt<padr<pegtl::one<'?'>>, attrpath>>
                                                >
                                               > {};

    template<typename CTX>
    struct expr_arrayconcat : right_assoc<expr_attrtest<CTX>, padr<pegtl::two<'+'>>, expr_apply<array_constructor>> {};
    template<> struct expr_arrayconcat<array_constructor> : right_assoc<expr_apply<array_constructor>, padr<pegtl::two<'+'>>> {};

    struct operators_product : pegtl::sor<padr<pegtl::one<'*'>>, op_one<'/', '/'>> {};
    template<typename CTX>
    struct expr_product : left_assoc<expr_arrayconcat<CTX>, operators_product, expr_negate<number>> {};
    template<> struct expr_product<number> : left_assoc<expr_negate<number>, operators_product> {};

    struct operators_sum_plus : padr<pegtl::one<'+'>> {};
    struct operators_sum : pegtl::sor<operators_sum_plus, op_one<'-', '>'>> {};
    template<typename CTX>
    struct expr_sum : left_assoc<expr_product<CTX>, operators_sum, expr_product<CTX>> {};
    template<> struct expr_sum<string> : left_assoc<expr_apply<string>, operators_sum_plus, expr_apply<string>> {};


    struct expr_not : pegtl::if_then_else<pegtl::plus<padr<pegtl::one<'!'>>>, expr_attrtest<boolean>, expr_sum<void>> {};

    template<typename CTX>
    struct expr_setplus : right_assoc<expr_not, padr<pegtl::two<'/'>>, expr_apply<table_constructor>> {};
    template<> struct expr_setplus<table_constructor> : right_assoc<expr_apply<table_constructor>, padr<pegtl::two<'/'>>> {};

    struct operators_ordering : padr<pegtl::sor<pegtl::string<'<', '='>, pegtl::string<'>', '='>, pegtl::one<'<', '>'>>> {};
    template<typename CTX>
    struct expr_ordering : left_assoc<expr_setplus<CTX>, operators_ordering, expr_sum<number>> {};
    //template<> struct expr_ordering<number> : left_assoc<expr_sum<number>, operators_ordering> {};

    struct operators_equality : padr<pegtl::sor<pegtl::two<'='>, pegtl::string<'!', '='>>> {};
// todo: we cannot do anything here, right?
    template<typename CTX>
    struct expr_equality : non_assoc<expr_ordering<CTX>, operators_equality> {};

    template<typename CTX>
    struct expr_and : left_assoc<expr_equality<CTX>, padr<pegtl::two<'&'>>, expr_equality<boolean>> {};

    template<typename CTX>
    struct expr_or : left_assoc<expr_and<CTX>, padr<pegtl::two<'|'>>, expr_and<boolean>> {};

    template<typename CTX> struct expr_impl;
    template<> struct expr_impl<void> : non_assoc<expr_or<void>, padr<pegtl::string<'-', '>'>>, expr_or<boolean>> {};
    template<> struct expr_impl<boolean> : non_assoc<expr_or<boolean>, padr<pegtl::string<'-', '>'>>, expr_or<boolean>> {};


    template<typename CTX>
    struct expr_if : pegtl::if_must<keyword::key_if, expression<boolean>, keyword::key_then, expression<CTX>, keyword::key_else, expression<CTX>> {};

    // TODO: allow importing uri
    //struct expr_import : pegtl::if_must<keyword::key_import, padr<pegtl::sor<path, spath, string, name, expr_select>>, pegtl::opt<expr_applying<expr_applying_tail>>> {};


    struct assert : pegtl::if_must<keyword::key_assert, expression<boolean>, padr<pegtl::one<';'>>> {};
// todo: restrict context?
    struct with : pegtl::if_must<keyword::key_with, expression<>, padr<pegtl::one<';'>>> {};
    struct let : pegtl::if_must<keyword::key_let, pegtl::until<keyword::key_in, binds>> {};

    struct statement : pegtl::sor<assert, with, let, arguments> {}; // seq<.., /*expr_import,*/>
    struct statement_list : pegtl::star<statement> {};

    template<> struct expression<void> : pegtl::seq<statement_list, pegtl::sor<expr_if<void>, expr_impl<void>>> {};
    template<> struct expression<boolean> : pegtl::seq<statement_list, pegtl::sor<expr_if<boolean>, expr_impl<boolean>>> {};
    template<> struct expression<string> : pegtl::seq<statement_list, pegtl::sor<expr_if<string>, expr_sum<string>>> {};
    template<> struct expression<number> : pegtl::seq<statement_list, pegtl::sor<expr_if<number>, expr_sum<number>>> {};
    template<> struct expression<table_constructor> : pegtl::seq<statement_list, pegtl::sor<expr_if<table_constructor>, expr_setplus<table_constructor>>> {};
    template<> struct expression<array_constructor> : pegtl::seq<statement_list, pegtl::sor<expr_if<array_constructor>, expr_arrayconcat<array_constructor>>> {};


    struct grammar : pegtl::must<seps, expression<>, pegtl::eof> {};

    struct control {
        template<typename Rule>
        struct normal : pegtl::normal<Rule> {};
    //    template<typename Rule>
    //    struct tracer : normal<Rule> {};
    };
    //template<> struct control::tracer<grammar> : pegtl::tracer<grammar> {};
    //template<typename x> struct control::tracer<expression<x>> : pegtl::tracer<expression<x>> {};
    //template<> struct control::tracer<short_string_content> : pegtl::normal<short_string_content> {};

    template<typename Rule>
    struct value_errors : pegtl::normal<Rule> {
        static const std::string error_message;
        template<typename Input, typename... States>
        static void raise(const Input& in, States&& ...) {
            throw pegtl::parse_error(error_message, in);
        }
    };

    template<typename Rule> struct value_action : pegtl::nothing<Rule> {};
    template<> struct value_action<number> {
        template<typename Input>
        static void apply(const Input& in, state::base& result) {
            result.result = std::make_shared<ast::number>(std::stoll(in.string()));
        }
    };

    template<> struct control::normal<number> : pegtl::change_action<number, value_action, value_errors> {};



} // namespace parser

} // namespace nix






