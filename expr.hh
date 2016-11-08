#pragma once

#include <pegtl.hh>
#include <pegtl/contrib/changes.hh>
#include <pegtl/contrib/unescape.hh>
#include <pegtl/contrib/uri.hh>
#include <pegtl/trace.hh>

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <typeinfo>
#include <vector>

#include "ast.hh"

namespace nix {
namespace parser {

namespace state {
struct base {
    base() = default;
    base(const base&) = delete;
    void operator=(const base&) = delete;
    std::unique_ptr<ast::base> value;

    void success(base& in_result) {
        assert(!in_result.value);
        assert(value);
        in_result.value = std::move(value);
    }
};

struct any_string : base {
    std::vector<std::unique_ptr<ast::base>> data;
    std::string unescaped;

    void push_back() {
        assert(!value);
        data.push_back(
            std::make_unique<ast::string_literal>(std::move(unescaped)));
        unescaped = std::string();
    }

    void push_back_expr() {
        if(unescaped.length()) {
            data.push_back(
                std::make_unique<ast::string_literal>(std::move(unescaped)));
            unescaped = std::string();
        }
        assert(value);
        data.push_back(std::make_unique<ast::dollar_curly>(std::move(value)));
    }
};

struct string : any_string {
    void success(base& in_result) {
        assert(!value);
        assert(!in_result.value);
        in_result.value = std::make_unique<ast::short_string>(std::move(data));
    }
};

struct long_string : any_string {
    unsigned int prefix_len;

    void prefix(unsigned int prefix) {
        prefix_len = std::min(prefix_len, prefix);
    }

    void success(base& in_result) {
        assert(!value);
        assert(!in_result.value);
        in_result.value =
            std::make_unique<ast::long_string>(std::move(data), prefix_len);
    }
};

template <typename T> struct binary_expression : base {
    void success(base& in_result) {
        assert(in_result.value);
        assert(value);
        in_result.value =
            std::make_unique<T>(std::move(in_result.value), std::move(value));
    }
};

struct if_then_else : base {
    std::unique_ptr<ast::base> test;
    std::unique_ptr<ast::base> then_expr;

    void set_test() {
        assert(value);
        assert(!test);
        test = std::move(value);
    }

    void set_then() {
        assert(test);
        assert(value);
        assert(!then_expr);
        then_expr = std::move(value);
    }

    void success(base& in_result) {
        assert(!in_result.value);
        assert(test);
        assert(then_expr);
        assert(value);
        in_result.value = std::make_unique<ast::if_then_else>(
            std::move(test), std::move(then_expr), std::move(value));
    }
};

struct binding_inherit : base {
    std::unique_ptr<ast::base> from;
    std::vector<std::unique_ptr<ast::base>> attrs;

    void set_from() {
        assert(value);
        from = std::move(value);
    }

    void push_back() {
        assert(value);
        attrs.push_back(std::move(value));
    }

    void success(base& in_result) {
        assert(!in_result.value);
        assert(!value);
        in_result.value = std::make_unique<ast::binding_inherit>(
            std::move(from), std::move(attrs));
    }
};

struct binds : base {
    std::vector<std::unique_ptr<ast::base>> data;

    void push_back() {
        assert(value);
        assert(dynamic_cast<ast::binding_eq*>(value.get()) ||
               dynamic_cast<ast::binding_inherit*>(value.get()));
        data.push_back(std::move(value));
    }

    void success(base& in_result) {
        assert(!value);
        assert(!in_result.value);
        in_result.value = std::make_unique<ast::binds>(std::move(data));
    }
};

struct formals : base {
    std::vector<std::unique_ptr<ast::base>> data;

    void push_back() {
        assert(value);
        // assert(std::dynamic_pointer_cast<ast::formal>(value));
        data.push_back(std::move(value));
    }

    void success(base& in_result) {
        assert(!value);
        assert(!in_result.value);
        in_result.value = std::make_unique<ast::formals>(std::move(data));
    }
};

struct array : base {
    std::vector<std::unique_ptr<ast::base>> data;

    void push_back() {
        assert(value);
        data.push_back(std::move(value));
    };

    void success(base& in_result) {
        assert(!in_result.value);
        assert(!value);
        in_result.value = std::make_unique<ast::array>(std::move(data));
    }
};
} // namespace state

struct identifier_first : pegtl::identifier_first {};
struct identifier_other
    : pegtl::sor<pegtl::identifier_other, pegtl::one<'\'', '-'>> {};
struct identifier
    : pegtl::seq<identifier_first, pegtl::star<identifier_other>> {};

struct line_comment
    : pegtl::disable<pegtl::one<'#'>, pegtl::until<pegtl::eolf>> {};
struct long_comment : pegtl::disable<pegtl::string<'/', '*'>,
                                     pegtl::until<pegtl::string<'*', '/'>>> {};
struct comment : pegtl::sor<line_comment, long_comment> {};

struct string;

struct dollarcurly_expr;

struct short_string_escaped : pegtl::one<'"', '$', '\\', 'n'> {};
struct short_string_accepted : pegtl::any {};
struct short_string_character
    : pegtl::sor<pegtl::seq<pegtl::one<'\\'>, short_string_escaped>,
                 short_string_accepted> {};
struct short_string_content
    : pegtl::until<pegtl::one<'"'>,
                   pegtl::sor<dollarcurly_expr, short_string_character>> {};
struct short_string : pegtl::if_must<pegtl::one<'"'>, short_string_content> {};
// XXX: add prefix stripping

struct long_string_escape_accepted
    : pegtl::sor<pegtl::one<'\''>,
                 pegtl::one<'$'>,
                 pegtl::seq<pegtl::one<'\\'>, pegtl::one<'n', 'r', 't'>>> {};
struct long_string_escaped
    : pegtl::seq<pegtl::two<'\''>, long_string_escape_accepted> {};
struct long_string_accepted
    : pegtl::seq<pegtl::not_at<pegtl::two<'\''>>, pegtl::any> {};
struct long_string_content : pegtl::star<pegtl::sor<long_string_escaped,
                                                    dollarcurly_expr,
                                                    long_string_accepted>> {};
struct long_string
    : pegtl::if_must<pegtl::two<'\''>, long_string_content, pegtl::two<'\''>> {
};

struct string : pegtl::sor<short_string, long_string> {};

struct sep : pegtl::sor<pegtl::ascii::space, comment> {};
struct seps : pegtl::star<sep> {};

template <typename R> struct pad : pegtl::pad<R, sep> {};
template <typename R> struct padr : pegtl::seq<R, seps> {};

template <typename S, typename O, typename P = S>
struct left_assoc : pegtl::seq<S, pegtl::star<pegtl::if_must<O, P>>> {};
template <typename S, typename O, typename P = S>
struct right_assoc
    : pegtl::seq<S, pegtl::opt<pegtl::if_must<O, right_assoc<P, O>>>> {};
template <typename S, typename O, typename P = S>
struct non_assoc : pegtl::seq<S, pegtl::opt<pegtl::if_must<O, P>>> {};
template <char O, char... N>
struct op_one
    : pegtl::seq<pegtl::one<O>,
                 pegtl::if_then_else<pegtl::plus<sep>,
                                     pegtl::success,
                                     pegtl::at<pegtl::not_one<N...>>>> {};
template <char O, char P, char... N>
struct op_two
    : pegtl::seq<padr<pegtl::one<O>>,
                 pegtl::one<P>,
                 pegtl::if_then_else<pegtl::plus<sep>,
                                     pegtl::success,
                                     pegtl::at<pegtl::not_one<N...>>>> {};

namespace keyword {
struct str_if : pegtl::string<'i', 'f'> {};
struct str_then : pegtl::string<'t', 'h', 'e', 'n'> {};
struct str_else : pegtl::string<'e', 'l', 's', 'e'> {};
struct str_assert : pegtl::string<'a', 's', 's', 'e', 'r', 't'> {};
struct str_with : pegtl::string<'w', 'i', 't', 'h'> {};
struct str_let : pegtl::string<'l', 'e', 't'> {};
struct str_in : pegtl::string<'i', 'n'> {};
struct str_rec : pegtl::string<'r', 'e', 'c'> {};
struct str_inherit : pegtl::string<'i', 'n', 'h', 'e', 'r', 'i', 't'> {};
struct str_or : pegtl::string<'o', 'r'> {};
struct str_ellipsis : pegtl::string<'.', '.', '.'> {};
struct str_true : pegtl::string<'t', 'r', 'u', 'e'> {};
struct str_false : pegtl::string<'f', 'a', 'l', 's', 'e'> {};
struct str_import : pegtl::string<'i', 'm', 'p', 'o', 'r', 't'> {};

// we have to allow legacy attrname or... cran has an attribute named import
struct str_forbidden : pegtl::sor<str_if,
                                  str_then,
                                  str_else,
                                  str_assert,
                                  str_with,
                                  str_let,
                                  str_rec,
                                  str_inherit,
                                  str_in,
                                  str_true,
                                  str_false> {};
struct str_any : pegtl::sor<str_forbidden, str_or> {};

template <typename Key>
struct key : pegtl::seq<Key,
                        pegtl::if_then_else<pegtl::plus<sep>,
                                            pegtl::success,
                                            pegtl::not_at<identifier_other>>> {
};

struct key_if : key<str_if> {};
struct key_then : key<str_then> {};
struct key_else : key<str_else> {};
struct key_assert : key<str_assert> {};
struct key_with : key<str_with> {};
struct key_let : key<str_let> {};
struct key_in : key<str_in> {};
struct key_rec : key<str_rec> {};
struct key_inherit : key<str_inherit> {};
struct key_or : key<str_or> {};
struct key_ellipsis : key<str_ellipsis> {};
struct key_true : key<str_true> {};
struct key_false : key<str_false> {};
struct key_import : key<str_import> {};

struct forbidden : pegtl::seq<str_forbidden, pegtl::not_at<identifier_other>> {
};
struct any : key<str_any> {};

} // namespace keyword

struct do_backtrack : pegtl::success {};
template <typename from>
struct backtrack : pegtl::seq<do_backtrack, pegtl::failure> {};

struct name : pegtl::seq<pegtl::at<identifier_first>,
                         pegtl::not_at<keyword::forbidden>,
                         identifier> {};

struct path_char
    : pegtl::sor<pegtl::identifier_other, pegtl::one<'.', '-', '+'>> {};
struct path : pegtl::seq<pegtl::star<path_char>,
                         pegtl::plus<pegtl::one<'/'>, pegtl::plus<path_char>>> {
};
struct spath : pegtl::seq<pegtl::one<'<'>,
                          pegtl::plus<path_char>,
                          pegtl::star<pegtl::one<'/'>, pegtl::plus<path_char>>,
                          pegtl::one<'>'>> {};

// XXX: we cannot allow ; in uris...
// until<':',sor<reserved, unreserved>>
struct uri
    : pegtl::seq<
          pegtl::uri::scheme,
          pegtl::one<':'>,
          pegtl::star<
              pegtl::not_at<
                  pegtl::sor<pegtl::one<';'>, pegtl::space, pegtl::eolf>>,
              pegtl::any>> {};

struct number : pegtl::plus<pegtl::digit> {};
struct boolean : pegtl::sor<keyword::key_true, keyword::key_false> {};

struct attr : pegtl::sor<name, string, dollarcurly_expr> {};
struct attrtail : pegtl::sor<name, string, dollarcurly_expr> {};


template <typename op>
struct binary_expr_apply : pegtl::if_must<op, typename op::next> {};


struct operator_attrpath : padr<pegtl::one<'.'>> {
    using ast = ast::attrpath;
    using next = attrtail;
};
struct attrpath
    : pegtl::seq<padr<attr>,
                 pegtl::star<binary_expr_apply<operator_attrpath>>> {};

template <typename CTX = void> struct expression;


struct operator_formal : padr<pegtl::one<'?'>> {
    using ast = ast::formal;
    using next = expression<>;
};
struct formal
    : pegtl::seq<padr<name>, pegtl::opt<binary_expr_apply<operator_formal>>> {};

struct formals_nonempty
    : pegtl::seq<pegtl::list<formal, padr<pegtl::one<','>>>,
                 pegtl::opt<padr<pegtl::one<','>>,
                            pegtl::opt<keyword::key_ellipsis>>> {};
struct formals
    : pegtl::opt<pegtl::sor<keyword::key_ellipsis, formals_nonempty>> {};

struct argument_single
    : pegtl::seq<
          pegtl::one<':'>,
          padr<pegtl::sor<sep, pegtl::at<pegtl::one<'[', '(', '{', '!'>>>>> {};
struct argument_formals
    : pegtl::seq<padr<pegtl::one<'{'>>, formals, pegtl::one<'}'>> {};
struct argument_set_prebind
    : pegtl::seq<pegtl::if_must<padr<pegtl::one<'@'>>, padr<argument_formals>>,
                 padr<pegtl::one<':'>>> {};
struct argument_set_postbind
    : pegtl::sor<pegtl::seq<padr<argument_formals>,
                            pegtl::opt<padr<pegtl::one<'@'>>, padr<name>>,
                            padr<pegtl::one<':'>>>,
                 backtrack<argument_set_postbind>> {};
struct argument_prebind : pegtl::seq<padr<name>,
                                     pegtl::sor<argument_set_prebind,
                                                argument_single,
                                                backtrack<argument_prebind>>> {
};
struct arguments : pegtl::sor<argument_set_postbind, argument_prebind> {};

struct bind;

template <typename U> struct binds : pegtl::until<U, bind> {};

struct table_begin_recursive
    : pegtl::seq<keyword::key_rec, padr<pegtl::one<'{'>>> {};
struct table_begin_nonrecursive : padr<pegtl::one<'{'>> {};
struct table_end : pegtl::one<'}'> {};
template <typename table_begin>
struct table_apply : pegtl::if_must<table_begin, binds<table_end>> {};
struct table : pegtl::sor<table_apply<table_begin_recursive>,
                          table_apply<table_begin_nonrecursive>> {};

// XXX shortcut here?
struct bind_eq_operator : padr<pegtl::one<'='>> {
    using ast = ast::binding_eq;
    using next = pegtl::seq<expression<>, padr<pegtl::one<';'>>>;
};
struct bind_eq
    : pegtl::seq<padr<attrpath>, binary_expr_apply<bind_eq_operator>> {};
struct bind_inherit_attrname : attrpath {};
struct bind_inherit_from : pegtl::if_must<padr<pegtl::one<'('>>,
                                          expression<table>,
                                          padr<pegtl::one<')'>>> {};
struct bind_inherit_apply : pegtl::seq<pegtl::opt<bind_inherit_from>,
                                       pegtl::star<padr<bind_inherit_attrname>>,
                                       padr<pegtl::one<';'>>> {};
struct bind_inherit : pegtl::if_must<keyword::key_inherit, bind_inherit_apply> {
};
struct bind : pegtl::sor<bind_eq, bind_inherit> {};

struct array_content_apply;

struct array_begin : padr<pegtl::one<'['>> {};
struct array_content : pegtl::until<pegtl::one<']'>, array_content_apply> {};
struct array : pegtl::if_must<array_begin, array_content> {};

struct dollarcurly_expr : pegtl::if_must<padr<pegtl::string<'$', '{'>>,
                                         expression<string>,
                                         pegtl::one<'}'>> {};
struct bracket_expr
    : pegtl::if_must<padr<pegtl::one<'('>>, expression<>, pegtl::one<')'>> {};

struct variable_tail;

struct expr_select
    : pegtl::seq<
          pegtl::sor<table, bracket_expr, name>,
          seps,
          pegtl::star<pegtl::seq<padr<pegtl::one<'.'>>, variable_tail>>> {};
struct expr_simple : pegtl::sor<boolean,
                                number,
                                string,
                                array,
                                dollarcurly_expr,
                                spath,
                                path,
                                uri> {};
struct expr_applying_tail : pegtl::sor<padr<expr_simple>, expr_select> {};

struct variable_tail_or_apply : expr_applying_tail {};
struct variable_tail_or
    : pegtl::if_must<keyword::key_or, variable_tail_or_apply> {};
struct variable_tail : pegtl::seq<pegtl::sor<name, string, dollarcurly_expr>,
                                  seps,
                                  pegtl::opt<variable_tail_or>> {};

// XXX reorder, this should be a few lines above near other array stuff
struct array_content_apply : expr_applying_tail {};
struct expr_applying
    : pegtl::seq<expr_select,
                 pegtl::star<pegtl::not_at<pegtl::one<';', ','>>,
                             expr_applying_tail>> {};
// XXX: reactivate CTX?
template <typename CTX = void>
struct expr_apply
    : pegtl::if_then_else<padr<expr_simple>, pegtl::success, expr_applying> {};

struct operator_negate_double : op_two<'-', '-', '>'> {};
struct operator_negate : op_one<'-', '>'> {};
struct expr_negate_val : expr_apply<number> {};
template <typename CTX>
struct expr_negate
    : pegtl::seq<
          pegtl::star<operator_negate_double>,
          pegtl::if_must_else<operator_negate, expr_negate_val, expr_apply<>>> {
};
// template<> struct expr_negate<number> : pegtl::seq<pegtl::star<op_one<'-',
// '>'>>, expr_apply<number>> {};

struct operator_attrtest : padr<pegtl::one<'?'>> {
    using ast = ast::attrtest;
    using next = attrpath;
};
struct expr_attrtest
    : pegtl::seq<expr_negate<void>,
                 pegtl::opt<binary_expr_apply<operator_attrtest>>> {};


// XXX right assoc?
struct operator_concat : padr<pegtl::two<'+'>> {
    using ast = ast::concat;
    using next = expr_apply<array>;
};
struct expr_concat
    : pegtl::seq<expr_attrtest,
                 pegtl::star<binary_expr_apply<operator_concat>>> {};

struct operator_div : op_one<'/', '/'> {
    using ast = ast::div;
    using next = expr_negate<number>;
};
struct expr_div
    : pegtl::seq<expr_concat, pegtl::star<binary_expr_apply<operator_div>>> {};

struct operator_mul : padr<pegtl::one<'*'>> {
    using ast = ast::mul;
    using next = expr_div;
};
struct expr_mul
    : pegtl::seq<expr_div, pegtl::star<binary_expr_apply<operator_mul>>> {};

struct operator_sub : op_one<'-', '>'> {
    using ast = ast::sub;
    using next = expr_mul;
};
struct expr_sub
    : pegtl::seq<expr_mul, pegtl::star<binary_expr_apply<operator_sub>>> {};


struct operator_add : padr<pegtl::one<'+'>> {
    using ast = ast::add;
    using next = expr_sub;
};
struct expr_add
    : pegtl::seq<expr_sub, pegtl::star<binary_expr_apply<operator_add>>> {};

struct operator_not : padr<pegtl::one<'!'>> {};
struct operator_not_double : pegtl::rep<2, operator_not> {};
struct expr_not_val : expr_attrtest {};
struct expr_not
    : pegtl::seq<pegtl::star<operator_not_double>,
                 pegtl::if_then_else<operator_not, expr_not_val, expr_add>> {};

// XXX: right assoc
struct operator_merge : padr<pegtl::two<'/'>> {
    using ast = ast::merge;
    using next = expr_apply<table>;
};
struct expr_merge
    : pegtl::seq<expr_not, pegtl::star<binary_expr_apply<operator_merge>>> {};


struct operators_ordering : padr<pegtl::sor<pegtl::string<'<', '='>,
                                            pegtl::string<'>', '='>,
                                            pegtl::one<'<', '>'>>> {};
template <typename CTX>
struct expr_ordering : left_assoc<expr_merge, operators_ordering, expr_add> {};

struct operator_eq : padr<pegtl::string<'=', '='>> {
    using ast = ast::eq;
    using next = expr_ordering<void>;
};
struct operator_neq : padr<pegtl::string<'!', '='>> {
    using ast = ast::neq;
    using next = expr_ordering<void>;
};
template <typename CTX>
struct expr_equality : pegtl::seq<expr_ordering<CTX>,
                                  pegtl::sor<binary_expr_apply<operator_eq>,
                                             binary_expr_apply<operator_neq>,
                                             pegtl::success>> {};

struct operator_and : padr<pegtl::two<'&'>> {
    using ast = ast::and_;
    using next = expr_equality<boolean>;
};
template <typename CTX>
struct expr_and : pegtl::seq<expr_equality<CTX>,
                             pegtl::star<binary_expr_apply<operator_and>>> {};


struct operator_or : padr<pegtl::two<'|'>> {
    using ast = ast::or_;
    using next = expr_and<boolean>;
};
template <typename CTX>
struct expr_or
    : pegtl::seq<expr_and<CTX>, pegtl::star<binary_expr_apply<operator_or>>> {};

struct operator_impl : padr<pegtl::string<'-', '>'>> {
    using ast = ast::impl;
    using next = expr_or<boolean>;
};
template <typename CTX>
struct expr_impl
    : pegtl::seq<expr_or<CTX>, pegtl::star<binary_expr_apply<operator_impl>>> {
};


template <typename CTX>
struct expr_if_apply : pegtl::seq<expression<boolean>,
                                  keyword::key_then,
                                  expression<CTX>,
                                  keyword::key_else,
                                  expression<CTX>> {};
template <typename CTX>
struct expr_if : pegtl::if_must<keyword::key_if, expr_if_apply<CTX>> {};

// TODO: allow importing uri
// struct expr_import : pegtl::if_must<keyword::key_import,
// padr<pegtl::sor<path, spath, string, name, expr_select>>,
// pegtl::opt<expr_applying<expr_applying_tail>>> {};

template <typename CTX> struct operator_assert : padr<pegtl::one<';'>> {
    using ast = ast::assertion;
    using next = expression<CTX>;
};
template <typename CTX>
struct assert : pegtl::if_must<keyword::key_assert,
                               expression<boolean>,
                               binary_expr_apply<operator_assert<CTX>>> {};


template <typename CTX> struct operator_with : padr<pegtl::one<';'>> {
    using ast = ast::with;
    using next = expression<CTX>;
};
// todo: restrict context?
template <typename CTX>
struct with : pegtl::if_must<keyword::key_with,
                             expression<>,
                             binary_expr_apply<operator_with<CTX>>> {};


template <typename CTX> struct operator_let : pegtl::success {
    using ast = ast::let;
    using next = expression<CTX>;
};

template <typename CTX>
struct let : pegtl::if_must<keyword::key_let,
                            binds<keyword::key_in>,
                            binary_expr_apply<operator_let<CTX>>> {};

template <typename CTX> struct function;

template <typename CTX>
struct statement : pegtl::sor<assert<CTX>, with<CTX>, let<CTX>, function<CTX>> {
}; // seq<.., /*expr_import,*/>
   //    struct statement_list : pegtl::star<statement> {};

template <>
struct expression<void>
    : pegtl::sor<statement<void>, expr_if<void>, expr_impl<void>> {};
template <>
struct expression<boolean>
    : pegtl::sor<statement<boolean>, expr_if<boolean>, expr_impl<boolean>> {};
template <>
struct expression<string>
    : pegtl::sor<statement<string>, expr_if<string>, expr_add> {};
template <>
struct expression<table>
    : pegtl::sor<statement<table>, expr_if<table>, expr_merge> {};


template <typename CTX> struct operator_function : pegtl::success {
    using ast = ast::function;
    using next = expression<CTX>;
};
template <typename CTX>
struct function
    : pegtl::if_must<arguments, binary_expr_apply<operator_function<CTX>>> {};

struct grammar : pegtl::must<seps, expression<>, pegtl::eof> {};

struct control {
    template <typename Rule> struct normal : pegtl::normal<Rule> {};
};

template <typename Rule> struct action : pegtl::nothing<Rule> {};

namespace actions {
template <typename Rule> struct binds : action<Rule> {};

template <> struct binds<bind_inherit_attrname> {
    template <typename Input>
    static void apply(const Input& in, state::binding_inherit& state) {
        state.push_back();
    }
};

template <> struct binds<bind_inherit_from> {
    template <typename Input>
    static void apply(const Input& in, state::binding_inherit& state) {
        state.set_from();
    }
};

template <> struct binds<bind> {
    template <typename Input>
    static void apply(const Input& in, state::binds& state) {
        state.push_back();
    }
};

template <typename Rule> struct array : action<Rule> {};

template <> struct array<array_content_apply> {
    template <typename Input>
    static void apply(const Input& in, state::array& state) {
        state.push_back();
    }
};

template <typename Rules> struct formals : action<Rules> {};

template <> struct formals<keyword::key_ellipsis> {
    template <typename Input>
    static void apply(const Input& in, state::formals& state) {
        state.value = std::make_unique<ast::ellipsis>();
        state.push_back();
    }
};

template <> struct formals<formal> {
    template <typename Input>
    static void apply(const Input& in, state::formals& state) {
        state.push_back();
    }
};

template <typename Rules> struct if_then_else : action<Rules> {};

template <> struct if_then_else<keyword::key_then> {
    template <typename Input>
    static void apply(const Input& in, state::if_then_else& state) {
        state.set_test();
    }
};

template <> struct if_then_else<keyword::key_else> {
    template <typename Input>
    static void apply(const Input& in, state::if_then_else& state) {
        state.set_then();
    }
};

template <typename Rules> struct string : pegtl::nothing<Rules> {};

// template<> struct string<short_string_escaped> :
// pegtl::unescape::unescape_c<short_string_escaped,
template <>
struct string<short_string_escaped>
    : pegtl::unescape::unescape_c<short_string_escaped, '"', '$', '\\', 'n'> {};
template <>
struct string<short_string_accepted> : pegtl::unescape::append_all {};

template <> struct string<short_string_content> {
    template <typename Input>
    static void apply(const Input& in, state::string& state) {
        state.push_back();
    }
};

template <> struct string<dollarcurly_expr> {
    template <typename Input>
    static void apply(const Input& in, state::any_string& state) {
        state.push_back_expr();
    }
};

template <>
struct string<long_string_escape_accepted> : pegtl::unescape::append_all {};
template <>
struct string<long_string_accepted> : pegtl::unescape::append_all {};

template <> struct string<long_string_content> {
    template <typename Input>
    static void apply(const Input& in, state::long_string& state) {
        state.push_back();
    }
};
} // namespace actions

template <typename x>
struct control::normal<expression<x>>
    : pegtl::change_state_and_action<expression<x>,
                                     state::base,
                                     action,
                                     pegtl::normal> {};

template <>
struct control::normal<variable_tail>
    : pegtl::change_state<variable_tail,
                          state::binary_expression<ast::attrpath>,
                          pegtl::normal> {};
template <>
struct control::normal<array_content>
    : pegtl::change_state_and_action<array_content,
                                     state::array,
                                     actions::array,
                                     pegtl::normal> {};
template <typename x>
struct control::normal<binary_expr_apply<x>>
    : pegtl::change_state_and_action<binary_expr_apply<x>,
                                     state::binary_expression<typename x::ast>,
                                     action,
                                     pegtl::normal> {};

template <>
struct control::normal<bind_inherit_apply>
    : pegtl::change_state<bind_inherit_apply,
                          state::binding_inherit,
                          pegtl::normal> {};
template <>
struct control::normal<expr_applying_tail>
    : pegtl::change_state<expr_applying_tail,
                          state::binary_expression<ast::call>,
                          pegtl::normal> {};
template <>
struct control::normal<variable_tail_or_apply>
    : pegtl::change_state<variable_tail_or_apply,
                          state::binary_expression<ast::set_or>,
                          pegtl::normal> {};

template <>
struct control::normal<formals>
    : pegtl::change_state_and_action<formals,
                                     state::formals,
                                     actions::formals,
                                     pegtl::normal> {};
template <typename x>
struct control::normal<binds<x>>
    : pegtl::change_state_and_action<binds<x>,
                                     state::binds,
                                     actions::binds,
                                     pegtl::normal> {};
template <typename x>
struct control::normal<expr_if_apply<x>>
    : pegtl::change_state_and_action<expr_if_apply<x>,
                                     state::if_then_else,
                                     actions::if_then_else,
                                     pegtl::normal> {};
template <>
struct control::normal<short_string_content>
    : pegtl::change_state_and_action<short_string_content,
                                     state::string,
                                     actions::string,
                                     pegtl::normal> {};
template <>
struct control::normal<long_string_content>
    : pegtl::change_state_and_action<long_string_content,
                                     state::long_string,
                                     actions::string,
                                     pegtl::normal> {};
//    template<typename x> struct control::normal<backtrack<x>> :
//    pegtl::normal<backtrack<x>> {};

template <typename Rule> struct errors : pegtl::normal<Rule> {
    static const std::string error_message;
    template <typename Input, typename... States>
    static void raise(const Input& in, States&&...) {
        throw pegtl::parse_error(error_message, in);
    }
};

template <> struct action<do_backtrack> {
    template <typename Input>
    static void apply(const Input& in, state::base& state) {
        // XXX assert state.value to find superfluous backtrackings
        state.value.reset();
    }
};

template <> struct action<number> {
    template <typename Input>
    static void apply(const Input& in, state::base& state) {
        assert(!state.value);
        state.value = std::make_unique<ast::number>(std::stoll(in.string()));
    }
};

template <> struct action<name> {
    template <typename Input>
    static void apply(const Input& in, state::base& state) {
        assert(!state.value);
        state.value = std::make_unique<ast::name>(in.string());
    }
};

template <> struct action<path> {
    template <typename Input>
    static void apply(const Input& in, state::base& state) {
        assert(!state.value);
        state.value = std::make_unique<ast::path>(in.string());
    }
};

template <> struct action<keyword::key_true> {
    template <typename Input>
    static void apply(const Input& in, state::base& state) {
        assert(!state.value);
        state.value = std::make_unique<ast::boolean>(true);
    }
};

template <> struct action<keyword::key_false> {
    template <typename Input>
    static void apply(const Input& in, state::base& state) {
        assert(!state.value);
        state.value = std::make_unique<ast::boolean>(false);
    }
};

template <> struct action<expr_not_val> {
    template <typename Input>
    static void apply(const Input& in, state::base& state) {
        assert(state.value);
        state.value = std::make_unique<ast::not_>(std::move(state.value));
    }
};

template <> struct action<expr_negate_val> {
    template <typename Input>
    static void apply(const Input& in, state::base& state) {
        assert(state.value);
        state.value = std::make_unique<ast::negate>(std::move(state.value));
    }
};

template <> struct action<table_apply<table_begin_nonrecursive>> {
    template <typename Input>
    static void apply(const Input& in, state::base& state) {
        assert(state.value);
        state.value =
            std::make_unique<ast::table>(std::move(state.value), false);
    }
};

template <> struct action<table_apply<table_begin_recursive>> {
    template <typename Input>
    static void apply(const Input& in, state::base& state) {
        assert(state.value);
        state.value =
            std::make_unique<ast::table>(std::move(state.value), true);
    }
};


//    template<> const std::string errors<expr_negate<number>>::error_message =
//    "incomplete negate expression, expected number";
//    template<> const std::string errors<expr_sum<number>>::error_message =
//    "incomplete sum expression, expected number";


bool parse_file(const std::string& filename, nix::parser::state::base& res);

bool parse_string(const std::string& data, nix::parser::state::base& res);

} // namespace parser

} // namespace nix
