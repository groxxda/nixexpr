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

    struct string : public base {
        explicit string(std::string in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << "\"" << data << "\""; }
        std::string data;
    };

    struct name : public base {
        explicit name(std::string in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << data; }
        std::string data;
    };

    struct boolean : public base {
        explicit boolean(bool in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << data; }
        bool data;
    };

    struct not_ : public base {
        explicit not_(std::shared_ptr<base> in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << "!(" << data << ")"; }
        std::shared_ptr<base> data;
    };

    struct negate : public base {
        explicit negate(std::shared_ptr<base> in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << "(-" << data << ")"; }
        std::shared_ptr<base> data;
    };

    struct inverse : public base {
        explicit inverse(std::shared_ptr<base> in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << "(1/" << data << ")"; }
        std::shared_ptr<base> data;
    };

    struct sum : public base {
        explicit sum() : base() {};
        virtual void stream(std::ostream& o) const override {
            o << "(";
            auto iter = data.cbegin();
            o << *iter;
            while (++iter != data.cend()) o << "+" << *iter;
            o << ")";
        }
        std::vector<std::shared_ptr<base>> data;
    };

    struct product : public base {
        explicit product() : base() {};
        virtual void stream(std::ostream& o) const override {
            o << "(";
            auto iter = data.cbegin();
            o << *iter;
            while (++iter != data.cend()) o << "*" << *iter;
            o << ")";
        }
        std::vector<std::shared_ptr<base>> data;
    };

    struct table : public base {
        explicit table(bool recursive) : base(), recursive(recursive) {};
        virtual void stream(std::ostream& o) const override {
            if (recursive) o << "rec ";
            o << "{ ";
            for (auto iter = data.cbegin(); iter != data.cend(); ++iter)
                o << *(iter->first) << " = " << iter->second << "; ";
            o << "}";
        }
        bool recursive;
        std::vector<std::pair<std::shared_ptr<ast::name>, std::shared_ptr<ast::base>>> data;
    };

} // namespace ast


namespace parser {

namespace state {
    struct base {
        base() = default;
        base(const base&) = delete;
        void operator=(const base&) = delete;
        std::shared_ptr<ast::base> value;
    };

    struct expression : base {
        std::shared_ptr<ast::base> result;
        void success(base& in_result) {
            assert(!in_result.value);
            assert(result);
            assert(!value);
//XXX: unwrap trivial expressions
            //if (auto is_name = std::dynamic_pointer_cast<ast::name>(result))
            //    result = std::move(is_name
            in_result.value = std::move(result);
        }
    };

    struct sum : expression {
        std::shared_ptr<ast::sum> sum = std::make_shared<ast::sum>();
        bool next_neg = false;
        void push_back() {
            assert(value);
            if (next_neg) {
//                auto is_negated = std::dynamic_pointer_cast<ast::negate>(value);
//                if (is_negated)
//                    value = std::move(is_negated->data);
//                else
                    value = std::make_shared<ast::negate>(std::move(value));
                next_neg = false;
            }
            sum->data.push_back(std::move(value));
            value.reset();
        }
        void success(expression& in_result) {
            assert(in_result.value);
            sum->data.insert(sum->data.begin(), std::move(in_result.value));
            assert(!this->value);
            in_result.value = sum;
        }
    };

    struct product : expression {
        std::shared_ptr<ast::product> product = std::make_shared<ast::product>();
        bool next_inverse = false;
        void push_back() {
            assert(value);
            if (next_inverse) {
//                auto is_inverse = std::dynamic_pointer_cast<ast::inverse>(value);
//                if (is_inverse)
//                    value = std::move(is_inverse->data);
//                else
                    value = std::make_shared<ast::inverse>(std::move(value));
                next_inverse = false;
            }
            product->data.push_back(std::move(value));
            value.reset();
        }
        void success(expression& in_result) {
            assert(in_result.value);
            product->data.insert(product->data.begin(), std::move(in_result.value));
            assert(!this->value);
            in_result.value = product;
        }
    };

    struct table : base {
        std::vector<std::pair<std::shared_ptr<ast::name>, std::shared_ptr<ast::base>>> data;
        std::shared_ptr<ast::name> key;
        std::shared_ptr<ast::name> from;

        void push_back() {
            assert(key);
            assert(value);
            if (from) {
                if (auto is_name = std::dynamic_pointer_cast<ast::name>(value))
                    value = std::make_shared<ast::name>(from->data + "." + is_name->data);
                else
                    assert(false);
            }
            data.emplace_back(std::move(key), std::move(value));
            key.reset();
            value.reset();
        }

        void success(expression& in_result) {
            assert(in_result.value);
            auto is_table = std::dynamic_pointer_cast<ast::table>(in_result.value);
            assert(is_table);
            is_table->data = std::move(data);
            assert(!this->value);
        }
    };
} // namespace state

struct identifier_first : pegtl::identifier_first {};
struct identifier_other : pegtl::sor<pegtl::identifier_other, pegtl::one<'\'', '-'>> {};
struct identifier : pegtl::seq< identifier_first, pegtl::star< identifier_other > > {};

struct line_comment : pegtl::disable<pegtl::one<'#'>, pegtl::until<pegtl::eolf>> {};
struct long_comment : pegtl::disable<pegtl::string<'/', '*'>, pegtl::until<pegtl::string<'*', '/'>>> {};
struct comment : pegtl::sor<line_comment, long_comment> {};

struct string;

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

template<typename S, typename O, typename P = S>
struct left_assoc : pegtl::seq<S, pegtl::star<pegtl::if_must<O, P>>> {};
template<typename S, typename O, typename P = S>
struct right_assoc :  pegtl::seq< S, pegtl::opt< pegtl::if_must< O, right_assoc< P, O > > > > {};
template<typename S, typename O, typename P = S>
struct non_assoc : pegtl::seq<S, pegtl::opt<pegtl::if_must<O, P>>> {};
template< char O, char ... N >
struct op_one : pegtl::seq< pegtl::one< O >, pegtl::if_then_else<pegtl::plus<sep>, pegtl::success, pegtl::at< pegtl::not_one< N ... > >> > {};
template<char O, char P, char... N>
struct op_two : pegtl::seq<padr<pegtl::one<O>>, pegtl::one<P>, pegtl::if_then_else<pegtl::plus<sep>, pegtl::success, pegtl::at<pegtl::not_one<N...>>>> {};

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

    struct attr : pegtl::sor<name, string, dollarcurly_expr> {};
    struct attrtail : pegtl::sor<name, string, dollarcurly_expr> {};

    struct attrpath : pegtl::list_must<padr<attrtail>, padr<pegtl::one<'.'>>> {};

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


    struct bind;

    template<typename U>
    struct binds : pegtl::until<U, bind> {};

    struct table_begin_recursive : pegtl::seq<keyword::key_rec, padr<pegtl::one<'{'>>> {};
    struct table_begin_nonrecursive : padr<pegtl::one<'{'>> {};
    struct table_begin : pegtl::sor<table_begin_recursive, table_begin_nonrecursive> {};
    struct table_end : pegtl::one<'}'> {};
    struct table : pegtl::if_must<table_begin, binds<table_end>> {};

    struct bind_eq_attrpath : attrpath {};
    struct bind_eq_operator : padr<pegtl::one<'='>> {};
    struct bind_eq : pegtl::seq<padr<attr>, pegtl::if_must<bind_eq_operator, expression<>>> {};
    struct bind_inherit_attrname : attr {};
    struct bind_inherit_from : pegtl::if_must<padr<pegtl::one<'('>>, expression<table>, padr<pegtl::one<')'>>> {};
    struct bind_inherit : pegtl::if_must<keyword::key_inherit, pegtl::opt<bind_inherit_from>, pegtl::star<padr<bind_inherit_attrname>>> {};
    struct bind : pegtl::seq<pegtl::sor<bind_eq, bind_inherit>, padr<pegtl::one<';'>>> {};




    struct expr_applying_tail;
    struct array_constructor : pegtl::if_must<padr<pegtl::one<'['>>, pegtl::until<pegtl::one<']'>, expr_applying_tail>> {};

    struct dollarcurly_expr : pegtl::if_must<padr<pegtl::string<'$', '{'>>, expression<string>, pegtl::one<'}'>> {};
    struct bracket_expr : pegtl::if_must<padr<pegtl::one<'('>>, expression<>, pegtl::one<')'>> {};




    struct variable_tail_or : pegtl::if_must<keyword::key_or, expr_applying_tail> {};
    struct variable_tail : pegtl::seq<pegtl::one<'.'>, seps, pegtl::sor<name, string, dollarcurly_expr>, seps, pegtl::opt<variable_tail_or>> {};

    struct expr_select : pegtl::seq<pegtl::sor<table, bracket_expr, name>, seps, pegtl::star<variable_tail>> {};
    struct expr_simple : pegtl::sor<boolean, number, string, array_constructor, dollarcurly_expr, spath, path, uri> {};
    struct expr_applying_tail : pegtl::sor<padr<expr_simple>, expr_select> {};
    struct expr_applying : pegtl::seq<expr_select, pegtl::star<pegtl::not_at<pegtl::one<';', ','>>, expr_applying_tail>> {};
// XXX: reactivate CTX?
    template<typename CTX = void>
    struct expr_apply : pegtl::if_then_else<padr<expr_simple>, pegtl::success, expr_applying> {};

    struct operator_negate_double : op_two<'-', '-', '>'> {};
    struct operator_negate : op_one<'-', '>'> {};
    struct expr_negate_val : expr_apply<number> {};
    template<typename CTX>
    struct expr_negate : pegtl::seq<pegtl::star<operator_negate_double>, pegtl::if_must_else<operator_negate, expr_negate_val, expr_apply<>>> {};
    //template<> struct expr_negate<number> : pegtl::seq<pegtl::star<op_one<'-', '>'>>, expr_apply<number>> {};

    struct expr_attrtest : pegtl::seq<expr_negate<void>, pegtl::opt<pegtl::if_must<pegtl::one<'?'>, seps, attrpath>>> {};


    template<typename CTX>
    struct expr_arrayconcat : right_assoc<expr_attrtest, padr<pegtl::two<'+'>>, expr_apply<array_constructor>> {};
    template<> struct expr_arrayconcat<array_constructor> : right_assoc<expr_apply<array_constructor>, padr<pegtl::two<'+'>>> {};

    struct operators_product_mul : padr<pegtl::one<'*'>> {};
    struct operators_product_div : op_one<'/', '/'> {};
    struct operators_product : pegtl::sor<operators_product_mul, operators_product_div> {};
    struct expr_product_val : expr_negate<number>{};
    struct expr_product_apply : pegtl::plus<pegtl::if_must<operators_product, expr_product_val>> {};
    template<typename CTX>
    struct expr_product : pegtl::seq<expr_arrayconcat<CTX>, pegtl::opt<expr_product_apply>> {};
    //template<> struct expr_product<number> : left_assoc<expr_negate<number>, operators_product> {};

    struct operators_sum_plus : padr<pegtl::one<'+'>> {};
    struct operators_sum_minus : op_one<'-', '>'> {};
    struct operators_sum : pegtl::sor<operators_sum_plus, operators_sum_minus> {};
    template<typename CTX>
    struct expr_sum_val : expr_product<CTX> {};
    template<typename CTX>
    struct expr_sum_apply : pegtl::plus<pegtl::if_must<operators_sum, expr_sum_val<CTX>>> {};
    template<typename CTX>
    struct expr_sum : pegtl::seq<expr_product<CTX>, pegtl::opt<expr_sum_apply<CTX>>> {};
    //template<> struct expr_sum<string> : left_assoc<expr_apply<string>, operators_sum_plus, expr_apply<string>> {};

    struct operator_not : padr<pegtl::one<'!'>> {};
    struct operator_not_double : pegtl::rep<2, operator_not> {};
    struct expr_not_val : expr_attrtest {};
    struct expr_not : pegtl::seq<pegtl::star<operator_not_double>, pegtl::if_then_else<operator_not, expr_not_val, expr_sum<void>>> {};

    template<typename CTX>
    struct expr_setplus : right_assoc<expr_not, padr<pegtl::two<'/'>>, expr_apply<table>> {};
    template<> struct expr_setplus<table> : right_assoc<expr_apply<table>, padr<pegtl::two<'/'>>> {};

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
    struct let : pegtl::if_must<keyword::key_let, binds<keyword::key_in>> {};

    struct statement : pegtl::sor<assert, with, let, arguments> {}; // seq<.., /*expr_import,*/>
    struct statement_list : pegtl::star<statement> {};

    template<> struct expression<void> : pegtl::seq<statement_list, pegtl::sor<expr_if<void>, expr_impl<void>>> {};
    template<> struct expression<boolean> : pegtl::seq<statement_list, pegtl::sor<expr_if<boolean>, expr_impl<boolean>>> {};
    template<> struct expression<string> : pegtl::seq<statement_list, pegtl::sor<expr_if<string>, expr_sum<string>>> {};
    template<> struct expression<table> : pegtl::seq<statement_list, pegtl::sor<expr_if<table>, expr_setplus<table>>> {};


    struct grammar : pegtl::must<seps, expression<>, pegtl::eof> {};

    struct control {
        template<typename Rule>
        struct normal : pegtl::normal<Rule> {};
    };



    template<typename Rule>
    struct action : pegtl::nothing<Rule> {};


    template<> struct action<expr_negate_val> {
        template<typename Input>
        static void apply(const Input& in, state::expression& state) {
            assert(state.value);
            state.value = std::make_shared<ast::negate>(std::move(state.value));
        }
    };


    namespace actions {
        template<typename Rule>
        struct table : action<Rule> {};

// XXX: also string and dollar curly
//        template<> struct table<name> {
//            template<typename Input> static void apply(const Input& in, state::table& state) {
//                state.key = std::make_shared<ast::name>(in.string());
//            }
//        };

        template<> struct table<bind_eq_operator> {
            template<typename Input> static void apply(const Input& in, state::table& state) {
                assert(!state.key);
                assert(state.value);
                if (auto is_name = std::dynamic_pointer_cast<ast::name>(state.value))
                    state.key = std::move(is_name);

                assert(state.key);
                state.value.reset();
            }
        };

        template<> struct table<bind_inherit_attrname> {
            template<typename Input> static void apply(const Input& in, state::table& state) {
                assert(!state.key);
                assert(state.value);
                if (auto is_name = std::dynamic_pointer_cast<ast::name>(state.value))
                    state.key = std::move(is_name);

                assert(state.key);
                state.push_back();
            }
        };

        template<> struct table<bind_inherit_from> {
            template<typename Input> static void apply(const Input& in, state::table& state) {
                assert(state.value);
                if (auto is_name = std::dynamic_pointer_cast<ast::name>(state.value))
                    state.from = std::move(is_name);

                assert(state.from);
            }
        };

        template<> struct table<keyword::key_inherit> {
            template<typename Input> static void apply(const Input& in, state::table& state) {
                state.value.reset();
            }
        };

        template<> struct table<bind_eq> {
            template<typename Input> static void apply(const Input& in, state::table& state) {
                state.push_back();
            }
        };

        template<> struct table<bind_inherit> {
            template<typename Input> static void apply(const Input& in, state::table& state) {
                state.from.reset();
            }
        };




        template<typename Rule>
        struct sum : action<Rule> {};

        template<> struct sum<expr_negate_val> {
            template<typename Input>
            static void apply(const Input& in, state::sum& state) {
                assert(state.value);
                state.next_neg = !state.next_neg;
            }
        };

        template<> struct sum<operators_sum_minus> {
            template<typename Input>
            static void apply(const Input& in, state::sum& state) {
                state.next_neg = true;
            }
        };

        template<typename x> struct sum<expr_sum_val<x>> {
            template<typename Input>
            static void apply(const Input& in, state::sum& state) {
                state.push_back();
            }
        };


        template<typename Rule>
        struct product : action<Rule> {};

        template<> struct product<operators_product_div> {
            template<typename Input>
            static void apply(const Input& in, state::product& state) {
                state.next_inverse = true;
            }
        };

        template<> struct product<expr_product_val> {
            template<typename Input>
            static void apply(const Input& in, state::product& state) {
                state.push_back();
            }
        };
    } // namespace actions

    //template<> struct control::normal<grammar> : pegtl::tracer<grammar> {};
    //template<> struct control::normal<grammar> : pegtl::tracer<grammar> {};
    //template<> struct control::normal<grammar> : pegtl::tracer<grammar> {};



    template<typename x> struct control::normal<expression<x>> : pegtl::change_state_and_action<expression<x>, state::expression, action, pegtl::tracer> {};
    template<typename x> struct control::normal<expr_sum_apply<x>> : pegtl::change_state_and_action<expr_sum_apply<x>, state::sum, actions::sum, pegtl::normal> {};
    template<> struct control::normal<expr_product_apply> : pegtl::change_state_and_action<expr_product_apply, state::product, actions::product, pegtl::normal> {};
    template<typename x> struct control::normal<binds<x>> : pegtl::change_state_and_action<binds<x>, state::table, actions::table, pegtl::normal> {};
    //template<> struct control::normal<name> : pegtl::tracer<name> {};
    //template<typename x> struct control::normal<expression<x>> : pegtl::tracer<expression<x>> {};
//    template<typename x> struct control::normal<expr_negate<x>> : pegtl::tracer<expr_negate<x>> {};

    //template<typename x> struct control::normal<expr_apply<x>> : pegtl::tracer<expr_apply<x>> {};
    //template<> struct control::normal<short_string_content> : pegtl::normal<short_string_content> {};

    template<typename Rule>
    struct errors : pegtl::normal<Rule> {
        static const std::string error_message;
        template<typename Input, typename... States>
        static void raise(const Input& in, States&& ...) {
            throw pegtl::parse_error(error_message, in);
        }
    };

    template<> struct action<number> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            state.value = std::make_shared<ast::number>(std::stoll(in.string()));
        }
    };

    template<> struct action<name> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            state.value = std::make_shared<ast::name>(in.string());
        }
    };

    template<> struct action<short_string_content> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            //xxx: can we move here?
            state.value = std::make_shared<ast::string>(in.string());
        }
    };

    template<> struct action<long_string_content> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            //xxx: can we move here?
            state.value = std::make_shared<ast::string>(in.string());
        }
    };


    template<> struct action<keyword::key_true> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            state.value = std::make_shared<ast::boolean>(true);
        }
    };

    template<> struct action<keyword::key_false> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            assert(!state.value);
            state.value = std::make_shared<ast::boolean>(false);
        }
    };

    template<> struct action<expr_not_val> {
        template<typename Input>
        static void apply(const Input& in, state::expression& state) {
            auto is_boolean = std::dynamic_pointer_cast<ast::boolean>(state.value);
            if (is_boolean)
                is_boolean->data = !is_boolean->data;
            else
                state.value = std::make_shared<ast::not_>(std::move(state.value));
        }
    };

    template<> struct action<table_begin_recursive> {
        template<typename Input>
        static void apply(const Input& in, state::expression& state) {
            state.value = std::make_shared<ast::table>(true);
        }
    };

    template<> struct action<keyword::key_let> {
        template<typename Input>
        static void apply(const Input& in, state::expression& state) {
            state.value = std::make_shared<ast::table>(true);
        }
    };

    template<> struct action<table_begin_nonrecursive> {
        template<typename Input>
        static void apply(const Input& in, state::expression& state) {
            state.value = std::make_shared<ast::table>(false);
        }
    };

    template<typename x> struct action<expression<x>> {
        template<typename Input>
        static void apply(const Input& in, state::expression& state) {
            assert(state.value);
            assert(!state.result);
            state.result = std::move(state.value);
        }
    };



//    template<> const std::string errors<expr_negate<number>>::error_message = "incomplete negate expression, expected number";
//    template<> const std::string errors<expr_sum<number>>::error_message = "incomplete sum expression, expected number";
//
//
//    template<typename Rule> struct value_action : pegtl::nothing<Rule> {};
//
//    /*template<> struct value_action<string> {
//        template<typename Input>
//        static void apply(const Input& in, state::base& result) {
//            result.result = std::make_shared<ast::string>(in.string());
//        }
//    };*/
//

//
//    template<typename Rule> struct negate_action : pegtl::nothing<Rule> {};
//    template<> struct negate_action<operator_negate> {
//        template<typename Input>
//        static void apply(const Input& in, state::negate& result) {
//            result.negated = !result.negated;
//        }
//    };
//
//    template<typename Rule> struct sum_action : pegtl::nothing<Rule> {};
//    template<> struct sum_action<operators_sum_plus> {
//        template<typename Input>
//        static void apply(const Input& in, state::sum& result) {
//            result.push_back();
//        }
//    };
//    template<> struct sum_action<operators_sum_minus> {
//        template<typename Input>
//        static void apply(const Input& in, state::sum& result) {
//            result.push_back();
//        }
//    };

    //template<typename Rule> struct value_action : pegtl::nothing<Rule> {};


    /*template<> struct not_action<operator_not> {
        template<typename Input>
        static void apply(const Input& in, state::base& result) {
    //apply not if e is (bool, not)
            result.result = std::make_shared<ast::not_>(result.result);
        }
    };*/


//    template<typename x> struct control::normal<expr_negate<x>> : pegtl::change_state_and_action<expr_negate<x>, state::negate, negate_action, errors> {};
//
//    template<typename x> struct control::normal<expr_sum<x>> : pegtl::change_state_and_action<expr_sum<x>, state::sum, sum_action, errors> {};
//
//    //template<typename x> struct control::normal<expr_apply<x>> : pegtl::change_state<expr_apply<x>, pegtl::nothing, pegtl::normal> {};
//
//    template<> struct control::normal<number> : pegtl::change_action<number, value_action, errors> {};





} // namespace parser

} // namespace nix






