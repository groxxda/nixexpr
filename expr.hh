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
        virtual bool operator==(const base* o) const { return o && o->operator==(*this); }
        virtual bool operator==(const base& o) const {
            std::stringstream msg;
            msg << "don't know how to compare ";
            msg << typeid(*this).name();
            msg << ": ";
            stream(msg);
            msg << " and ";
            msg << typeid(o).name();
            msg << ": ";
            o.stream(msg);
            throw msg.str();
        }
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

    inline bool operator==(const std::shared_ptr<base>& a, const std::shared_ptr<base>& b) {
        return a->operator==(b.get());
    }

    struct number : public base {
        explicit number(const long long in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << data; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const number*>(o); return cast && data == cast->data; }
        unsigned long long data;
    };

    struct string : public base {
        explicit string(std::string in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << "\"" << data << "\""; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const string*>(o); return cast && data == cast->data; }
        std::string data;
    };

    struct name : public base {
        explicit name(std::string in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << data; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const name*>(o); return cast && data == cast->data; }
        std::string data;
    };

    struct boolean : public base {
        explicit boolean(bool in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << std::boolalpha << data; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const boolean*>(o); return cast && data == cast->data; }
        operator bool() { return data; }
        bool data;
    };

    template<char op>
    struct unary_expression : public base {
        explicit unary_expression(std::shared_ptr<base> value) : base(), value(value) {}
        virtual void stream(std::ostream& o) const override { o << op << value; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const unary_expression<op>*>(o); return cast && value == cast->value; }
        std::shared_ptr<base> value;
    };

    struct not_ : public unary_expression<'!'> { using unary_expression<'!'>::unary_expression; };

    struct negate : public unary_expression<'-'> { using unary_expression<'-'>::unary_expression; };

    template<char... op>
    struct binary_expression : public base {
        explicit binary_expression(std::shared_ptr<base> lhs, std::shared_ptr<base> rhs) : base(), lhs(lhs), rhs(rhs) {}
        virtual void stream(std::ostream& o) const override { o << "(" << lhs; (o << ... << op) << rhs << ")"; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const binary_expression<op...>*>(o); return cast && lhs == cast->lhs && rhs == cast->rhs; }
        std::shared_ptr<base> lhs;
        std::shared_ptr<base> rhs;
    };

    struct add : public binary_expression<'+'> { using binary_expression<'+'>::binary_expression; };

    struct sub : public binary_expression<'-'> { using binary_expression<'-'>::binary_expression; };

    struct mul : public binary_expression<'*'> { using binary_expression<'*'>::binary_expression; };

    struct div : public binary_expression<'/'> { using binary_expression<'/'>::binary_expression; };

    struct or_ : public binary_expression<'|', '|'> { using binary_expression<'|', '|'>::binary_expression; };

    struct and_ : public binary_expression<'&', '&'> { using binary_expression<'&', '&'>::binary_expression; };

    struct impl : public binary_expression<'-', '>'> { using binary_expression<'-', '>'>::binary_expression; };


    struct binds : virtual public base {
        explicit binds(bool recursive) : base(), recursive(recursive) {};
        virtual void stream(std::ostream& o) const override {
            for (auto iter = data.cbegin(); iter != data.cend(); ++iter)
                o << *(iter->first) << " = " << iter->second << "; ";
        }
        std::vector<std::pair<std::shared_ptr<ast::name>, std::shared_ptr<ast::base>>> data;
        bool recursive;
    };

    struct table : public binds {
        explicit table(bool recursive) : binds(recursive) {}
        virtual void stream(std::ostream& o) const override {
            if (recursive) o << "rec ";
            o << "{ ";
            binds::stream(o);
            o << "}";
        }
    };

    struct array : public base {
        explicit array() : base(), data() { };
        virtual void stream(std::ostream& o) const override {
            o << "[ ";
            for (const auto& i : data) o << *i << " ";
            o << "]";
        }
        std::vector<std::shared_ptr<ast::base>> data;
    };

    struct statement : virtual public base {
        explicit statement(const std::shared_ptr<ast::base> expr) : base(), expr(expr) {}
        virtual bool operator==(const statement* o) const { return expr == o->expr; }
        const std::shared_ptr<ast::base> expr;
    };

    struct let : public binds, public statement {
        explicit let(std::shared_ptr<ast::base> expr) : binds(true), statement(expr) {}
        virtual void stream(std::ostream& o) const override { o << "let "; binds::stream(o); o << "in "; }
    };

    struct function : public statement {
        explicit function(std::shared_ptr<ast::base> argument, std::shared_ptr<ast::base> expr) : statement(expr), argument(argument) {}
        virtual void stream(std::ostream& o) const override { o << argument << ": " << expr; }
        virtual bool operator==(const base* o) { auto cast = dynamic_cast<const function*>(o); return cast && argument == cast->argument && expr == cast->expr; }
        const std::shared_ptr<ast::base> argument;
    };

    struct assertion : public statement {
        explicit assertion(const std::shared_ptr<ast::base> what, const std::shared_ptr<ast::base> expr) : statement(expr), what(what) {}
        virtual void stream(std::ostream& o) const override { o << "assert " << what << "; " << expr; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const assertion*>(o); return cast && statement::operator==(cast) && what == cast->what; }
        const std::shared_ptr<ast::base> what;
    };

    struct with : public statement {
        explicit with(const std::shared_ptr<ast::base> what, const std::shared_ptr<ast::base> expr) : statement(expr), what(what) {}
        virtual void stream(std::ostream& o) const override { o << "with " << what << "; " << expr; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const with*>(o); return cast && statement::operator==(cast) && what == cast->what; }
        const std::shared_ptr<ast::base> what;
    };


} // namespace ast


namespace parser {

namespace state {
    struct base {
        base() = default;
        base(const base&) = delete;
        void operator=(const base&) = delete;
        std::shared_ptr<ast::base> value;
        void success(base& in_result) {
            assert(!in_result.value);
            assert(value);
            in_result.value = std::move(value);
        }
    };

    template<typename T>
    struct binary_expression : base {
        void start(base& in_result) {
            std::cout << "starting binary_expression " << this << std::endl;
        }
        void success(base& in_result) {
            assert(in_result.value);
            assert(value);
            in_result.value = std::make_shared<T>(in_result.value, value);
        }
    };


    struct binds : base {
        std::vector<std::pair<std::shared_ptr<ast::name>, std::shared_ptr<ast::base>>> data;
        std::shared_ptr<ast::name> key;
        std::shared_ptr<ast::base> from;

        void push_back() {
            assert(key);
            assert(value);
            if (from) {
// XXX: ^_^
                std::stringstream t;
                t << from;
                t << ".";
                t << value;
                value = std::make_shared<ast::name>(t.str());
            }
            data.emplace_back(std::move(key), std::move(value));
            key.reset();
            value.reset();
        }

        void success(base& in_result) {
            assert(!value);
            assert(in_result.value);
            auto is_table = std::dynamic_pointer_cast<ast::binds>(in_result.value);
            assert(is_table);
            is_table->data = std::move(data);
        }
    };

    struct array : base {
        std::shared_ptr<ast::array> data = std::make_shared<ast::array>();

        void push_back() {
            assert(value);
            data->data.push_back(std::move(value));
        };

        void success(base& in_result) {
            assert(!in_result.value);
            assert(!value);
            in_result.value = std::move(data);
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

    struct array_begin : padr<pegtl::one<'['>> {};
    struct array_content : pegtl::until<pegtl::one<']'>, expr_applying_tail> {};
    struct array : pegtl::if_must<array_begin, array_content> {};

    struct dollarcurly_expr : pegtl::if_must<padr<pegtl::string<'$', '{'>>, expression<string>, pegtl::one<'}'>> {};
    struct bracket_expr : pegtl::if_must<padr<pegtl::one<'('>>, expression<>, pegtl::one<')'>> {};




    struct variable_tail_or : pegtl::if_must<keyword::key_or, expr_applying_tail> {};
    struct variable_tail : pegtl::seq<pegtl::one<'.'>, seps, pegtl::sor<name, string, dollarcurly_expr>, seps, pegtl::opt<variable_tail_or>> {};

    struct expr_select : pegtl::seq<pegtl::sor<table, bracket_expr, name>, seps, pegtl::star<variable_tail>> {};
    struct expr_simple : pegtl::sor<boolean, number, string, array, dollarcurly_expr, spath, path, uri> {};
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
    struct expr_arrayconcat : right_assoc<expr_attrtest, padr<pegtl::two<'+'>>, expr_apply<array>> {};
    template<> struct expr_arrayconcat<array> : right_assoc<expr_apply<array>, padr<pegtl::two<'+'>>> {};


    struct operator_div : op_one<'/', '/'> {};
    struct expr_div_apply : pegtl::if_must<operator_div, expr_negate<number>> {};
    struct expr_div : pegtl::seq<expr_arrayconcat<void>, pegtl::star<expr_div_apply>> {};


    struct operator_mul : padr<pegtl::one<'*'>> {};
    struct expr_mul_apply : pegtl::if_must<operator_mul, expr_div> {};
    struct expr_mul : pegtl::seq<expr_div, pegtl::star<expr_mul_apply>> {};


    struct operator_sub : op_one<'-', '>'> {};
    struct expr_sub_apply : pegtl::if_must<operator_sub, expr_mul> {};
    struct expr_sub : pegtl::seq<expr_mul, pegtl::star<expr_sub_apply>> {};


    struct operator_add : padr<pegtl::one<'+'>> {};
    struct expr_add_apply : pegtl::if_must<operator_add, expr_sub> {};
    struct expr_add : pegtl::seq<expr_sub, pegtl::star<expr_add_apply>> {};


    struct operator_not : padr<pegtl::one<'!'>> {};
    struct operator_not_double : pegtl::rep<2, operator_not> {};
    struct expr_not_val : expr_attrtest {};
    struct expr_not : pegtl::seq<pegtl::star<operator_not_double>, pegtl::if_then_else<operator_not, expr_not_val, expr_add>> {};

    template<typename CTX>
    struct expr_setplus : right_assoc<expr_not, padr<pegtl::two<'/'>>, expr_apply<table>> {};
    template<> struct expr_setplus<table> : right_assoc<expr_apply<table>, padr<pegtl::two<'/'>>> {};

    struct operators_ordering : padr<pegtl::sor<pegtl::string<'<', '='>, pegtl::string<'>', '='>, pegtl::one<'<', '>'>>> {};
    template<typename CTX>
    struct expr_ordering : left_assoc<expr_setplus<CTX>, operators_ordering, expr_add> {};
    //template<> struct expr_ordering<number> : left_assoc<expr_sum<number>, operators_ordering> {};

    struct operators_equality : padr<pegtl::sor<pegtl::two<'='>, pegtl::string<'!', '='>>> {};
// todo: we cannot do anything here, right?
    template<typename CTX>
    struct expr_equality : non_assoc<expr_ordering<CTX>, operators_equality> {};


    struct operator_and : padr<pegtl::two<'&'>> {};
    struct expr_and_apply : pegtl::if_must<operator_and, expr_equality<boolean>> {};
    template<typename CTX>
    struct expr_and : pegtl::seq<expr_equality<CTX>, pegtl::star<expr_and_apply>> {};


    struct operator_or : padr<pegtl::two<'|'>> {};
    struct expr_or_apply : pegtl::if_must<operator_or, expr_and<boolean>> {};
    template<typename CTX>
    struct expr_or : pegtl::seq<expr_and<CTX>, pegtl::star<expr_or_apply>> {};


    struct operator_impl : padr<pegtl::string<'-', '>'>> {};
    struct expr_impl_apply : pegtl::if_must<operator_impl, expr_or<boolean>> {};
    template<typename CTX>
    struct expr_impl : pegtl::seq<expr_or<CTX>, pegtl::star<expr_impl_apply>> {};


    template<typename CTX>
    struct expr_if : pegtl::if_must<keyword::key_if, expression<boolean>, keyword::key_then, expression<CTX>, keyword::key_else, expression<CTX>> {};

    // TODO: allow importing uri
    //struct expr_import : pegtl::if_must<keyword::key_import, padr<pegtl::sor<path, spath, string, name, expr_select>>, pegtl::opt<expr_applying<expr_applying_tail>>> {};


    template<typename CTX>
    struct assert_apply : expression<CTX> {};
    template<typename CTX>
    struct assert : pegtl::if_must<keyword::key_assert, expression<boolean>, padr<pegtl::one<';'>>, assert_apply<CTX>> {};

// todo: restrict context?
    template<typename CTX>
    struct with_apply : expression<CTX> {};
    template<typename CTX>
    struct with : pegtl::if_must<keyword::key_with, expression<>, padr<pegtl::one<';'>>, with_apply<CTX>> {};
    template<typename CTX>
    struct let : pegtl::if_must<keyword::key_let, binds<keyword::key_in>, expression<CTX>> {};
    template<typename CTX>
    struct function : pegtl::if_must<arguments, expression<CTX>> {};

    template<typename CTX>
    struct statement : pegtl::sor<assert<CTX>, with<CTX>, let<CTX>, function<CTX>> {}; // seq<.., /*expr_import,*/>
//    struct statement_list : pegtl::star<statement> {};

    template<> struct expression<void> : pegtl::sor<statement<void>, expr_if<void>, expr_impl<void>> {};
    template<> struct expression<boolean> : pegtl::sor<statement<boolean>, expr_if<boolean>, expr_impl<boolean>> {};
    template<> struct expression<string> : pegtl::sor<statement<string>, expr_if<string>, expr_add> {};
    template<> struct expression<table> : pegtl::sor<statement<table>, expr_if<table>, expr_setplus<table>> {};


    struct grammar : pegtl::must<seps, expression<>, pegtl::eof> {};

    struct control {
        template<typename Rule>
        struct normal : pegtl::normal<Rule> {};
    };



    template<typename Rule>
    struct action : pegtl::nothing<Rule> {};


    namespace actions {
        template<typename Rule>
        struct binds : action<Rule> {};

        template<> struct binds<bind_eq_operator> {
            template<typename Input> static void apply(const Input& in, state::binds& state) {
                assert(!state.key);
                assert(state.value);
                if (auto is_name = std::dynamic_pointer_cast<ast::name>(state.value))
                    state.key = std::move(is_name);

                assert(state.key);
                state.value.reset();
            }
        };

        template<> struct binds<bind_inherit_attrname> {
            template<typename Input> static void apply(const Input& in, state::binds& state) {
                assert(!state.key);
                assert(state.value);
                if (auto is_name = std::dynamic_pointer_cast<ast::name>(state.value))
                    state.key = std::move(is_name);

                assert(state.key);
                state.push_back();
            }
        };

        template<> struct binds<bind_inherit_from> {
            template<typename Input> static void apply(const Input& in, state::binds& state) {
                assert(state.value);
                state.from = std::move(state.value);

                assert(state.from);
            }
        };

        template<> struct binds<keyword::key_inherit> {
            template<typename Input> static void apply(const Input& in, state::binds& state) {
                state.value.reset();
            }
        };

        template<> struct binds<bind_eq> {
            template<typename Input> static void apply(const Input& in, state::binds& state) {
                state.push_back();
            }
        };

        template<> struct binds<bind_inherit> {
            template<typename Input> static void apply(const Input& in, state::binds& state) {
                state.from.reset();
            }
        };


        template<typename Rule>
        struct array : action<Rule> {};

        template<> struct array<expr_applying_tail> {
            template<typename Input> static void apply(const Input& in, state::array& state) {
                state.push_back();
            }
        };


    } // namespace actions


    template<typename x> struct control::normal<expression<x>> : pegtl::change_state_and_action<expression<x>, state::base, action, pegtl::tracer> {};
    template<> struct control::normal<expr_add_apply> : pegtl::change_state<expr_add_apply, state::binary_expression<ast::add>, pegtl::normal> {};
    template<> struct control::normal<expr_sub_apply> : pegtl::change_state<expr_sub_apply, state::binary_expression<ast::sub>, pegtl::normal> {};
    template<> struct control::normal<expr_mul_apply> : pegtl::change_state<expr_mul_apply, state::binary_expression<ast::mul>, pegtl::normal> {};
    template<> struct control::normal<expr_div_apply> : pegtl::change_state<expr_div_apply, state::binary_expression<ast::div>, pegtl::normal> {};
    template<> struct control::normal<expr_or_apply> : pegtl::change_state<expr_or_apply, state::binary_expression<ast::or_>, pegtl::normal> {};
    template<> struct control::normal<expr_and_apply> : pegtl::change_state<expr_and_apply, state::binary_expression<ast::and_>, pegtl::normal> {};
    template<> struct control::normal<expr_impl_apply> : pegtl::change_state<expr_impl_apply, state::binary_expression<ast::impl>, pegtl::normal> {};
    template<> struct control::normal<array_content> : pegtl::change_state_and_action<array_content, state::array, actions::array, pegtl::normal> {};
    template<typename x> struct control::normal<binds<x>> : pegtl::change_state_and_action<binds<x>, state::binds, actions::binds, pegtl::normal> {};
    template<typename x> struct control::normal<assert_apply<x>> : pegtl::change_state<assert_apply<x>, state::binary_expression<ast::assertion>, pegtl::normal> {};
    template<typename x> struct control::normal<with_apply<x>> : pegtl::change_state<with_apply<x>, state::binary_expression<ast::with>, pegtl::normal> {};


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
        static void apply(const Input& in, state::base& state) {
            assert(state.value);
            state.value = std::make_shared<ast::not_>(std::move(state.value));
        }
    };

    template<> struct action<expr_negate_val> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            assert(state.value);
            state.value = std::make_shared<ast::negate>(std::move(state.value));
        }
    };

    template<> struct action<table_begin_recursive> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            state.value = std::make_shared<ast::table>(true);
        }
    };

//    template<> struct action<keyword::key_let> {
//        template<typename Input>
//        static void apply(const Input& in, state::base& state) {
//            state.value = std::make_shared<ast::let>();
//        }
//    };

    template<> struct action<table_begin_nonrecursive> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            state.value = std::make_shared<ast::table>(false);
        }
    };

    template<typename x> struct action<statement<x>> {
        template<typename Input> static void apply(const Input& in, state::base& state) {
            std::cout << "action<statement<x>>";
            //state.push_statement();
        }
    };


//    template<> const std::string errors<expr_negate<number>>::error_message = "incomplete negate expression, expected number";
//    template<> const std::string errors<expr_sum<number>>::error_message = "incomplete sum expression, expected number";


} // namespace parser

} // namespace nix






