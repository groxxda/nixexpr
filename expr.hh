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

    struct string_literal : public base {
        explicit string_literal(std::string in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << data; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const string_literal*>(o); return cast && data == cast->data; }
        std::string data;
    };

    struct short_string : public base {
        explicit short_string(const std::vector<std::shared_ptr<ast::base>> parts) : base(), parts(parts) {};
        virtual void stream(std::ostream& o) const override { o << "\""; for (const auto& i : parts) o << *i; o << "\""; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const short_string*>(o); return cast && parts == cast->parts; }
        const std::vector<std::shared_ptr<ast::base>> parts;
    };

    struct long_string : public base {
        explicit long_string(const std::vector<std::shared_ptr<ast::base>> parts, const unsigned int prefixlen) : base(), parts(parts), prefixlen(prefixlen) {}
        virtual void stream(std::ostream& o) const override { o << "''"; for (const auto& i : parts) o << *i; o << "''"; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const long_string*>(o); return cast && parts == cast->parts; }
        const std::vector<std::shared_ptr<ast::base>> parts;
        const unsigned int prefixlen;
    };

    struct name : public base {
        explicit name(std::string in_data) : base(), data(in_data) {};
        virtual void stream(std::ostream& o) const override { o << data; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const name*>(o); return cast && data == cast->data; }
        std::string data;
    };

    struct path : public base {
        explicit path(std::string in_data) : base(), data(in_data) {};
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

    struct ellipsis : public base {
        explicit ellipsis() : base() {};
        virtual void stream(std::ostream& o) const override { o << "..."; }
        virtual bool operator ==(const base* o) const override { return dynamic_cast<const ellipsis*>(o); }
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

    struct dollar_curly : public unary_expression<'$'> {
        using unary_expression<'$'>::unary_expression;
        virtual void stream(std::ostream& o) const override { o << "${" << value << "}"; }
    };

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

    struct concat : public binary_expression<'+', '+'> { using binary_expression<'+', '+'>::binary_expression; };

    struct merge : public binary_expression<'/', '/'> { using binary_expression<'/', '/'>::binary_expression; };

    struct attrtest : public binary_expression<'?'> { using binary_expression<'?'>::binary_expression; };

    struct attrpath : public binary_expression<'.'> { using binary_expression<'.'>::binary_expression; };

    struct set_or : public binary_expression<'o', 'r'> { using binary_expression<'o', 'r'>::binary_expression; };

    struct call : public binary_expression<' '> { using binary_expression<' '>::binary_expression; };


    struct binding_eq : public binary_expression<'='> {
        using binary_expression<'='>::binary_expression;
        virtual void stream(std::ostream& o) const override { o << lhs << " = " << rhs; }
    };

    struct binding_inherit : public base {
        explicit binding_inherit(const std::shared_ptr<ast::base> from, const std::vector<std::shared_ptr<ast::base>> attrs) : base(), from(from), attrs(attrs) {};
        virtual void stream(std::ostream& o) const override {
            o << "inherit";
            if (from) { o << " (" << from << ")"; }
            for (const auto& i : attrs) o << " " << *i;
        }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const binding_inherit*>(o); return cast && from == cast->from && attrs == cast->attrs; }
        const std::shared_ptr<ast::base> from;
        const std::vector<std::shared_ptr<ast::base>> attrs;
    };

    struct binds : public base {
        explicit binds(const std::vector<std::shared_ptr<ast::base>> data) : base(), data(data) {};
        virtual void stream(std::ostream& o) const override { for (const auto& i : data) o << i << "; "; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const binds*>(o); return cast && data == cast->data; }
        const std::vector<std::shared_ptr<ast::base>> data;
    };

    struct formal : public binary_expression<'?'> {
        using binary_expression<'?'>::binary_expression;
        virtual void stream(std::ostream& o) const override { o << lhs; if (rhs) o << " ? "  << rhs; }
    };

    struct formals : public base {
        explicit formals(const std::vector<std::shared_ptr<ast::base>> data) : base(), data(data) {}
        virtual void stream(std::ostream& o) const override { o << "{"; auto i = data.cbegin(); if (i != data.cend()) o << " " << *i++; while (i != data.cend()) o << ", " << *i++; o << " }"; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const formals*>(o); return cast && data == cast->data; }
        const std::vector<std::shared_ptr<ast::base>> data;
    };

    struct table : public base {
        explicit table(const std::shared_ptr<ast::base> data, bool recursive) : binds(data), recursive(recursive) {}
        virtual void stream(std::ostream& o) const override { if (recursive) o << "rec "; o << "{ " << binds << "}"; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const table*>(o); return cast && recursive == cast->recursive && binds == cast->binds; }
        const std::shared_ptr<ast::base> binds;
        const bool recursive;
    };

    struct array : public base {
        explicit array(const std::vector<std::shared_ptr<ast::base>> data) : base(), data(data) { };
        virtual void stream(std::ostream& o) const override { o << "[ "; for (const auto& i : data) o << *i << " "; o << "]"; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const array*>(o); return cast && data == cast->data; }
        const std::vector<std::shared_ptr<ast::base>> data;
    };

    struct let : public binary_expression<'l', 'e', 't'> {
        using binary_expression<'l', 'e', 't'>::binary_expression;
        virtual void stream(std::ostream& o) const override { o << "let " << lhs << "in " << rhs; }
    };

    struct function : public binary_expression<'f', 'u', 'n'> {
        using binary_expression<'f', 'u', 'n'>::binary_expression;
        virtual void stream(std::ostream& o) const override { o << lhs << ": " << rhs; }
    };

    struct assertion : public binary_expression<'a', 's', 's', 'e', 'r', 't'> {
        using binary_expression<'a', 's', 's', 'e', 'r', 't'>::binary_expression;
        virtual void stream(std::ostream& o) const override { o << "assert " << lhs << "; " << rhs; }
    };

    struct with : public binary_expression<'w', 'i', 't', 'h'> {
        using binary_expression<'w', 'i', 't', 'h'>::binary_expression;
        virtual void stream(std::ostream& o) const override { o << "with " << lhs << "; " << rhs; }
    };

    struct if_then_else : public base {
        explicit if_then_else(const std::shared_ptr<ast::base> test, const std::shared_ptr<ast::base> then_expr, const std::shared_ptr<ast::base> else_expr) : base(), test(test), then_expr(then_expr), else_expr(else_expr) {}
        virtual void stream(std::ostream& o) const override { o << "if " << test << " then " << then_expr << " else " << else_expr; }
        virtual bool operator==(const base* o) const override { auto cast = dynamic_cast<const if_then_else*>(o); return cast && test == cast->test && then_expr == cast->then_expr && else_expr == cast->else_expr; }
        const std::shared_ptr<ast::base> test;
        const std::shared_ptr<ast::base> then_expr;
        const std::shared_ptr<ast::base> else_expr;
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

    struct any_string : base {
        std::vector<std::shared_ptr<ast::base>> data;
        std::string unescaped;

        void push_back() {
            assert(!value);
            data.push_back(std::make_shared<ast::string_literal>(std::move(unescaped)));
            unescaped = std::string();
        }

        void push_back_expr() {
            if (unescaped.length()) {
                data.push_back(std::make_shared<ast::string_literal>(std::move(unescaped)));
                unescaped = std::string();
            }
            assert(value);
            data.push_back(std::make_shared<ast::dollar_curly>(std::move(value)));
        }

    };

    struct string : any_string {
        void success(base& in_result) {
            assert(!value);
            assert(!in_result.value);
            in_result.value = std::make_shared<ast::short_string>(std::move(data));
        }
    };

    struct long_string : any_string {
        unsigned int prefix_len;

        void prefix(unsigned int prefix) { prefix_len = std::min(prefix_len, prefix); }

        void success(base& in_result) {
            assert(!value);
            assert(!in_result.value);
            in_result.value = std::make_shared<ast::long_string>(std::move(data), prefix_len);
        }
    };

    template<typename T>
    struct binary_expression : base {
        void success(base& in_result) {
            assert(in_result.value);
            assert(value);
            in_result.value = std::make_shared<T>(std::move(in_result.value), std::move(value));
        }
    };

    struct if_then_else : base {
        std::shared_ptr<ast::base> test;
        std::shared_ptr<ast::base> then_expr;

        void set_test() {
            assert(value);
            test = std::move(value);
        }

        void set_then() {
            assert(value);
            then_expr = std::move(value);
        }

        void success(base& in_result) {
            assert(!in_result.value);
            assert(value);
            in_result.value = std::make_shared<ast::if_then_else>(std::move(test), std::move(then_expr), std::move(value));
        }
    };

    struct binding_inherit : base {
        std::shared_ptr<ast::base> from;
        std::vector<std::shared_ptr<ast::base>> attrs;

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
            in_result.value = std::make_shared<ast::binding_inherit>(from, attrs);
        }
    };

    struct binds : base {
        std::vector<std::shared_ptr<ast::base>> data;

        void push_back() {
            assert(value);
            assert(std::dynamic_pointer_cast<ast::binding_eq>(value) || std::dynamic_pointer_cast<ast::binding_inherit>(value));
            data.push_back(std::move(value));
        }

        void success(base& in_result) {
            assert(!value);
            assert(!in_result.value);
            in_result.value = std::make_shared<ast::binds>(std::move(data));
        }
    };

    struct formals : base {
        std::vector<std::shared_ptr<ast::base>> data;

        void push_back() {
            assert(value);
            //assert(std::dynamic_pointer_cast<ast::formal>(value));
            data.push_back(std::move(value));
        }

        void success(base& in_result) {
            assert(!value);
            assert(!in_result.value);
            in_result.value = std::make_shared<ast::formals>(std::move(data));
        }
    };

    struct array : base {
        std::vector<std::shared_ptr<ast::base>> data;

        void push_back() {
            assert(value);
            data.push_back(std::move(value));
        };

        void success(base& in_result) {
            assert(!in_result.value);
            assert(!value);
            in_result.value = std::make_shared<ast::array>(std::move(data));
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

struct short_string_escaped : pegtl::one<'"', '$', '\\', 'n'> {};
struct short_string_accepted : pegtl::any {};
struct short_string_character : pegtl::sor<pegtl::seq<pegtl::one<'\\'>, short_string_escaped>, short_string_accepted> {};
struct short_string_content : pegtl::until<pegtl::one<'"'>, pegtl::sor<dollarcurly_expr, short_string_character>> {};
struct short_string : pegtl::if_must<pegtl::one<'"'>, short_string_content> {};
// XXX: add prefix stripping

struct long_string_escape_accepted : pegtl::sor<pegtl::one<'\''>, pegtl::one<'$'>, pegtl::seq<pegtl::one<'\\'>, pegtl::one<'n', 'r', 't'>>> {};
struct long_string_escaped : pegtl::seq<pegtl::two<'\''>, long_string_escape_accepted> {};
struct long_string_accepted : pegtl::seq<pegtl::not_at<pegtl::two<'\''>>, pegtl::any> {};
struct long_string_content : pegtl::star<pegtl::sor<long_string_escaped, dollarcurly_expr, long_string_accepted>> {};
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
    struct str_forbidden:pegtl::sor<str_if, str_then, str_else, str_assert, str_with, str_let, str_rec, str_inherit, str_in, str_true, str_false> {};
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

    struct do_backtrack : pegtl::success {};
    template<typename from>
    struct backtrack : pegtl::seq<do_backtrack, pegtl::failure> {};

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

    struct attrpath_apply : pegtl::if_must<padr<pegtl::one<'.'>>, padr<attrtail>> {};
    struct attrpath : pegtl::seq<padr<attr>, pegtl::star<attrpath_apply>> {};

    template<typename CTX = void>
    struct expression;

    struct formal_apply : pegtl::if_must<padr<pegtl::one<'?'>>, expression<>> {};
    struct formal : pegtl::seq<padr<name>, pegtl::opt<formal_apply>> {};
    struct formals_nonempty : pegtl::seq<pegtl::list<formal, padr<pegtl::one<','>>>, pegtl::opt<padr<pegtl::one<','>>, pegtl::opt<keyword::key_ellipsis>>> {};
    struct formals : pegtl::opt<pegtl::sor<keyword::key_ellipsis, formals_nonempty>> {};

    struct argument_single : pegtl::seq<pegtl::one<':'>, padr<pegtl::sor<sep, pegtl::at<pegtl::one<'[', '(', '{','!'>>>>> {};
    struct argument_formals : pegtl::seq<padr<pegtl::one<'{'>>, formals, pegtl::one<'}'>> {};
    struct argument_set_prebind : pegtl::seq<pegtl::if_must<padr<pegtl::one<'@'>>, padr<argument_formals>>, padr<pegtl::one<':'>>> {};
    struct argument_set_postbind : pegtl::sor<pegtl::seq<padr<argument_formals>, pegtl::opt<padr<pegtl::one<'@'>>, padr<name>>, padr<pegtl::one<':'>>>, backtrack<argument_set_postbind>> {};
    struct argument_prebind : pegtl::seq<padr<name>, pegtl::sor<argument_set_prebind, argument_single, backtrack<argument_prebind>>> {};
    struct arguments : pegtl::sor<argument_set_postbind, argument_prebind> {};


    struct bind;

    template<typename U>
    struct binds : pegtl::until<U, bind> {};

    struct table_begin_recursive : pegtl::seq<keyword::key_rec, padr<pegtl::one<'{'>>> {};
    struct table_begin_nonrecursive : padr<pegtl::one<'{'>> {};
    struct table_end : pegtl::one<'}'> {};
    template<typename table_begin>
    struct table_apply : pegtl::if_must<table_begin, binds<table_end>> {};
    struct table : pegtl::sor<table_apply<table_begin_recursive>, table_apply<table_begin_nonrecursive>> {};

    //struct bind_eq_attrpath : attrpath {};
    struct bind_eq_operator : padr<pegtl::one<'='>> {};
//XXX shortcut here?
    struct bind_eq_apply : pegtl::seq<expression<>, padr<pegtl::one<';'>>> {};
    struct bind_eq : pegtl::seq<padr<attrpath>, pegtl::if_must<bind_eq_operator, bind_eq_apply>> {};
    struct bind_inherit_attrname : attrpath {};
    struct bind_inherit_from : pegtl::if_must<padr<pegtl::one<'('>>, expression<table>, padr<pegtl::one<')'>>> {};
    struct bind_inherit_apply : pegtl::seq<pegtl::opt<bind_inherit_from>, pegtl::star<padr<bind_inherit_attrname>>, padr<pegtl::one<';'>>> {};
    struct bind_inherit : pegtl::if_must<keyword::key_inherit, bind_inherit_apply> {};
    struct bind : pegtl::sor<bind_eq, bind_inherit> {};




    struct array_content_apply;

    struct array_begin : padr<pegtl::one<'['>> {};
    struct array_content : pegtl::until<pegtl::one<']'>, array_content_apply> {};
    struct array : pegtl::if_must<array_begin, array_content> {};

    struct dollarcurly_expr : pegtl::if_must<padr<pegtl::string<'$', '{'>>, expression<string>, pegtl::one<'}'>> {};
    struct bracket_expr : pegtl::if_must<padr<pegtl::one<'('>>, expression<>, pegtl::one<')'>> {};



    struct variable_tail;

    struct expr_select : pegtl::seq<pegtl::sor<table, bracket_expr, name>, seps, pegtl::star<pegtl::seq<padr<pegtl::one<'.'>>, variable_tail>>> {};
    struct expr_simple : pegtl::sor<boolean, number, string, array, dollarcurly_expr, spath, path, uri> {};
    struct expr_applying_tail : pegtl::sor<padr<expr_simple>, expr_select> {};

    struct variable_tail_or_apply : expr_applying_tail {};
    struct variable_tail_or : pegtl::if_must<keyword::key_or, variable_tail_or_apply> {};
    struct variable_tail : pegtl::seq<pegtl::sor<name, string, dollarcurly_expr>, seps, pegtl::opt<variable_tail_or>> {};


// XXX reorder, this should be a few lines above near other array stuff
    struct array_content_apply : expr_applying_tail {};
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


    struct operator_attrtest : padr<pegtl::one<'?'>> {};
    struct expr_attrtest_apply : pegtl::if_must<operator_attrtest, attrpath> {};
    struct expr_attrtest : pegtl::seq<expr_negate<void>, pegtl::opt<expr_attrtest_apply>> {};


//XXX right assoc?
    struct operator_concat : padr<pegtl::two<'+'>> {};
    struct expr_concat_apply : pegtl::if_must<operator_concat, expr_apply<array>> {};
    struct expr_concat : pegtl::seq<expr_attrtest, pegtl::star<expr_concat_apply>> {};


    struct operator_div : op_one<'/', '/'> {};
    struct expr_div_apply : pegtl::if_must<operator_div, expr_negate<number>> {};
    struct expr_div : pegtl::seq<expr_concat, pegtl::star<expr_div_apply>> {};


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


//XXX: right assoc
    struct operator_merge : padr<pegtl::two<'/'>> {};
    struct expr_merge_apply : pegtl::if_must<operator_merge, expr_apply<table>> {};
    struct expr_merge : pegtl::seq<expr_not, pegtl::star<expr_merge_apply>> {};


    struct operators_ordering : padr<pegtl::sor<pegtl::string<'<', '='>, pegtl::string<'>', '='>, pegtl::one<'<', '>'>>> {};
    template<typename CTX>
    struct expr_ordering : left_assoc<expr_merge, operators_ordering, expr_add> {};
    //template<> struct expr_ordering<number> : left_assoc<expr_sum<number>, operators_ordering> {};

    template<char... CMP>
    struct expr_eq_apply : pegtl::if_must<padr<pegtl::string<CMP...>>, expr_ordering<void>> {};
// todo: we cannot do anything here, right?
    template<typename CTX>
    struct expr_equality : pegtl::seq<expr_ordering<CTX>, pegtl::sor<expr_eq_apply<'=', '='>, expr_eq_apply<'!', '='>, pegtl::success>> {};


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
    struct expr_if_apply : pegtl::seq<expression<boolean>, keyword::key_then, expression<CTX>, keyword::key_else, expression<CTX>> {};
    template<typename CTX>
    struct expr_if : pegtl::if_must<keyword::key_if, expr_if_apply<CTX>> {};

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
    struct let_apply : expression<CTX> {};
    template<typename CTX>
    struct let : pegtl::if_must<keyword::key_let, binds<keyword::key_in>, let_apply<CTX>> {};

    template<typename CTX>
    struct function_apply;
    template<typename CTX>
    struct function : pegtl::if_must<arguments, function_apply<CTX>> {};

    template<typename CTX>
    struct statement : pegtl::sor<assert<CTX>, with<CTX>, let<CTX>, function<CTX>> {}; // seq<.., /*expr_import,*/>
//    struct statement_list : pegtl::star<statement> {};

    template<> struct expression<void> : pegtl::sor<statement<void>, expr_if<void>, expr_impl<void>> {};
    template<> struct expression<boolean> : pegtl::sor<statement<boolean>, expr_if<boolean>, expr_impl<boolean>> {};
    template<> struct expression<string> : pegtl::sor<statement<string>, expr_if<string>, expr_add> {};
    template<> struct expression<table> : pegtl::sor<statement<table>, expr_if<table>, expr_merge> {};

    template<typename CTX>
    struct function_apply : expression<CTX> {};

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

        template<> struct binds<bind_inherit_attrname> {
            template<typename Input> static void apply(const Input& in, state::binding_inherit& state) {
                state.push_back();
            }
        };

        template<> struct binds<bind_inherit_from> {
            template<typename Input> static void apply(const Input& in, state::binding_inherit& state) {
                state.set_from();
            }
        };

        template<> struct binds<bind> {
            template<typename Input> static void apply(const Input& in, state::binds& state) {
                state.push_back();
            }
        };


        template<typename Rule>
        struct array : action<Rule> {};

        template<> struct array<array_content_apply> {
            template<typename Input> static void apply(const Input& in, state::array& state) {
                state.push_back();
            }
        };


        template<typename Rules>
        struct formals : action<Rules> {};

        template<> struct formals<keyword::key_ellipsis> {
            template<typename Input> static void apply(const Input& in, state::formals& state) {
                state.value = std::make_shared<ast::ellipsis>();
                state.push_back();
            }
        };

        template<> struct formals<formal> {
            template<typename Input> static void apply(const Input& in, state::formals& state) {
                state.push_back();
            }
        };

        template<typename Rules>
        struct if_then_else : action<Rules> {};

        template<> struct if_then_else<keyword::key_then> {
            template<typename Input> static void apply(const Input& in, state::if_then_else& state) {
                state.set_test();
            }
        };

        template<> struct if_then_else<keyword::key_else> {
            template<typename Input> static void apply(const Input& in, state::if_then_else& state) {
                state.set_then();
            }
        };


        template<typename Rules>
        struct string : pegtl::nothing<Rules> {};

        //template<> struct string<short_string_escaped> : pegtl::unescape::unescape_c<short_string_escaped,
        template<> struct string<short_string_escaped> : pegtl::unescape::unescape_c<short_string_escaped, '"', '$', '\\', 'n'> {};
        template<> struct string<short_string_accepted> : pegtl::unescape::append_all {};

        template<> struct string<short_string_content> {
            template<typename Input> static void apply(const Input& in, state::string& state) {
                state.push_back();
            }
        };

        template<> struct string<dollarcurly_expr> {
            template<typename Input> static void apply(const Input& in, state::any_string& state) {
                state.push_back_expr();
            }
        };

        template<> struct string<long_string_escape_accepted> : pegtl::unescape::append_all {};
        template<> struct string<long_string_accepted> : pegtl::unescape::append_all {};

        template<> struct string<long_string_content> {
            template<typename Input>
            static void apply(const Input& in, state::long_string& state) {
                state.push_back();
            }
        };
    } // namespace actions


    template<typename x> struct control::normal<expression<x>> : pegtl::change_state_and_action<expression<x>, state::base, action, pegtl::normal> {};
    template<> struct control::normal<expr_add_apply> : pegtl::change_state<expr_add_apply, state::binary_expression<ast::add>, pegtl::normal> {};
    template<> struct control::normal<expr_sub_apply> : pegtl::change_state<expr_sub_apply, state::binary_expression<ast::sub>, pegtl::normal> {};
    template<> struct control::normal<expr_mul_apply> : pegtl::change_state<expr_mul_apply, state::binary_expression<ast::mul>, pegtl::normal> {};
    template<> struct control::normal<expr_div_apply> : pegtl::change_state<expr_div_apply, state::binary_expression<ast::div>, pegtl::normal> {};
    template<> struct control::normal<expr_concat_apply> : pegtl::change_state<expr_concat_apply, state::binary_expression<ast::concat>, pegtl::normal> {};
    template<> struct control::normal<expr_merge_apply> : pegtl::change_state<expr_merge_apply, state::binary_expression<ast::merge>, pegtl::normal> {};
    template<> struct control::normal<expr_or_apply> : pegtl::change_state<expr_or_apply, state::binary_expression<ast::or_>, pegtl::normal> {};
    template<> struct control::normal<expr_and_apply> : pegtl::change_state<expr_and_apply, state::binary_expression<ast::and_>, pegtl::normal> {};
    template<char... CMP> struct control::normal<expr_eq_apply<CMP...>> : pegtl::change_state<expr_eq_apply<CMP...>, state::binary_expression<ast::binary_expression<CMP...>>, pegtl::normal> {};
    template<> struct control::normal<expr_impl_apply> : pegtl::change_state<expr_impl_apply, state::binary_expression<ast::impl>, pegtl::normal> {};
    template<> struct control::normal<variable_tail> : pegtl::change_state<variable_tail, state::binary_expression<ast::attrpath>, pegtl::normal> {};
    template<> struct control::normal<attrpath_apply> : pegtl::change_state<attrpath_apply, state::binary_expression<ast::attrpath>, pegtl::normal> {};
    template<> struct control::normal<expr_attrtest_apply> : pegtl::change_state<expr_attrtest_apply, state::binary_expression<ast::attrtest>, pegtl::normal> {};
    template<> struct control::normal<array_content> : pegtl::change_state_and_action<array_content, state::array, actions::array, pegtl::normal> {};
    template<> struct control::normal<bind_eq_apply> : pegtl::change_state<bind_eq_apply, state::binary_expression<ast::binding_eq>, pegtl::normal> {};
    template<> struct control::normal<bind_inherit_apply> : pegtl::change_state<bind_inherit_apply, state::binding_inherit, pegtl::normal> {};
    template<> struct control::normal<expr_applying_tail> : pegtl::change_state<expr_applying_tail, state::binary_expression<ast::call>, pegtl::normal> {};
    template<> struct control::normal<variable_tail_or_apply> : pegtl::change_state<variable_tail_or_apply, state::binary_expression<ast::set_or>, pegtl::normal> {};
    template<> struct control::normal<formal_apply> : pegtl::change_state_and_action<formal_apply, state::binary_expression<ast::formal>, action, pegtl::normal> {};
    template<> struct control::normal<formals> : pegtl::change_state_and_action<formals, state::formals, actions::formals, pegtl::normal> {};
    template<typename x> struct control::normal<binds<x>> : pegtl::change_state_and_action<binds<x>, state::binds, actions::binds, pegtl::normal> {};
    template<typename x> struct control::normal<assert_apply<x>> : pegtl::change_state<assert_apply<x>, state::binary_expression<ast::assertion>, pegtl::normal> {};
    template<typename x> struct control::normal<with_apply<x>> : pegtl::change_state<with_apply<x>, state::binary_expression<ast::with>, pegtl::normal> {};
    template<typename x> struct control::normal<let_apply<x>> : pegtl::change_state<let_apply<x>, state::binary_expression<ast::let> , pegtl::normal> {};
    template<typename x> struct control::normal<function_apply<x>> : pegtl::change_state<function_apply<x>, state::binary_expression<ast::function> , pegtl::normal> {};
    template<typename x> struct control::normal<expr_if_apply<x>> : pegtl::change_state_and_action<expr_if_apply<x>, state::if_then_else, actions::if_then_else, pegtl::normal> {};
    template<> struct control::normal<short_string_content> : pegtl::change_state_and_action<short_string_content, state::string, actions::string, pegtl::normal> {};
    template<> struct control::normal<long_string_content> : pegtl::change_state_and_action<long_string_content, state::long_string, actions::string, pegtl::normal> {};
//    template<typename x> struct control::normal<backtrack<x>> : pegtl::normal<backtrack<x>> {};



    template<typename Rule>
    struct errors : pegtl::normal<Rule> {
        static const std::string error_message;
        template<typename Input, typename... States>
        static void raise(const Input& in, States&& ...) {
            throw pegtl::parse_error(error_message, in);
        }
    };

    template<> struct action<do_backtrack> {
        template<typename Input> static void apply(const Input& in, state::base& state) {
//XXX assert state.value to find superfluous backtrackings
            state.value.reset();
        }
    };

    template<> struct action<number> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            assert(!state.value);
            state.value = std::make_shared<ast::number>(std::stoll(in.string()));
        }
    };

    template<> struct action<name> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            assert(!state.value);
            state.value = std::make_shared<ast::name>(in.string());
        }
    };

    template<> struct action<path> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            assert(!state.value);
            state.value = std::make_shared<ast::path>(in.string());
        }
    };


    template<> struct action<keyword::key_true> {
        template<typename Input>
        static void apply(const Input& in, state::base& state) {
            assert(!state.value);
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

    template<> struct action<table_apply<table_begin_nonrecursive>> {
        template<typename Input> static void apply(const Input& in, state::base& state) {
            assert(state.value);
            state.value = std::make_shared<ast::table>(std::move(state.value), false);
        }
    };

    template<> struct action<table_apply<table_begin_recursive>> {
        template<typename Input> static void apply(const Input& in, state::base& state) {
            assert(state.value);
            state.value = std::make_shared<ast::table>(std::move(state.value), true);
        }
    };



//    template<> const std::string errors<expr_negate<number>>::error_message = "incomplete negate expression, expected number";
//    template<> const std::string errors<expr_sum<number>>::error_message = "incomplete sum expression, expected number";


} // namespace parser

} // namespace nix






