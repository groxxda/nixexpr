#pragma once

#include <iosfwd>
#include <string>
#include <vector>

namespace nix {

namespace ast {
struct base {
    virtual void stream(std::ostream&) const = 0;
    virtual bool operator==(const base* o) const {
        return o && o->operator==(*this);
    }
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
    virtual ~base() = default;

protected:
    explicit base() {}
};

inline std::ostream& operator<<(std::ostream& o, const base& b) {
    b.stream(o);
    return o;
}

inline std::ostream& operator<<(std::ostream& o,
                                const std::unique_ptr<base>& b) {
    return b ? (o << *b) : (o << "NULL");
}

inline bool operator==(const std::unique_ptr<base>& a,
                       const std::unique_ptr<base>& b) {
    return a->operator==(b.get());
}

struct number : public base {
    explicit number(const unsigned long long in_data) : base(), data(in_data){};
    virtual void stream(std::ostream& o) const override { o << data; }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const number*>(o);
        return cast && data == cast->data;
    }
    const unsigned long long data;
};

struct string_literal : public base {
    explicit string_literal(std::string&& in_data)
        : base(), data(std::move(in_data)){};
    virtual void stream(std::ostream& o) const override { o << data; }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const string_literal*>(o);
        return cast && data == cast->data;
    }
    const std::string data;
};

struct short_string : public base {
    explicit short_string(std::vector<std::unique_ptr<ast::base>>&& parts)
        : base(), parts(std::move(parts)){};
    virtual void stream(std::ostream& o) const override {
        o << "\"";
        for(const auto& i : parts) o << *i;
        o << "\"";
    }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const short_string*>(o);
        return cast && parts == cast->parts;
    }
    const std::vector<std::unique_ptr<ast::base>> parts;
};

struct long_string : public base {
    explicit long_string(std::vector<std::unique_ptr<ast::base>>&& parts,
                         const unsigned int prefixlen)
        : base(), parts(std::move(parts)), prefixlen(prefixlen) {}
    virtual void stream(std::ostream& o) const override {
        o << "''";
        for(const auto& i : parts) o << *i;
        o << "''";
    }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const long_string*>(o);
        return cast && parts == cast->parts;
    }
    const std::vector<std::unique_ptr<ast::base>> parts;
    const unsigned int prefixlen;
};

struct name : public base {
    explicit name(const std::string&& in_data) : base(), data(in_data){};
    virtual void stream(std::ostream& o) const override { o << data; }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const name*>(o);
        return cast && data == cast->data;
    }
    const std::string data;
};

struct path : public base {
    explicit path(const std::string&& in_data) : base(), data(in_data){};
    virtual void stream(std::ostream& o) const override { o << data; }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const name*>(o);
        return cast && data == cast->data;
    }
    const std::string data;
};

struct boolean : public base {
    explicit boolean(const bool in_data) : base(), data(in_data){};
    virtual void stream(std::ostream& o) const override {
        o << std::boolalpha << data;
    }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const boolean*>(o);
        return cast && data == cast->data;
    }
    operator bool() const { return data; }
    const bool data;
};

struct ellipsis : public base {
    explicit ellipsis() : base(){};
    virtual void stream(std::ostream& o) const override { o << "..."; }
    virtual bool operator==(const base* o) const override {
        return dynamic_cast<const ellipsis*>(o);
    }
};

template <char op> struct unary_expression : public base {
    explicit unary_expression(std::unique_ptr<base>&& value)
        : base(), value(std::move(value)) {}
    virtual void stream(std::ostream& o) const override { o << op << value; }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const unary_expression<op>*>(o);
        return cast && value == cast->value;
    }
    const std::unique_ptr<base> value;
};

struct not_ : public unary_expression<'!'> {
    using unary_expression<'!'>::unary_expression;
};

struct negate : public unary_expression<'-'> {
    using unary_expression<'-'>::unary_expression;
};

struct dollar_curly : public unary_expression<'$'> {
    using unary_expression<'$'>::unary_expression;
    virtual void stream(std::ostream& o) const override {
        o << "${" << value << "}";
    }
};

template <char... op> struct binary_expression : public base {
    explicit binary_expression(std::unique_ptr<base>&& lhs,
                               std::unique_ptr<base>&& rhs)
        : base(), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    virtual void stream(std::ostream& o) const override {
        o << "(" << lhs;
        (o << ... << op) << rhs << ")";
    }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const binary_expression<op...>*>(o);
        return cast && lhs == cast->lhs && rhs == cast->rhs;
    }
    const std::unique_ptr<base> lhs;
    const std::unique_ptr<base> rhs;
};

struct add : public binary_expression<'+'> {
    using binary_expression<'+'>::binary_expression;
};

struct sub : public binary_expression<'-'> {
    using binary_expression<'-'>::binary_expression;
};

struct mul : public binary_expression<'*'> {
    using binary_expression<'*'>::binary_expression;
};

struct div : public binary_expression<'/'> {
    using binary_expression<'/'>::binary_expression;
};

struct or_ : public binary_expression<'|', '|'> {
    using binary_expression<'|', '|'>::binary_expression;
};

struct and_ : public binary_expression<'&', '&'> {
    using binary_expression<'&', '&'>::binary_expression;
};

struct impl : public binary_expression<'-', '>'> {
    using binary_expression<'-', '>'>::binary_expression;
};

struct concat : public binary_expression<'+', '+'> {
    using binary_expression<'+', '+'>::binary_expression;
};

struct merge : public binary_expression<'/', '/'> {
    using binary_expression<'/', '/'>::binary_expression;
};

struct attrtest : public binary_expression<'?'> {
    using binary_expression<'?'>::binary_expression;
};

struct attrpath : public binary_expression<'.'> {
    using binary_expression<'.'>::binary_expression;
};

struct set_or : public binary_expression<'o', 'r'> {
    using binary_expression<'o', 'r'>::binary_expression;
};

struct call : public binary_expression<' '> {
    using binary_expression<' '>::binary_expression;
};

struct binding_eq : public binary_expression<'='> {
    using binary_expression<'='>::binary_expression;
    virtual void stream(std::ostream& o) const override {
        o << lhs << " = " << rhs;
    }
};

struct binding_inherit : public base {
    explicit binding_inherit(std::unique_ptr<ast::base>&& from,
                             std::vector<std::unique_ptr<ast::base>>&& attrs)
        : base(), from(std::move(from)), attrs(std::move(attrs)){};
    virtual void stream(std::ostream& o) const override {
        o << "inherit";
        if(from) { o << " (" << from << ")"; }
        for(const auto& i : attrs) o << " " << *i;
    }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const binding_inherit*>(o);
        return cast && from == cast->from && attrs == cast->attrs;
    }
    const std::unique_ptr<ast::base> from;
    const std::vector<std::unique_ptr<ast::base>> attrs;
};

struct binds : public base {
    explicit binds(std::vector<std::unique_ptr<ast::base>>&& data)
        : base(), data(std::move(data)){};
    virtual void stream(std::ostream& o) const override {
        for(const auto& i : data) o << i << "; ";
    }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const binds*>(o);
        return cast && data == cast->data;
    }
    const std::vector<std::unique_ptr<ast::base>> data;
};

struct formal : public binary_expression<'?'> {
    using binary_expression<'?'>::binary_expression;
    virtual void stream(std::ostream& o) const override {
        o << lhs;
        if(rhs) o << " ? " << rhs;
    }
};

struct formals : public base {
    explicit formals(std::vector<std::unique_ptr<ast::base>>&& data)
        : base(), data(std::move(data)) {}
    virtual void stream(std::ostream& o) const override {
        o << "{";
        auto i = data.cbegin();
        if(i != data.cend()) o << " " << *i++;
        while(i != data.cend()) o << ", " << *i++;
        o << " }";
    }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const formals*>(o);
        return cast && data == cast->data;
    }
    const std::vector<std::unique_ptr<ast::base>> data;
};

struct table : public base {
    explicit table(std::unique_ptr<ast::base>&& data, bool recursive)
        : binds(std::move(data)), recursive(recursive) {}
    virtual void stream(std::ostream& o) const override {
        if(recursive) o << "rec ";
        o << "{ " << binds << "}";
    }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const table*>(o);
        return cast && recursive == cast->recursive && binds == cast->binds;
    }
    const std::unique_ptr<ast::base> binds;
    const bool recursive;
};

struct array : public base {
    explicit array(std::vector<std::unique_ptr<ast::base>>&& data)
        : base(), data(std::move(data)){};
    virtual void stream(std::ostream& o) const override {
        o << "[ ";
        for(const auto& i : data) o << *i << " ";
        o << "]";
    }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const array*>(o);
        return cast && data == cast->data;
    }
    const std::vector<std::unique_ptr<ast::base>> data;
};

struct let : public binary_expression<'l', 'e', 't'> {
    using binary_expression<'l', 'e', 't'>::binary_expression;
    virtual void stream(std::ostream& o) const override {
        o << "let " << lhs << "in " << rhs;
    }
};

struct function : public binary_expression<'f', 'u', 'n'> {
    using binary_expression<'f', 'u', 'n'>::binary_expression;
    virtual void stream(std::ostream& o) const override {
        o << lhs << ": " << rhs;
    }
};

struct assertion : public binary_expression<'a', 's', 's', 'e', 'r', 't'> {
    using binary_expression<'a', 's', 's', 'e', 'r', 't'>::binary_expression;
    virtual void stream(std::ostream& o) const override {
        o << "assert " << lhs << "; " << rhs;
    }
};

struct with : public binary_expression<'w', 'i', 't', 'h'> {
    using binary_expression<'w', 'i', 't', 'h'>::binary_expression;
    virtual void stream(std::ostream& o) const override {
        o << "with " << lhs << "; " << rhs;
    }
};

struct if_then_else : public base {
    explicit if_then_else(std::unique_ptr<ast::base>&& test,
                          std::unique_ptr<ast::base>&& then_expr,
                          std::unique_ptr<ast::base>&& else_expr)
        : base(), test(std::move(test)), then_expr(std::move(then_expr)),
          else_expr(std::move(else_expr)) {}
    virtual void stream(std::ostream& o) const override {
        o << "if " << test << " then " << then_expr << " else " << else_expr;
    }
    virtual bool operator==(const base* o) const override {
        auto cast = dynamic_cast<const if_then_else*>(o);
        return cast && test == cast->test && then_expr == cast->then_expr &&
               else_expr == cast->else_expr;
    }
    const std::unique_ptr<ast::base> test;
    const std::unique_ptr<ast::base> then_expr;
    const std::unique_ptr<ast::base> else_expr;
};
} // namespace ast

} // namespace nix
