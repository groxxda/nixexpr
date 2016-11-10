#pragma once

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

namespace nix {

namespace ast {
class node {
    struct node_interface {
        virtual ~node_interface() = default;
        virtual void stream(std::ostream&) const = 0;
        virtual bool operator==(const node_interface& o) const = 0;
    };

    template <typename T> struct node_impl : node_interface {
        node_impl(T t) : object(std::move(t)) {}
        virtual ~node_impl() = default;
        virtual void stream(std::ostream& o) const override {
            object.stream(o);
        }
        virtual bool operator==(const node_interface& o) const override {
            const node_impl<T>* oc = dynamic_cast<const node_impl<T>*>(&o);
            if(!oc) return false;
            return object == oc->object;
        }

    private:
        const T object;
    };

    std::shared_ptr<node_interface> object;

public:
    template <typename T>
    explicit node(const T& t) : object(new node_impl<T>(t)) {}
    void stream(std::ostream& o) const { object->stream(o); }
    bool operator==(const node& o) const { return *object == *o.object; }
};

inline std::ostream& operator<<(std::ostream& o, const node& b) {
    b.stream(o);
    return o;
}

struct number {
    explicit number(const uint64_t in_data) : data(in_data){};
    void stream(std::ostream& o) const { o << data; }
    bool operator==(const number& o) const { return data == o.data; }
    const uint64_t data;
};

struct string_literal {
    explicit string_literal(std::string&& in_data) : data(std::move(in_data)){};
    bool operator==(const string_literal& o) const { return data == o.data; }
    void stream(std::ostream& o) const { o << data; }
    const std::string data;
};

struct short_string {
    explicit short_string(std::vector<ast::node>&& parts)
        : parts(std::move(parts)){};
    bool operator==(const short_string& o) const { return parts == o.parts; }
    void stream(std::ostream& o) const {
        o << "\"";
        for(const auto& i : parts) o << i;
        o << "\"";
    }
    const std::vector<ast::node> parts;
};

struct long_string {
    explicit long_string(std::vector<ast::node>&& parts,
                         const unsigned int prefixlen)
        : parts(std::move(parts)), prefixlen(prefixlen) {}
    bool operator==(const long_string& o) const {
        return prefixlen == o.prefixlen && parts == o.parts;
    }
    void stream(std::ostream& o) const {
        o << "''";
        for(const auto& i : parts) o << i;
        o << "''";
    }
    const std::vector<ast::node> parts;
    const unsigned int prefixlen;
};

struct name {
    explicit name(const std::string&& in_data) : data(in_data){};
    void stream(std::ostream& o) const { o << data; }
    bool operator==(const name& o) const { return data == o.data; }
    const std::string data;
};

struct path {
    explicit path(const std::string&& in_data) : data(in_data){};
    void stream(std::ostream& o) const { o << data; }
    bool operator==(const path& o) const { return data == o.data; }
    const std::string data;
};

struct spath {
    explicit spath(const std::string&& in_data) : data(std::move(in_data)){};
    void stream(std::ostream& o) const { o << "<" << data << ">"; }
    bool operator==(const spath& o) const { return data == o.data; }
    const std::string data;
};

struct boolean {
    explicit boolean(const bool in_data) : data(in_data){};
    bool operator==(const boolean& o) const { return data == o.data; }
    void stream(std::ostream& o) const { o << std::boolalpha << data; }
    operator bool() const { return data; }
    const bool data;
};

struct ellipsis {
    explicit ellipsis(){};
    void stream(std::ostream& o) const { o << "..."; }
    bool operator==(const ellipsis& o) const { return true; }
};

template <char op> struct unary_expression {
    explicit unary_expression(ast::node&& value) : value(std::move(value)) {}
    explicit unary_expression(std::unique_ptr<ast::node>&& ptr)
        : value(*ptr.release()) {}
    void stream(std::ostream& o) const { o << op << value; }
    const ast::node value;
};

struct not_ : public unary_expression<'!'> {
    using unary_expression<'!'>::unary_expression;
    bool operator==(const not_& o) const { return value == o.value; }
};

struct negate : public unary_expression<'-'> {
    using unary_expression<'-'>::unary_expression;
    bool operator==(const negate& o) const { return value == o.value; }
};

struct dollar_curly : public unary_expression<'$'> {
    using unary_expression<'$'>::unary_expression;
    bool operator==(const dollar_curly& o) const { return value == o.value; }
    void stream(std::ostream& o) const { o << "${" << value << "}"; }
};

template <char... op> struct binary_expression {
    explicit binary_expression(ast::node&& lhs, ast::node&& rhs)
        : lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    explicit binary_expression(std::unique_ptr<ast::node>&& lhs,
                               std::unique_ptr<ast::node>&& rhs)
        : binary_expression<op...>(std::move(*lhs.release()),
                                   std::move(*rhs.release())) {}
    void stream(std::ostream& o) const {
        o << "(" << lhs;
        (o << ... << op) << rhs << ")";
    }
    const ast::node lhs;
    const ast::node rhs;
};

struct add : public binary_expression<'+'> {
    using binary_expression<'+'>::binary_expression;
    bool operator==(const add& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct sub : public binary_expression<'-'> {
    using binary_expression<'-'>::binary_expression;
    bool operator==(const sub& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct mul : public binary_expression<'*'> {
    using binary_expression<'*'>::binary_expression;
    bool operator==(const mul& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct div : public binary_expression<'/'> {
    using binary_expression<'/'>::binary_expression;
    bool operator==(const div& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct or_ : public binary_expression<'|', '|'> {
    using binary_expression<'|', '|'>::binary_expression;
    bool operator==(const or_& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct and_ : public binary_expression<'&', '&'> {
    using binary_expression<'&', '&'>::binary_expression;
    bool operator==(const and_& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
};

struct impl : public binary_expression<'-', '>'> {
    using binary_expression<'-', '>'>::binary_expression;
    bool operator==(const impl& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
};

struct concat : public binary_expression<'+', '+'> {
    using binary_expression<'+', '+'>::binary_expression;
    bool operator==(const concat& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
};

struct merge : public binary_expression<'/', '/'> {
    using binary_expression<'/', '/'>::binary_expression;
    bool operator==(const merge& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
};

struct eq : public binary_expression<'=', '='> {
    using binary_expression<'=', '='>::binary_expression;
    bool operator==(const eq& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct neq : public binary_expression<'!', '='> {
    using binary_expression<'!', '='>::binary_expression;
    bool operator==(const neq& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct gt : public binary_expression<'>'> {
    using binary_expression<'>'>::binary_expression;
    bool operator==(const gt& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct geq : public binary_expression<'>', '='> {
    using binary_expression<'>', '='>::binary_expression;
    bool operator==(const geq& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct lt : public binary_expression<'<'> {
    using binary_expression<'<'>::binary_expression;
    bool operator==(const lt& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct leq : public binary_expression<'<', '='> {
    using binary_expression<'<', '='>::binary_expression;
    bool operator==(const leq& o) const { return lhs == o.lhs && rhs == o.rhs; }
};

struct attrtest : public binary_expression<'?'> {
    using binary_expression<'?'>::binary_expression;
    bool operator==(const attrtest& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
};

struct attrpath : public binary_expression<'.'> {
    using binary_expression<'.'>::binary_expression;
    bool operator==(const attrpath& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
};

struct lookup : public binary_expression<'.'> {
    using binary_expression<'.'>::binary_expression;
    bool operator==(const lookup& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
};

struct lookup_or {
    explicit lookup_or(ast::node&& from, ast::node&& path, ast::node&& or_)
        : from(std::move(from)), path(std::move(path)), or_(std::move(or_)) {}
    explicit lookup_or(std::unique_ptr<ast::node>&& from,
                       std::unique_ptr<ast::node>&& path,
                       std::unique_ptr<ast::node>&& or_)
        : lookup_or(std::move(*from.release()),
                    std::move(*path.release()),
                    std::move(*or_.release())) {}

    void stream(std::ostream& o) const {
        o << from << "." << path << " or " << or_;
    }
    bool operator==(const lookup_or& o) const {
        return from == o.from && path == o.path && from == o.from;
    }

    const ast::node from;
    const ast::node path;
    const ast::node or_;
};

struct call : public binary_expression<' '> {
    using binary_expression<' '>::binary_expression;
    bool operator==(const call& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
};

struct binding_eq : public binary_expression<'='> {
    using binary_expression<'='>::binary_expression;
    bool operator==(const binding_eq& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
    void stream(std::ostream& o) const { o << lhs << " = " << rhs; }
};

struct binding_inherit {
    explicit binding_inherit(std::vector<ast::node>&& attrs)
        : attrs(std::move(attrs)){};
    bool operator==(const binding_inherit& o) const { return attrs == o.attrs; }
    void stream(std::ostream& o) const {
        o << "inherit";
        for(const auto& i : attrs) o << " " << i;
    }
    const std::vector<ast::node> attrs;
};

struct binding_inherit_from {
    explicit binding_inherit_from(ast::node&& from,
                                  std::vector<ast::node>&& attrs)
        : from(std::move(from)), attrs(std::move(attrs)){};
    explicit binding_inherit_from(std::unique_ptr<ast::node>&& from,
                                  std::vector<ast::node>&& attrs)
        : binding_inherit_from(std::move(*from.release()), std::move(attrs)) {}
    bool operator==(const binding_inherit_from& o) const {
        return from == o.from && attrs == o.attrs;
    }
    void stream(std::ostream& o) const {
        o << "inherit (" << from << ")";
        for(const auto& i : attrs) o << " " << i;
    }
    const ast::node from;
    const std::vector<ast::node> attrs;
};

struct binds {
    explicit binds(std::vector<ast::node>&& data) : data(std::move(data)){};
    bool operator==(const binds& o) const { return data == o.data; }
    void stream(std::ostream& o) const {
        for(const auto& i : data) o << i << "; ";
    }
    const std::vector<ast::node> data;
};

struct formal : public binary_expression<'?'> {
    using binary_expression<'?'>::binary_expression;
    bool operator==(const formal& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
    void stream(std::ostream& o) const { o << lhs << " ? " << rhs; }
};

struct formals {
    explicit formals(std::vector<ast::node>&& data) : data(std::move(data)) {}
    bool operator==(const formals& o) const { return data == o.data; }
    void stream(std::ostream& o) const {
        o << "{";
        auto i = data.cbegin();
        if(i != data.cend()) o << " " << *i++;
        while(i != data.cend()) o << ", " << *i++;
        o << " }";
    }
    const std::vector<ast::node> data;
};

struct named_formals : public binary_expression<'@'> {
    using binary_expression<'@'>::binary_expression;
    bool operator==(const named_formals& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
};

struct table {
    explicit table(ast::node&& data, bool recursive)
        : binds(std::move(data)), recursive(recursive) {}
    explicit table(std::unique_ptr<ast::node>&& data, bool recursive)
        : table(std::move(*data.release()), recursive) {}
    bool operator==(const table& o) const {
        return binds == o.binds && recursive == o.recursive;
    }
    void stream(std::ostream& o) const {
        if(recursive) o << "rec ";
        o << "{ " << binds << "}";
    }
    const ast::node binds;
    const bool recursive;
};

struct array {
    explicit array(std::vector<ast::node>&& data) : data(std::move(data)){};
    bool operator==(const array& o) const { return data == o.data; }
    void stream(std::ostream& o) const {
        o << "[ ";
        for(const auto& i : data) o << i << " ";
        o << "]";
    }
    const std::vector<ast::node> data;
};

struct let : public binary_expression<'l', 'e', 't'> {
    using binary_expression<'l', 'e', 't'>::binary_expression;
    bool operator==(const let& o) const { return lhs == o.lhs && rhs == o.rhs; }
    void stream(std::ostream& o) const { o << "let " << lhs << "in " << rhs; }
};

struct function : public binary_expression<'f', 'u', 'n'> {
    using binary_expression<'f', 'u', 'n'>::binary_expression;
    bool operator==(const function& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
    void stream(std::ostream& o) const { o << lhs << ": " << rhs; }
};

struct assertion : public binary_expression<'a', 's', 's', 'e', 'r', 't'> {
    using binary_expression<'a', 's', 's', 'e', 'r', 't'>::binary_expression;
    bool operator==(const assertion& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
    void stream(std::ostream& o) const { o << "assert " << lhs << "; " << rhs; }
};

struct with : public binary_expression<'w', 'i', 't', 'h'> {
    using binary_expression<'w', 'i', 't', 'h'>::binary_expression;
    bool operator==(const with& o) const {
        return lhs == o.lhs && rhs == o.rhs;
    }
    void stream(std::ostream& o) const { o << "with " << lhs << "; " << rhs; }
};

struct if_then_else {
    explicit if_then_else(ast::node&& test,
                          ast::node&& then_expr,
                          ast::node&& else_expr)
        : test(std::move(test)), then_expr(std::move(then_expr)),
          else_expr(std::move(else_expr)) {}
    explicit if_then_else(std::unique_ptr<ast::node>&& test,
                          std::unique_ptr<ast::node>&& then_expr,
                          std::unique_ptr<ast::node>&& else_expr)
        : if_then_else(std::move(*test.release()),
                       std::move(*then_expr.release()),
                       std::move(*else_expr.release())) {}
    bool operator==(const if_then_else& o) const {
        return test == o.test && then_expr == o.then_expr &&
               else_expr == o.else_expr;
    }
    void stream(std::ostream& o) const {
        o << "if " << test << " then " << then_expr << " else " << else_expr;
    }
    const ast::node test;
    const ast::node then_expr;
    const ast::node else_expr;
};
} // namespace ast

} // namespace nix
