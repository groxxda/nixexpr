#pragma once

#include <memory>
#include "ast.hh"

using namespace std::literals;

nix::ast::node boolean(bool v) { return nix::ast::node(nix::ast::boolean(v)); }
nix::ast::node operator""_b(unsigned long long v) {
    return nix::ast::node(nix::ast::boolean(v));
}
nix::ast::node string(std::string&& v) {
    std::vector<nix::ast::node> vals;
    vals.emplace_back(nix::ast::string_literal(std::move(v)));

    return nix::ast::node(nix::ast::short_string(std::move(vals)));
}
nix::ast::node long_string(std::string&& v, unsigned long len) {
    std::vector<nix::ast::node> vals;
    vals.emplace_back(nix::ast::string_literal(std::move(v)));

    return nix::ast::node(nix::ast::long_string(std::move(vals),

                                                len));
}
nix::ast::node operator"" _s(const char* v, size_t len) {
    return string(std::string(v, len));
}
nix::ast::node number(unsigned long long v) {
    return nix::ast::node(nix::ast::number(v));
}
nix::ast::node operator"" _n(unsigned long long v) { return number(v); }
nix::ast::node name(std::string&& v) {
    return nix::ast::node(nix::ast::name(std::move(v)));
}
nix::ast::node operator"" _n(const char* v, size_t len) {
    return name(std::string(v, len));
}
nix::ast::node not_(nix::ast::node&& v) {
    return nix::ast::node(nix::ast::not_(std::move(v)));
}
nix::ast::node negate(nix::ast::node&& v) {
    return nix::ast::node(nix::ast::negate(std::move(v)));
}
nix::ast::node eq(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::eq(std::move(lhs), std::move(rhs)));
}
nix::ast::node neq(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::neq(std::move(lhs), std::move(rhs)));
}
nix::ast::node gt(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::gt(std::move(lhs), std::move(rhs)));
}
nix::ast::node geq(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::geq(std::move(lhs), std::move(rhs)));
}
nix::ast::node lt(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::lt(std::move(lhs), std::move(rhs)));
}
nix::ast::node leq(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::leq(std::move(lhs), std::move(rhs)));
}
nix::ast::node add(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::add(std::move(lhs), std::move(rhs)));
}
nix::ast::node sub(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::sub(std::move(lhs), std::move(rhs)));
}
nix::ast::node mul(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::mul(std::move(lhs), std::move(rhs)));
}
nix::ast::node div(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::div(std::move(lhs), std::move(rhs)));
}
nix::ast::node concat(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::concat(std::move(lhs), std::move(rhs)));
}
nix::ast::node merge(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::merge(std::move(lhs), std::move(rhs)));
}
nix::ast::node call(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::call(std::move(lhs), std::move(rhs)));
}
nix::ast::node lookup(nix::ast::node&& from, nix::ast::node&& path) {
    return nix::ast::node(nix::ast::lookup(std::move(from), std::move(path)));
}
nix::ast::node
lookup(nix::ast::node&& from, nix::ast::node&& path, nix::ast::node&& or_) {
    return nix::ast::node(
        nix::ast::lookup_or(std::move(from), std::move(path), std::move(or_)));
}
nix::ast::node attrtest(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::attrtest(std::move(lhs), std::move(rhs)));
}
nix::ast::node attrpath(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::attrpath(std::move(lhs), std::move(rhs)));
}
nix::ast::node or_(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::or_(std::move(lhs), std::move(rhs)));
}
nix::ast::node and_(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::and_(std::move(lhs), std::move(rhs)));
}
nix::ast::node impl(nix::ast::node&& lhs, nix::ast::node&& rhs) {
    return nix::ast::node(nix::ast::impl(std::move(lhs), std::move(rhs)));
}
nix::ast::node assertion(nix::ast::node&& what, nix::ast::node&& expr) {
    return nix::ast::node(
        nix::ast::assertion(std::move(what), std::move(expr)));
}
nix::ast::node with(nix::ast::node&& what, nix::ast::node&& expr) {
    return nix::ast::node(nix::ast::with(std::move(what), std::move(expr)));
}
nix::ast::node function(nix::ast::node&& arg, nix::ast::node&& expr) {
    return nix::ast::node(nix::ast::function(std::move(arg), std::move(expr)));
}
nix::ast::node bind(nix::ast::node&& name, nix::ast::node&& value) {
    return nix::ast::node(
        nix::ast::binding_eq(std::move(name), std::move(value)));
}
template <typename... T> nix::ast::node array(T&&... values) {
    nix::ast::node vals[] = {std::move(values)...};
    std::vector<nix::ast::node> vec;
    for(auto&& i : vals) vec.emplace_back(std::move(i));
    return nix::ast::node(nix::ast::array(std::move(vec)));
}
template <typename... T> nix::ast::node table(bool recursive, T&&... binds) {
    nix::ast::node vals[] = {std::move(binds)...};
    std::vector<nix::ast::node> vec;
    for(auto&& i : vals) vec.emplace_back(std::move(i));
    return nix::ast::node(nix::ast::table(
        nix::ast::node(nix::ast::binds(std::move(vec))), recursive));
}
nix::ast::node if_then_else(nix::ast::node&& test,
                            nix::ast::node&& then_expr,
                            nix::ast::node&& else_expr) {
    return nix::ast::node(nix::ast::if_then_else(
        std::move(test), std::move(then_expr), std::move(else_expr)));
}
