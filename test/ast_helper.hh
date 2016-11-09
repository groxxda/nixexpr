#pragma once

#include "ast.hh"
#include <memory>

using namespace std::literals;

std::unique_ptr<nix::ast::base> boolean(bool v) {
    return std::make_unique<nix::ast::boolean>(v);
}
std::unique_ptr<nix::ast::base> operator""_b(unsigned long long v) {
    return std::make_unique<nix::ast::boolean>(v);
}
std::unique_ptr<nix::ast::base> string(std::string&& v) {
    std::vector<std::unique_ptr<nix::ast::base>> vals;
    vals.push_back(std::make_unique<nix::ast::string_literal>(std::move(v)));

    return std::make_unique<nix::ast::short_string>(std::move(vals));
}
std::unique_ptr<nix::ast::base> long_string(std::string&& v,
                                            unsigned long len) {
    std::vector<std::unique_ptr<nix::ast::base>> vals;
    vals.push_back(std::make_unique<nix::ast::string_literal>(std::move(v)));

    return std::make_unique<nix::ast::long_string>(std::move(vals),

                                                   len);
}
std::unique_ptr<nix::ast::base> operator"" _s(const char* v, size_t len) {
    return string(std::string(v, len));
}
std::unique_ptr<nix::ast::base> number(unsigned long long v) {
    return std::make_unique<nix::ast::number>(v);
}
std::unique_ptr<nix::ast::base> operator"" _n(unsigned long long v) {
    return number(v);
}
std::unique_ptr<nix::ast::base> name(std::string&& v) {
    return std::make_unique<nix::ast::name>(std::move(v));
}
std::unique_ptr<nix::ast::base> operator"" _n(const char* v, size_t len) {
    return name(std::string(v, len));
}
std::unique_ptr<nix::ast::base> not_(std::unique_ptr<nix::ast::base>&& v) {
    return std::make_unique<nix::ast::not_>(std::move(v));
}
std::unique_ptr<nix::ast::base> negate(std::unique_ptr<nix::ast::base>&& v) {
    return std::make_unique<nix::ast::negate>(std::move(v));
}
std::unique_ptr<nix::ast::base> eq(std::unique_ptr<nix::ast::base>&& lhs,
                                   std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::eq>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> neq(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::neq>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> gt(std::unique_ptr<nix::ast::base>&& lhs,
                                   std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::gt>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> geq(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::geq>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> lt(std::unique_ptr<nix::ast::base>&& lhs,
                                   std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::lt>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> leq(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::leq>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> add(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::add>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> sub(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::sub>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> mul(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::mul>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> div(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::div>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> concat(std::unique_ptr<nix::ast::base>&& lhs,
                                       std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::concat>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> merge(std::unique_ptr<nix::ast::base>&& lhs,
                                      std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::merge>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> call(std::unique_ptr<nix::ast::base>&& lhs,
                                     std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::call>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base>
lookup(std::unique_ptr<nix::ast::base>&& from,
       std::unique_ptr<nix::ast::base>&& path,
       std::unique_ptr<nix::ast::base>&& or_ = NULL) {
    return std::make_unique<nix::ast::lookup>(std::move(from), std::move(path),
                                              std::move(or_));
}
std::unique_ptr<nix::ast::base>
attrtest(std::unique_ptr<nix::ast::base>&& lhs,
         std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::attrtest>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base>
attrpath(std::unique_ptr<nix::ast::base>&& lhs,
         std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::attrpath>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> or_(std::unique_ptr<nix::ast::base>&& lhs,
                                    std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::or_>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> and_(std::unique_ptr<nix::ast::base>&& lhs,
                                     std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::and_>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base> impl(std::unique_ptr<nix::ast::base>&& lhs,
                                     std::unique_ptr<nix::ast::base>&& rhs) {
    return std::make_unique<nix::ast::impl>(std::move(lhs), std::move(rhs));
}
std::unique_ptr<nix::ast::base>
assertion(std::unique_ptr<nix::ast::base>&& what,
          std::unique_ptr<nix::ast::base>&& expr) {
    return std::make_unique<nix::ast::assertion>(std::move(what),
                                                 std::move(expr));
}
std::unique_ptr<nix::ast::base> with(std::unique_ptr<nix::ast::base>&& what,
                                     std::unique_ptr<nix::ast::base>&& expr) {
    return std::make_unique<nix::ast::with>(std::move(what), std::move(expr));
}
std::unique_ptr<nix::ast::base>
function(std::unique_ptr<nix::ast::base>&& arg,
         std::unique_ptr<nix::ast::base>&& expr) {
    return std::make_unique<nix::ast::function>(std::move(arg),
                                                std::move(expr));
}
std::unique_ptr<nix::ast::base> bind(std::unique_ptr<nix::ast::base>&& name,
                                     std::unique_ptr<nix::ast::base>&& value) {
    return std::make_unique<nix::ast::binding_eq>(std::move(name),
                                                  std::move(value));
}
template <typename... T> std::unique_ptr<nix::ast::base> array(T&&... values) {
    std::unique_ptr<nix::ast::base> vals[] = {std::move(values)...};
    std::vector<std::unique_ptr<nix::ast::base>> vec;
    for(auto&& i : vals) vec.push_back(std::move(i));
    return std::make_unique<nix::ast::array>(std::move(vec));
}
template <typename... T>
std::unique_ptr<nix::ast::base> table(bool recursive, T&&... binds) {
    std::unique_ptr<nix::ast::base> vals[] = {std::move(binds)...};
    std::vector<std::unique_ptr<nix::ast::base>> vec;
    for(auto&& i : vals) vec.push_back(std::move(i));
    return std::make_unique<nix::ast::table>(
        std::make_unique<nix::ast::binds>(std::move(vec)), recursive);
}
std::unique_ptr<nix::ast::base>
if_then_else(std::unique_ptr<nix::ast::base>&& test,
             std::unique_ptr<nix::ast::base>&& then_expr,
             std::unique_ptr<nix::ast::base>&& else_expr) {
    return std::make_unique<nix::ast::if_then_else>(
        std::move(test), std::move(then_expr), std::move(else_expr));
}
