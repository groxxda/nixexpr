#pragma once

#include <pegtl.hh>
#include <pegtl/trace.hh>
#include <pegtl/contrib/changes.hh>
#include <pegtl/contrib/unescape.hh>

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <iostream>


namespace nix {
using namespace pegtl;


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
        explicit number(const long double in_data) : base(), data(in_data) {}
        long double data;
        virtual void stream(std::ostream& o) const override { o << data; }
    };

    struct string : public base {
        explicit string(const std::string& in_data) : base(), data(in_data) {}
        std::string data;
        virtual void stream(std::ostream& o) const override { o << data; }
    };

    struct attr_path : public base {
        explicit attr_path(const std::string& in_data) : base(), data(in_data) {}
        std::string data;
        virtual void stream(std::ostream& o) const override { o << data; }
    };

    struct array : public base {
        array() : base() {}
        std::vector<std::shared_ptr<base>> data;
        virtual void stream(std::ostream& o) const override {
            o << "[ ";
            for (auto i = data.begin(); i != data.end(); ++i) o << *i << " ";
            o << "]";
        }
    };

    struct object : public base {
        object() : base() {}
        std::map<std::shared_ptr<attr_path>, std::shared_ptr<base>> data;
        virtual void stream(std::ostream& o) const override {
            o << "{ ";
            for (auto i = data.begin(); i != data.end(); ++i) o << *i->first << " = " << *i->second << "; ";
            o << "}";
        }
    };
} // namespace ast

namespace parser {

    struct result_state {
        result_state() = default;
        result_state(const result_state&) = delete;
        void operator=(const result_state&) = delete;
        std::shared_ptr<ast::base> result;
    };

    struct array_state : public result_state {
        std::shared_ptr<ast::array> array = std::make_shared<ast::array>();
        void push_back() {
            array->data.push_back(std::move(result));
            result.reset();
        }
        void success(result_state& in_result) {
            if (this->result) push_back();
            in_result.result = array;
        }
    };

    struct object_state : public result_state {
        std::shared_ptr<ast::attr_path> attr_path;
        std::shared_ptr<ast::object> object = std::make_shared<ast::object>();
        void insert() {
            object->data.emplace(std::move(attr_path), std::move(result));
            attr_path.reset();
            result.reset();
        }
        void success(result_state& in_result) {
            if (this->result) insert();
            in_result.result = object;
        }
    };

    struct ws : one<' ', '\t', '\n', '\r'> {};
    struct char_ : utf8::any {};
    struct identifier_follow : sor<identifier_other, one<'\'','-'>> {};
    struct id : seq<identifier_first, star<identifier_follow>> {};

    template< typename R, typename P = ws > struct padr : internal::seq< R, internal::star< P > > {};


    struct number : plus<digit> {};
    struct string_content : until<at<one<'"'>>, must<char_>> {};
    struct string_ : seq<one<'"'>, must<string_content>, any> { using content = string_content; };

    struct attr_separator : one<'.'> {};
    struct attr_path : list<id, attr_separator, ws> {};


    struct array_begin : padr<one<'['>> {};
    struct array_separator : plus<ws> {};
    struct array_end : one<']'> {};

    struct object_begin : padr<one<'{'>> {};
    struct object_assign : padr<one<'='>> {};
    struct object_separator : padr<one<';'>> {};
    struct object_end : one<'}'> {};


    struct array_element;
    struct array_content : opt<list_tail<array_element, array_separator>> {};
    struct array : seq<array_begin, array_content, must<array_end>> {
        using begin = array_begin;
        using end = array_end;
        using element = array_element;
        using content = array_content;
    };


    struct object_element;
    struct object_content : star<object_element, object_separator> {};
    struct object : seq<object_begin, object_content, must<object_end>> {
        using begin = object_begin;
        using end = object_end;
        using element = object_element;
        using content = object_content;
    };


    struct value : sor<string_, number, array, object> {};
    struct value_pad : padr<value> {};

    struct array_element : seq<value> {};
    struct object_element : seq<attr_path, object_assign, value> {};


    struct expression;


    struct expression_application : number {};

    struct operator_unary : one<'!', '-'> {};
    struct expression_unary : seq<operator_unary, expression_application> {};

    struct eq : string<'=', '='> {};
    struct neq : string<'!', '='> {};
    struct less : one<'<'> {};
    struct leq : string<'<', '='> {};
    struct greater : one<'>'> {};
    struct geq : string<'>', '='> {};
    struct operator_binary : sor<eq, neq, less, leq, greater, geq> {};
    //struct expression_binary : seq<expression_operator, operator_binary, expression_operator> {};

//    struct expression_operator : sor<expression_unary, expression_binary> {};


    struct if_ : string<'i', 'f'> {};
    struct then_ : string<'t', 'h', 'e', 'n'> {};
    struct else_ : string<'e', 'l', 's', 'e'> {};
    struct if_then_else : seq<if_, expression, then_, expression, else_> {};
    //struct expression_if : sor<if_then_else, expression_operator> {};


    struct expression : sor<expression_unary, expression_application> {};


    struct grammar : must<value, eof> {};



    template<typename Rule> struct value_action : nothing<Rule> {};
    template<> struct value_action<number> {
        template<typename Input>
        static void apply(const Input& in, result_state& result) {
            result.result = std::make_shared<ast::number>(std::stold(in.string()));
        }
    };
    template<> struct value_action<string_> {
        template<typename Input>
        static void apply(const Input& in, result_state& result) {
            result.result = std::make_shared<ast::string>(in.string());
        }
    };

    template<typename Rule> struct array_action : nothing<Rule> {};
    template<> struct array_action<array_element> {
        template<typename Input>
        static void apply(const Input& in, array_state& result) {
            result.push_back();
        }
    };

    template<typename Rule> struct object_action : nothing<Rule> {};
    template<> struct object_action<attr_path> {
        template<typename Input>
        static void apply(const Input& in, object_state& result) {
            result.attr_path = std::make_shared<ast::attr_path>(in.string());
        }
    };
    template<> struct object_action<object_separator> {
        template<typename Input>
        static void apply(const Input& in, object_state& result) {
            result.insert();
        }
    };



    template< typename Rule >
    struct errors : public tracer< Rule >
    {
        static const std::string error_message;

        template< typename Input, typename ... States >
        static void raise( const Input & in, States && ... )
        {
            throw pegtl::parse_error( error_message, in );
        }
    };

    template<> const std::string errors<expression>::error_message = "expression";

    template<> const std::string errors<eof>::error_message = "unexpected character value";
    template<> const std::string errors<array_end>::error_message = "array incomplete, expected: ']'";
    template<> const std::string errors<array_element>::error_message = "(in array) expected: value";
    template<> const std::string errors<string_::content>::error_message = "string incomplete, expected: '\"'";
    template<> const std::string errors<char_>::error_message = "invalid character in string";
    template<> const std::string errors<value>::error_message = "expected: value";
    template<> const std::string errors<value_pad>::error_message = "expected: value";
    template<> const std::string errors<object_end>::error_message = "object incomplete, expected: '}'";
    template<> const std::string errors<object_element>::error_message = "object incomplete, expected: '='";




    template<typename Rule> struct control : errors<Rule> {};
    template<> struct control<value> : change_action<value, value_action, errors> {};
    //template<> struct control<string::content> : change_state<string::content, string_state, errors> {};
    template<> struct control<array::content> : change_state_and_action<array::content, array_state, array_action, errors> {};
    template<> struct control<object::content> : change_state_and_action<object::content, object_state, object_action, errors> {};



} // namespace parser

} // namespace nix
