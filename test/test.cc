/*
 * test.cc
 *
 *  Created on: Oct 24, 2016
 *      Author: groxxda
 */
#include "catch.hpp"

#include <iostream>
#include <pegtl/analyze.hh>
#include "expr.hh"

using namespace nix::parser;
using namespace std::literals;

template <typename Str, typename... Args>
bool parse(Str&& str, Args&&... args) {
    try {
//        nix::parser::state::base result;
        //auto res = pegtl::parse_string<grammar, nix::parser::action/*nix::parser::grammar_action*//*pegtl::nothing*/, /*pegtl::normal*/nix::parser::control::normal>(std::forward<Str>(str), std::forward<Str>(str), std::forward<Args>(args)...);
        auto res = pegtl::parse_string<grammar, pegtl::nothing, /*pegtl::normal*/nix::parser::control::normal>(std::forward<Str>(str), std::forward<Str>(str), std::forward<Args>(args)...);
//        std::cout << "result: " << result.result << std::endl;
        return res;
    } catch (pegtl::parse_error& e) {
        std::cerr << "Error parsing " << str << ":" << std::endl;
        std::cerr << e.what() << std::endl;
        return false;
    } catch (...) {
        std::cerr << "unknown error parsing " << str << ":" << std::endl;
        return false;
    }
}

template<typename R>
R compare(nix::parser::state::expression& state) {
    std::stringstream s;
    s << state.value;
    R r;
    s >> r;
    return r;
}

template<>
std::string compare(nix::parser::state::expression& state) {
    std::stringstream s;
    s << state.value;
    return s.str();
}

template<typename R, typename A>
R compare_downcast(nix::parser::state::expression& state) {
    auto downcast = std::dynamic_pointer_cast<A>(state.value);
    if (!downcast) {
        auto val = state.value;
        std::stringstream msg("expected ");
        msg << typeid(A).name();
        msg << ", but got type=";
        if (val) {
            auto& val_ = *val;
            msg << typeid(val_).name();
        } else
            msg << "nullptr";
        msg << " with value=";
        msg << val;
        throw msg.str();
    }
    return downcast->data;
}

template<> int compare(nix::parser::state::expression& state) { return compare_downcast<int, nix::ast::number>(state); }

template<> bool compare(nix::parser::state::expression& state) { return compare_downcast<bool, nix::ast::boolean>(state); }

template<typename S, typename R>
void check(S&& str, const R& expect) {
    SECTION(str) {
        nix::parser::state::expression result;
        REQUIRE(parse(str, result));
        REQUIRE(compare<R>(result) == expect);
    }
}

template<typename S>
void check(S&& str) {
    check<>(str, str);
}

#if 0
TEST_CASE("grammar analysis") {
    const size_t issues_found = pegtl::analyze<grammar>();
    CHECK(issues_found == 0);
}
#endif

TEST_CASE("comments") {
    check("1#single line comment", 1);
    check("4/* multi \n line \n comment */", 4);
    check("/* multi \n line \n comment */16", 16);
}

TEST_CASE("boolean expression") {
    check("true", true);
    check("false", false);
    check("!true", false);
    check("!\ttrue", false);
    check("!(true)", false);
}

TEST_CASE("strings") {
    check("\"\""s);
    check("\"shortstring\""s);
    check("\"shortstring with \\\" escape\""s);
//    check("\"shortstring with dollarcurly ${true}\""s);
//    check("\"shortstring with dollarcurlys ${true}${true}\""s);
//    check("\"shortstring with dollarcurly ${\"with inner string\"}\""s);
//    check("\"shortstring with nested dollarcurly ${\"[outer,${\"<inner>\"}]\"}\""s);
    check("''''"s, "\"\""s);
    check("''longstring''"s, "\"longstring\""s);
//    check("''longstring with ''' escape''"s, "\"longstring with ' escape\""s);
}


/*
TEST_CASE("bla") {
    nix::parser::state::base result;
    SECTION("true") {
        REQUIRE(parse("-({a=1;}).a", result));
        std::cout << "parsed: " << result.data << std::endl;
    }
}*/





TEST_CASE("number") {
    check("1337", 1337);
    check("-1337"s, "(-1337)"s);
    check("--1337", 1337);
}

TEST_CASE("arithmetic sum") {
    nix::parser::state::expression result;
    SECTION("-23 + 42") {
        REQUIRE(parse("-23 + 42", result));
        REQUIRE(compare<std::string>(result) == "((-23)+42)");
    }
    SECTION("23 - 42") {
        REQUIRE(parse("23 - 42", result));
        REQUIRE(compare<std::string>(result) == "(23+(-42))");
    }
    SECTION("23 - -42") {
        REQUIRE(parse("23 - -42", result));
        REQUIRE(compare<std::string>(result) == "(23+42)");
    }
    SECTION("1 + 2 + 3") {
        REQUIRE(parse("1 + 2 + 3", result));
        REQUIRE(compare<std::string>(result) == "(1+2+3)");
    }
    SECTION("1 - 2 - 3") {
        REQUIRE(parse("1 - 2 - 3", result));
        REQUIRE(compare<std::string>(result) == "(1+(-2)+(-3))");
    }
}

TEST_CASE("arithmetic product") {
    nix::parser::state::expression result;
    SECTION("23 * 42") {
        REQUIRE(parse("23 * 42", result));
        REQUIRE(compare<std::string>(result) == "(23*42)");
    }
    SECTION("-23 * 42") {
        REQUIRE(parse("-23 * 42", result));
        REQUIRE(compare<std::string>(result) == "((-23)*42)");
    }
    SECTION("23 * -42") {
        REQUIRE(parse("23 * -42", result));
        REQUIRE(compare<std::string>(result) == "(23*(-42))");
    }
    SECTION("1 * 2 * 3") {
        REQUIRE(parse("1 * 2 * 3", result));
        REQUIRE(compare<std::string>(result) == "(1*2*3)");
    }
    SECTION("1 * -2 * 3") {
        REQUIRE(parse("1 * -2 * 3", result));
        REQUIRE(compare<std::string>(result) == "(1*(-2)*3)");
    }
    SECTION("1 / -2") {
        REQUIRE(parse("1 / -2", result));
        REQUIRE(compare<std::string>(result) == "(1*(1/(-2)))");
    }
}

TEST_CASE("arithmetic mixed") {
    nix::parser::state::expression result;
    SECTION("1 * 2 + 1") {
        REQUIRE(parse("1 * 2 + 1", result));
        REQUIRE(compare<std::string>(result) == "((1*2)+1)");
    }
    SECTION("1 + 2 * 1") {
        REQUIRE(parse("1 + 2 * 1", result));
        REQUIRE(compare<std::string>(result) == "(1+(2*1))");
    }
}


TEST_CASE("table") {
    check("{ }"s);
    check("rec { }"s);
    check("{ a = 1; }"s);
    check("rec { a = 1; }"s);
    check("{ a = 1; b = \"c\"; }"s);
    check("{ a = 1; b = \"c\"; c = foobar; }"s);
    check("{ inherit a; }"s, "{ a = a; }"s);
    check("{ inherit a b; }"s, "{ a = a; b = b; }"s);
    check("{ inherit (a) b; }"s, "{ b = a.b; }"s);
    check("{ inherit (a) b c; }"s, "{ b = a.b; c = a.c; }"s);
    check("{ inherit (a) b; inherit c; inherit (d) e; }"s, "{ b = a.b; c = c; e = d.e; }"s);
}

TEST_CASE("table merge") {
    //check("{ } // {}"s);
}


TEST_CASE("array") {
    check("[]"s, "[ ]"s);
    check("[ ]"s);
    check("[ 1 ]"s);
    check("[ 1 \"b\" ]"s);
    check("[ 1 \"b\" c ]"s);
}

TEST_CASE("array merge") {
    //check("[] ++ []"));
}

//TEST_CASE("parameter") {
//    CHECK(parse("a: 1"));
//    CHECK(parse("a: b: 1"));
//}
//
//TEST_CASE("parameter list") {
//    CHECK(parse("{ }: 1"));
//    CHECK(parse("{ a }: 1"));
//    CHECK(parse("{ a, b }: 1"));
//    CHECK(parse("{ ... }: 1"));
//    CHECK(parse("{ a, ... }: 1"));
//    CHECK(parse("{ a, b, ... }: 1"));
//}
//
//TEST_CASE("assert") {
//    CHECK(parse("assert true; 1"));
//    CHECK(parse("assert 1; 1"));
//    CHECK(parse("assert true;assert false ; 1"));
//}
//
//TEST_CASE("with") {
//    CHECK(parse("with x; 1"));
//}
//
//TEST_CASE("let in") {
//    CHECK(parse("let x=1; in 1"));
//    CHECK(parse("let x = 1; in 1"));
//    CHECK(parse("let x = 1; y=2; in 1"));
//    CHECK(parse("let inherit x; in 1"));
//    CHECK(parse("let inherit(x) y; in 1"));
//    CHECK(parse("let inherit (x) y z; in 1"));
//}
//
//TEST_CASE("if then else") {
//    CHECK(parse("if true then \"yes\" else \"false\""));
//    CHECK(parse("if (1) then (\"yes\") else (\"false\")"));
//}
//
//
//TEST_CASE("complex") {
//
//    CHECK(parse("let requiredVersion = import ./lib/minver.nix; in\n"
//            "if ! builtins ? nixVersion || builtins.compareVersions requiredVersion builtins.nixVersion == 1 then \n"
//            "  abort \"This version of Nixpkgs requires Nix >= ${requiredVersion}, please upgrade! See https://nixos.org/wiki/How_to_update_when_Nix_is_too_old_to_evaluate_Nixpkgs\"\n"
//            "else\n"
//            "  import ./pkgs/top-level/impure.nix"));
//}



