#![allow(dead_code)]

error_chain!{
    foreign_links {
    }

    links {
        Jit(::jit::errors::Error, ::jit::errors::ErrorKind);
    }

    errors {
        UnexpectedToken(msg: String, token: ::lexer::Token) {
            description("unexpected token")
            display("{}, but got unexpected token: '{:?}'", msg, token)
        }
        UnknownVariable(name: String) {
            description("unknown variable")
            display("unknown variable: {}", name)
        }
        UnknownFunction(name: String) {
            description("unknown function")
            display("unknown function: {}", name)
        }
        IncorrectArguments(passed: usize, expected: usize) {
            description("incorrect arguments passed")
            display("incorrect arguments, passed {}, expected {}", passed, expected)
        }
    }
}
