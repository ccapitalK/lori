#[macro_use]
mod types;
mod garbage_collector;

use self::garbage_collector::{GarbageCollector, Gc};
use self::types::*;
use crate::ast_types::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Interpreter {
    scopes: Vec<HashMap<String, LuaValue>>,
    gc: GarbageCollector,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut scopes = Vec::new();
        scopes.push(HashMap::new());
        let gc = GarbageCollector::new();
        Interpreter {
            scopes: scopes,
            gc: gc,
        }
    }
    fn set_local(&mut self, var_name: &str, value: LuaValue) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(var_name.to_string(), value);
    }
    fn set_variable(&mut self, var_name: &str, value: LuaValue) {
        self.scopes[0].insert(var_name.to_string(), value);
    }
    fn get_variable(&self, var_name: &str) -> LuaValue {
        for table in self.scopes.iter().rev() {
            if let Some(v) = table.get(var_name) {
                return v.clone();
            }
        }
        Default::default()
    }
}

impl ASTVisitor<InterpreterResult> for Interpreter {
    fn visit_table_constructor(&mut self, tc: &mut TableConstructor) -> Option<InterpreterResult> {
        let rv = LuaTableRef::default();
        let mut index_count = 1.0f64;
        {
            for field in tc.0.iter_mut() {
                match field {
                    &mut Field::Exp(ref mut e) => {
                        let value = try_ir!(self.visit_exp(e)).unwrap().get_lua_value();
                        try_ir!(rv.insert_value(LuaValue::Number(index_count), value));
                        index_count += 1.0f64;
                    }
                    &mut Field::NamedExp(ref name, ref mut value) => {
                        let value = try_ir!(self.visit_exp(value)).unwrap().get_lua_value();
                        try_ir!(rv.insert_value(LuaValue::String(name.clone()), value));
                    }
                    &mut Field::IndexExp(ref mut index, ref mut value) => {
                        let index = try_ir!(self.visit_exp(index)).unwrap().get_lua_value();
                        let value = try_ir!(self.visit_exp(value)).unwrap().get_lua_value();
                        try_ir!(rv.insert_value(index, value));
                    }
                }
            }
        }
        Some(InterpreterResult::LuaValue(LuaValue::Table(rv)))
    }
    fn visit_prefix_exp(&mut self, pe: &mut PrefixExp) -> Option<InterpreterResult> {
        unimplemented!()
    }
    fn visit_simple_exp(&mut self, se: &mut SimpleExp) -> Option<InterpreterResult> {
        Some(InterpreterResult::LuaValue(match se {
            &mut SimpleExp::Nil => LuaValue::Nil,
            &mut SimpleExp::False => LuaValue::Boolean(false),
            &mut SimpleExp::True => LuaValue::Boolean(true),
            &mut SimpleExp::Number(ref val) => LuaValue::Number(*val),
            &mut SimpleExp::StringLiteral(ref val) => LuaValue::String(val.clone()),
            &mut SimpleExp::Elipsis => LuaValue::Nil,
            &mut SimpleExp::PrefixExp(ref mut pe) => return self.visit_prefix_exp(pe),
            &mut SimpleExp::TableConstructor(ref mut tc) => {
                return self.visit_table_constructor(tc)
            }
        }))
    }
    fn visit_exp(&mut self, exp: &mut Exp) -> Option<InterpreterResult> {
        match exp {
            &mut Exp::SimpleExp(ref mut se) => self.visit_simple_exp(se),
            &mut Exp::UnaryOp(UnOp::Not, ref mut e) => {
                let lv = try_ir!(self.visit_exp(e)).unwrap().get_lua_value();
                let rv = LuaValue::Boolean(!(lv.to_bool()));
                Some(InterpreterResult::LuaValue(rv))
            }
            &mut Exp::UnaryOp(UnOp::Neg, ref mut e) => {
                let lv = try_ir!(self.visit_exp(e)).unwrap().get_lua_value();
                let rv = LuaValue::Number(-try_ir!(res lv.to_number()));
                Some(InterpreterResult::LuaValue(rv))
            }
            &mut Exp::UnaryOp(UnOp::Len, ref mut e) => {
                match try_ir!(self.visit_exp(e)).unwrap().get_lua_value() {
                    LuaValue::String(se) => Some(InterpreterResult::LuaValue(LuaValue::Number(
                        se.len() as f64,
                    ))),
                    _ => panic_ir!("Tried to call len on non string value"),
                }
            }
            &mut Exp::BinaryOp(ref mut e1, op, ref mut e2) => {
                let lv1 = try_ir!(self.visit_exp(e1)).unwrap().get_lua_value();
                let lv2 = try_ir!(self.visit_exp(e2)).unwrap().get_lua_value();
                let rv = match op {
                    BinOp::Plus => LuaValue::Number(
                        try_ir!(res lv1.to_number()) + try_ir!(res lv2.to_number()),
                    ),
                    BinOp::Minus => LuaValue::Number(
                        try_ir!(res lv1.to_number()) - try_ir!(res lv2.to_number()),
                    ),
                    BinOp::Mult => LuaValue::Number(
                        try_ir!(res lv1.to_number()) * try_ir!(res lv2.to_number()),
                    ),
                    BinOp::Div => LuaValue::Number(
                        try_ir!(res lv1.to_number()) / try_ir!(res lv2.to_number()),
                    ),
                    BinOp::Pow => LuaValue::Number(
                        try_ir!(res lv1.to_number()).powf(try_ir!(res lv2.to_number())),
                    ),
                    BinOp::Concat => LuaValue::String(format!(
                        "{}{}",
                        try_ir!(res lv1.to_string()),
                        try_ir!(res lv2.to_string())
                    )),
                    BinOp::LessThan => LuaValue::Boolean(
                        try_ir!(res lv1.to_number()) < try_ir!(res lv2.to_number()),
                    ),
                    BinOp::LessEqual => LuaValue::Boolean(
                        try_ir!(res lv1.to_number()) <= try_ir!(res lv2.to_number()),
                    ),
                    BinOp::GreaterThan => LuaValue::Boolean(
                        try_ir!(res lv1.to_number()) > try_ir!(res lv2.to_number()),
                    ),
                    BinOp::GreaterEqual => LuaValue::Boolean(
                        try_ir!(res lv1.to_number()) >= try_ir!(res lv2.to_number()),
                    ),
                    BinOp::Equals => LuaValue::Boolean(lv1 == lv2),
                    BinOp::NotEquals => LuaValue::Boolean(lv1 != lv2),
                    BinOp::And => LuaValue::Boolean(lv1.to_bool() && lv2.to_bool()),
                    BinOp::Or => LuaValue::Boolean(lv1.to_bool() || lv2.to_bool()),
                    BinOp::Mod => LuaValue::Number(
                        try_ir!(res lv1.to_number()) % try_ir!(res lv2.to_number()),
                    ),
                };
                Some(InterpreterResult::LuaValue(rv))
            }
        }
    }
    fn visit_stat(&mut self, st: &mut Stat) -> Option<InterpreterResult> {
        match st {
            &mut Stat::LocalAssign(ref name_list, ref mut exp_list) => {
                let mut res_list = Vec::new();
                for mut exp in exp_list.0.iter_mut() {
                    res_list.push(try_ir!(self.visit_exp(&mut exp)).unwrap().get_lua_value());
                }
                res_list.resize(name_list.0.len(), LuaValue::Nil);
                for (name, value) in name_list.0.iter().zip(res_list.into_iter()) {
                    self.set_local(name, value);
                }
                None
            }
            &mut Stat::Assign(ref mut var_list, ref mut exp_list) => {
                let mut res_list = Vec::new();
                for mut exp in exp_list.0.iter_mut() {
                    res_list.push(try_ir!(self.visit_exp(&mut exp)).unwrap().get_lua_value());
                }
                res_list.resize(var_list.0.len(), LuaValue::Nil);
                let table_vs_insert = |interpreter: &mut Interpreter,
                                       lt: LuaTableRef,
                                       vss: &mut [VarSuffix],
                                       value: LuaValue| {
                    let index_exp = match vss[0] {
                        _ => unimplemented!(),
                    };
                    if vss.len() == 1 {
                    } else {
                    }
                    None
                };
                for (var, value) in var_list.0.iter_mut().zip(res_list.into_iter()) {
                    match var {
                        &mut Var::Name(ref name, ref mut vs) => {
                            if vs.len() == 0 {
                                self.set_local(name, value);
                            } else {
                                let table = try_ir!(res self.get_variable(&name).to_table());
                                try_ir!(table_vs_insert(self, table, &mut vs[..], value));
                            }
                        }
                        &mut Var::Exp(ref mut exp, ref mut vs) => {
                            let table = try_ir!(res try_ir!(self.visit_exp(exp)).unwrap().get_lua_value().to_table());
                            try_ir!(table_vs_insert(self, table, &mut vs[..], value));
                        }
                    }
                }
                None
            }
            //TODO: Implement all the other expressions
            _ => unimplemented!(),
        }
    }
    fn visit_chunk(&mut self, ch: &mut Chunk) -> Option<InterpreterResult> {
        for s in ch.0.iter_mut() {
            try_ir!(self.visit_stat(s));
        }
        match ch.1 {
            Some(ref mut v) => self.visit_last_stat(v),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use ast_types::ASTVisitor;
    use interpreter::{Interpreter, LuaValue};
    use parse;

    #[test]
    fn test_local_assign() {
        let mut input =
            parse::parse_lua_source(b"local a = 4 local b, c = 5, 6 local d, e = 7 local f = 8, 9")
                .unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.visit_chunk(&mut input);
        assert_eq!(interpreter.get_variable("a"), LuaValue::Number(4.0));
        assert_eq!(interpreter.get_variable("b"), LuaValue::Number(5.0));
        assert_eq!(interpreter.get_variable("c"), LuaValue::Number(6.0));
        assert_eq!(interpreter.get_variable("d"), LuaValue::Number(7.0));
        assert_eq!(interpreter.get_variable("e"), LuaValue::Nil);
        assert_eq!(interpreter.get_variable("f"), LuaValue::Number(8.0));
    }
    #[test]
    fn test_assign() {
        let mut input = parse::parse_lua_source(
            b"a = 4
              b = {}
              b.a = 4",
        )
        .unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.visit_chunk(&mut input);
        assert_eq!(interpreter.get_variable("a"), LuaValue::Number(4.0));
        assert_eq!(
            interpreter
                .get_variable("b")
                .to_table()
                .unwrap()
                .get_value_by_name(format!("a"))
                .unwrap()
                .get_lua_value(),
            LuaValue::Number(4.0)
        );
    }

    #[test]
    fn test_eval_exp() {
        let mut interpreter = Interpreter::new();
        let mut input = parse::parse_lua_source(
            b"local a, b, c = nil, false, true
              local d, e = 2, [[Hello]]
              local f, g, h, i = not nil, not false, not true, not 5
              local j, k = # [[hello]], -4.0
              local l, m, n, o = 1 + 1, 2 - 1, 2 * 2, 7 / 2
              local p, q, r, s = 'h'..'i', '4' ^ 4, true and false, true or false
              local t = {a = false}\n",
        )
        .unwrap();
        assert_eq!(interpreter.get_variable("a"), LuaValue::Nil);
        assert_eq!(interpreter.get_variable("b"), LuaValue::Boolean(false));
        assert_eq!(interpreter.get_variable("c"), LuaValue::Boolean(true));
        assert_eq!(interpreter.get_variable("d"), LuaValue::Number(2.0));
        assert_eq!(
            interpreter.get_variable("e"),
            LuaValue::String(format!("Hello"))
        );
        assert_eq!(interpreter.get_variable("f"), LuaValue::Boolean(true));
        assert_eq!(interpreter.get_variable("g"), LuaValue::Boolean(true));
        assert_eq!(interpreter.get_variable("h"), LuaValue::Boolean(false));
        assert_eq!(interpreter.get_variable("i"), LuaValue::Boolean(false));
        assert_eq!(interpreter.get_variable("j"), LuaValue::Number(5.0));
        assert_eq!(interpreter.get_variable("k"), LuaValue::Number(-4.0));
        assert_eq!(interpreter.get_variable("l"), LuaValue::Number(2.0));
        assert_eq!(interpreter.get_variable("m"), LuaValue::Number(1.0));
        assert_eq!(interpreter.get_variable("n"), LuaValue::Number(4.0));
        assert_eq!(interpreter.get_variable("o"), LuaValue::Number(3.5));
        assert_eq!(
            interpreter.get_variable("p"),
            LuaValue::String(format!("hi"))
        );
        assert_eq!(interpreter.get_variable("q"), LuaValue::Number(256.0));
        assert_eq!(interpreter.get_variable("r"), LuaValue::Boolean(false));
        assert_eq!(interpreter.get_variable("s"), LuaValue::Boolean(true));
        assert_eq!(
            interpreter
                .get_variable("t")
                .to_table()
                .unwrap()
                .get_value_by_name(format!("a"))
                .unwrap()
                .get_lua_value(),
            LuaValue::Number(4.0)
        );
    }
}
