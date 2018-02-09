use ordered_float;
use ast_types::FuncBody;
use std::rc::Rc;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Clone)]
pub enum LuaValue {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Function(LuaFunction),
    Table(LuaTable),
    Thread,
    UserData,
}

impl LuaValue {
    pub fn to_string(self) -> String {
        match self {
            LuaValue::Nil => format!("nil"),
            LuaValue::Boolean(b) => format!("{}", b),
            LuaValue::String(b) => b,
            LuaValue::Number(x) => format!("{}", x),
            _ => unimplemented!()
        }
    }
    pub fn to_number(self) -> f64 {
        match self {
            LuaValue::Number(n) => n,
            LuaValue::String(n) => {
                str::parse::<f64>(&n).unwrap()
            }
            _ => unimplemented!()
        }
    }
    pub fn to_bool(self) -> bool {
        match self {
            LuaValue::Nil => false,
            LuaValue::Boolean(false) => false,
            _ => true,
        }
    }
    pub fn to_table(self) -> Option<LuaTable> {
        match self {
            LuaValue::Table(lt) => Some(lt),
            _ => None
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum LuaIndexValue {
    Boolean(bool),
    Number(ordered_float::NotNaN<f64>),
    String(String),
    Function(LuaFunction),
    Table(LuaTable),
    Thread,
    UserData,
}

impl LuaIndexValue {
    fn from(lv: LuaValue) -> Result<Self, String> {
        match lv {
            LuaValue::Nil => Err(format!("Table index is nil")),
            LuaValue::Boolean(t) => Ok(LuaIndexValue::Boolean(t)),
            LuaValue::Number(n) => {
                if n.is_nan() {
                    Err(format!("Table index is NaN"))
                } else {
                    Ok(LuaIndexValue::Number(ordered_float::NotNaN::from(n)))
                }
            },
            LuaValue::String(s) => Ok(LuaIndexValue::String(s)),
            LuaValue::Function(lf) => Ok(LuaIndexValue::Function(lf)),
            LuaValue::Table(lt) => Ok(LuaIndexValue::Table(lt)),
            LuaValue::Thread => Ok(LuaIndexValue::Thread),
            LuaValue::UserData => Ok(LuaIndexValue::UserData),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct LuaTable {
    map: Rc<RefCell<HashMap<LuaIndexValue, LuaValue>>>
}

impl Hash for LuaTable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.map.as_ptr().hash(state);
    }
}

impl PartialEq for LuaTable {
    fn eq(&self, other: &LuaTable) -> bool {
        self.map.as_ptr() == other.map.as_ptr() 
    }
}

impl Eq for LuaTable {}

impl LuaTable {
    pub fn borrow(&self) -> Ref<HashMap<LuaIndexValue, LuaValue>> {
        self.map.borrow()
    }
    pub fn borrow_mut(&self) -> RefMut<HashMap<LuaIndexValue, LuaValue>> {
        self.map.borrow_mut()
    }
    pub fn insert_value(&self, index: LuaValue, value: LuaValue) -> Option<InterpreterResult> {
        let mut table = self.borrow_mut();
        let index = match LuaIndexValue::from(index) {
            Ok(v) => v,
            Err(e) => return Some(InterpreterResult::Error(e)),
        };
        table.insert(index, value);
        None
    }
    pub fn get_value(&self, index: LuaValue) -> Option<InterpreterResult> {
        let table = self.borrow();
        let index = match LuaIndexValue::from(index) {
            Ok(v) => v,
            Err(e) => return Some(InterpreterResult::Error(e)),
        };
        Some(InterpreterResult::LuaValue(table[&index].clone()))
    }
    pub fn get_value_by_name(&self, index: String) -> Option<InterpreterResult> {
        self.get_value(LuaValue::String(index))
    }
}

#[derive(Clone, Debug)]
pub struct LuaFunction {
    body: Rc<RefCell<FuncBody>>
}

impl Hash for LuaFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.body.as_ptr().hash(state);
    }
}

impl PartialEq for LuaFunction {
    fn eq(&self, other: &LuaFunction) -> bool {
        self.body.as_ptr() == other.body.as_ptr() 
    }
}

impl Eq for LuaFunction {}

#[derive(Debug)]
pub enum InterpreterResult {
    LuaValue(LuaValue),
    Error(String)
}

impl InterpreterResult {
    pub fn get_lua_value(self) -> LuaValue {
        if let InterpreterResult::LuaValue(lv) = self {
            lv
        } else {
            panic!("Expected LuaValue, got something else.")
        }
    }
}

macro_rules! try_ir {
    ($x:expr) => {
        match $x {
            Some(InterpreterResult::Error(s)) => return Some(InterpreterResult::Error(s)),
            other => other,
        }
    };
}
