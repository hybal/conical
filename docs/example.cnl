enum Option<T> {
    Some(T),
    None
}

impl<T> Option<T> {
     fn is_some(&self) -> bool{
        match self {
            Some(_) => true,
            None => false
        }
     }
     
     fn is_none(self) { 
        !self.is_some() //any value can be passed into another function using the dot operator, it does not need to be in an impl block
     }
}

struct Example { //unlike in rust tuple-like structs are not supported (so struct Example(int))
    a: i32;
    b: i32;
}



fn cycle_example() {
    mut cyc: &Cycle = new Cycle<'this>; //<'this> is a builtin lifetime that refers to the current scope, there is also `'static` for static lifetimes, and `'parent` for the scope directly above the current one.
    mut cyc2: &Cycle = new Cycle<'cyc>{&cyc}; //instead of making lifetimes template parameters that need to be passed, they are based on identifiers. For example, you can specify that you want a variable to last the length of a function by putting <'function_name> you can also create a named scope by using an identifier in front of it `id: {...}`
    //by setting cyc2's lifetime to be 'cyc it will make it so that no matter how many references cyc2 has it will be dropped when cyc is. This makes it so cycles are still managed correctly
    //even if you dont set cyc2's lifetime to be 'cyc it will still be fine since you are specefying exactly how long it will last for.
    cyc.next = &cyc2; //to assign a reference to another variable you must take a new reference. 

}



fn main() {
    printn!("Hello, World!"); //unlike in rust any macro can be used procedurally or like a function, since macros are just code that take in a token stream.
}





