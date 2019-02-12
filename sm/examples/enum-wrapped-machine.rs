extern crate sm;
use sm::sm;

type U8 = u8;
type Led = u32;
sm! {
    Lock {
        GuardResources {
            {a: U8} }
        ActionResouces {
            {led: Led} }
        InitialStates { Locked, Unlocked }

        Coin { Locked => Unlocked }
        Push { Unlocked => Locked }
    }
    Lock2 {
        GuardResources {
            {a: U8} }
        ActionResouces {
            {led: Led} }
        InitialStates { Locked, Unlocked }

        Coin { Locked => Unlocked }
        Push { Unlocked => Locked }
    }
}

fn main() {
    use Lock::*;

    let mut fsm = Machine::new(Locked).as_enum();
    let result = fsm.eval_machine(0, 0);
    match result {
        Ok(_) => println!("Ok"),
        Err(_) => println!("Err"),
    }
}

impl Lock::ValidEvent for Lock::Coin {
    fn is_enabled(_a: U8) -> bool {
        true
    }
    fn action(led: Led) {
        println!("{}", led);
    }
}

impl Lock::ValidEvent for Lock::Push {
    fn is_enabled(_a: U8) -> bool {
        true
    }
    fn action(led: Led){
        println!("{}", led);
    }
}

impl Lock2::ValidEvent for Lock2::Coin {
    fn is_enabled(_a: U8) -> bool {
        true
    }
    fn action(led: Led) {
        println!("{}", led);
    }
}

impl Lock2::ValidEvent for Lock2::Push {
    fn is_enabled(_a: U8) -> bool {
        true
    }
    fn action(led: Led){
        println!("{}", led);
    }
}
