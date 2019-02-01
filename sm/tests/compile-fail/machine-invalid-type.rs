extern crate sm;
use sm::sm;

sm!{
    Lock {
        InitialStates { Locked }

        TurnKey { Locked => Unlocked }
    }
}

fn main() {
    use Lock::*;

    let sm = Machine::new(Locked);
    while sm.state() == Locked {
        sm = sm.transition(TurnKey);
        //~^ ERROR mismatched types
    }
}
