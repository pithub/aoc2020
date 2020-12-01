use roc_std::{ RocCallResult, RocList };

extern "C" {
    #[link_name = "roc__aocMain_1_exposed"]
    fn aocMain(output: &mut RocCallResult<RocList<RocList<i64>>>) -> ();
}

#[no_mangle]
pub fn rust_main() -> isize {
    let answer = unsafe {
        use std::mem::MaybeUninit;
        let mut output = MaybeUninit::uninit();

        aocMain(&mut *output.as_mut_ptr());

        match output.assume_init().into() {
            Ok(value) => value,
            Err(msg) => panic!("roc failed with message {}", msg),
        }
    };

    println!(",--");
    for sub_list in answer.as_slice() {
        println!("|   {:?}", &sub_list.as_slice());
    }
    println!("`--");

    // Exit code
    0
}
