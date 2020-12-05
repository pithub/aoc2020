#![allow(non_snake_case)]

use roc_std::{alloca, RocCallResult, RocList, RocStr};
use std::alloc::Layout;
use std::fs::read;

extern "C" {
    #[link_name = "roc__aocMain_1_exposed"]
    fn aoc_main(output: *mut u8) -> ();

    #[link_name = "roc__aocMain_1_size"]
    fn aoc_main_size() -> i64;

    #[link_name = "roc__aocMain_1_Fx_caller"]
    fn call_Fx(function_pointer: *const u8, closure_data: *const u8, output: *mut u8) -> ();

    #[link_name = "roc__aocMain_1_Fx_size"]
    fn size_Fx() -> i64;
}

#[no_mangle]
pub fn roc_fx_readFile(name: RocStr) -> RocList<i64> {
    let name_bytes = name.as_slice();
    let name_string = unsafe { std::str::from_utf8_unchecked(name_bytes) };

    let file_bytes =
        match read(name_string) {
            Ok(b) => b,
            Err(_) => Vec::new(),
        };

    let ints: Vec<i64> = file_bytes.iter().map(byteToInt).collect();
    RocList::from_slice(&ints)
}

#[no_mangle]
pub fn roc_fx_writeData(data: RocList<RocList<i64>>) -> () {
    for sub_list in data.as_slice() {
        println!("|   {:?}", &sub_list.as_slice());
    }
    ()
}

fn byteToInt(byte: &u8) -> i64 {
    i64::from(*byte)
}

unsafe fn call_the_closure(function_pointer: *const u8, closure_data_ptr: *const u8) -> i64 {
    let size = size_Fx() as usize;

    alloca::with_stack_bytes(size, |buffer| {
        let buffer: *mut std::ffi::c_void = buffer;
        let buffer: *mut u8 = buffer as *mut u8;

        call_Fx(
            function_pointer,
            closure_data_ptr as *const u8,
            buffer as *mut u8,
        );

        let output = &*(buffer as *mut RocCallResult<i64>);

        match output.into() {
            Ok(_) => 0,
            Err(e) => panic!("Closure failed with {}", e),
        }
    })
}

#[no_mangle]
pub fn rust_main() -> isize {
    println!(",--");

    let size = unsafe { aoc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    unsafe {
        let buffer = std::alloc::alloc(layout);

        aoc_main(buffer);

        let output = &*(buffer as *mut RocCallResult<()>);

        match output.into() {
            Ok(()) => {
                let function_pointer = {
                    // this is a pointer to the location where the function pointer is stored
                    // we pass just the function pointer
                    let temp = buffer.offset(8) as *const i64;

                    (*temp) as *const u8
                };

                let closure_data_ptr = buffer.offset(16);

                call_the_closure(function_pointer as *const u8, closure_data_ptr as *const u8);

                std::alloc::dealloc(buffer, layout);
            }
            Err(msg) => {
                std::alloc::dealloc(buffer, layout);

                panic!("Roc failed with message: {}", msg);
            }
        }
    };

    println!("`--\n");

    // Exit code
    0
}
