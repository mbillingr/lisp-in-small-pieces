mod error;
mod memory;
mod op_impl;
#[cfg(test)]
mod op_tests;
mod opcode;
mod scheme_object_file;
mod types;
mod vm;

use bdwgc_alloc::Allocator;
use error::Result;
use opcode::Op;
use scheme_object_file::SchemeObjectFile;
use vm::VirtualMachine;

#[cfg(not(feature = "disable-global-gc"))]
#[global_allocator]
static GLOBAL_ALLOCATOR: Allocator = Allocator;

// Note that parallel tests do not work in combination with the allocator
// because it is not practically possible to call Allocator::initialize()
// in the main thread of the test harness and register the test threads
// with GC.
// Workarounds:
//    a) run tests without GC (cargo test --no-default-features --features=disable-global-gc)
//    b) run tests serially (cargo test -- --test-threads=1)

fn main() -> Result<()> {
    unsafe { Allocator::initialize() }

    let sco = SchemeObjectFile::from_file("../test2.sco")?;

    println!("{:#?}", sco);

    let mut vm = VirtualMachine::from_sco(sco);

    unsafe {
        println!("Result: {}", vm.run());
    }

    unsafe {
        println!("Result: {}", vm.run());
    }

    let mut counts: Vec<_> = vm
        .statistics
        .iter()
        .enumerate()
        .map(|(i, n)| (*n, Op::from_u8(i as u8)))
        .collect();
    counts.sort_by_key(|(n, _)| *n);

    for c in counts.iter().rev().take(10) {
        println!("{:?}", c);
    }

    println!("max stack: {}", vm.max_stack);

    Ok(())
}
