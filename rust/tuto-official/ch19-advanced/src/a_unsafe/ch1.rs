use std::slice;

pub fn exec() {
    let mut num = 5;
    let r1 = &num as *const i32;
    let r2 = &mut num as *mut i32;

    unsafe {
        println!("{:?} {:?}", *r1, *r2);
    }

    let address = 0x012345usize;
    // let r = address as *const i32;
    let r = address as *mut i32;

    let values: &[i32] = unsafe { slice::from_raw_parts_mut(r, 10000) };
    // println!("{:?}", r);
    println!("{:?}", values);
}
