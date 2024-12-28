use std::slice;

pub fn exec() {
    unsafe {
        dangerous();
    }
}

unsafe fn dangerous() {}

pub fn exec2() {
    let mut v = vec![1, 2, 3, 4, 5, 6];

    let r = &mut v[..];

    // let (a, b) = r.split_at_mut(3);
    let (a, b) = split_at_mut(r, 3);

    assert_eq!(a, &mut [1, 2, 3]);
    assert_eq!(b, &mut [4, 5, 6]);
}

fn split_at_mut(values: &mut [i32], mid: usize) -> (&mut [i32], &mut [i32]) {
    let len = values.len();

    assert!(mid <= len);

    // (&mut values[..mid], &mut values[mid..])
    let ptr = values.as_mut_ptr();
    unsafe {
        (
            slice::from_raw_parts_mut(ptr, mid),
            slice::from_raw_parts_mut(ptr.add(mid), len - mid),
        )
    }
}
