# clone, copy

## clone
- deep copy value in heap.

## copy
- trait of duplicating some "STACK" value.
- you can make custom .copy() trait.
- automatic called when you use value like this.
```rust
    fn main() {
        let a = 3;
        let b = a; // copy a
    }
```

# drop
- trait of dropping value in heap.
- you can make custom .drop() trait.
- automatic called when value exiting scope.

# conclusion
- copy is used on stack.
- drop is used on heap.
- so, customizing both of .copy() and .drop() makes compiler get panic.