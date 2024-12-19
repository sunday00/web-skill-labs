struct Point<T> {
    x: T,
    y: T,
}

struct DynamicPoint<T, U> {
    x: T,
    y: U,
}

impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }
}

impl Point<f32> {
    fn distance_from_origin(&self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}

impl<A, B> DynamicPoint<A, B> {
    fn mixup<M, N>(self, other: DynamicPoint<M, N>) -> DynamicPoint<A, N> {
        DynamicPoint {
            x: self.x,
            y: other.y,
        }
    }
}

pub fn exec() {
    let integer = Point { x: 5, y: 10 };
    let float = Point { x: 1.0, y: 4.0 };

    println!("{}", integer.x());
    // println!("{}", integer.distance_from_origin()); // 🚫distance_from_origin is only able to called with <f32> Point, not else.
    println!("{}", float.distance_from_origin());

    let integer = DynamicPoint { x: 5, y: 10 };
    let float = DynamicPoint { x: 1.0, y: 4.0 };
    let mixed1 = DynamicPoint { x: 1.0, y: 4 };
    let mixed2 = DynamicPoint { x: 1, y: 4.0 };

    let p1 = DynamicPoint { x: 5, y: 10.4 };
    let p2 = DynamicPoint { x: "Hello", y: 'c' };

    let p3 = p1.mixup(p2);

    println!("p3.x = {}, p3.y = {}", p3.x, p3.y);
}