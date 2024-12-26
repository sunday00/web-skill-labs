pub trait Draw {
    fn draw(&self);
}

pub struct Screen<T: Draw> {
    // pub components: Vec<Box<dyn Draw>>,
    pub components: Vec<T>,
}

// impl Screen {
//     pub fn run(&self) {
//         for component in self.components.iter() {
//             component.draw();
//         }
//     }
// }
impl<T> Screen<T>
where
    T: Draw,
{
    pub fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}

pub fn exec() {}
