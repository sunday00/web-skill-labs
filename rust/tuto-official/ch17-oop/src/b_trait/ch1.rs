pub trait Draw {
    fn draw(&self);
}

pub struct Screen {
// pub struct Screen<T: Draw> {
    pub components: Vec<Box<dyn Draw>>,
    // pub components: Vec<T>,
}

impl Screen {
    pub fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}
// impl<T> Screen<T>
// where
//     T: Draw,
// {
//     pub fn run(&self) {
//         for component in self.components.iter() {
//             component.draw();
//         }
//     }
// }

pub struct Button {
    pub width: u32,
    pub height: u32,
    pub label: String,
}

impl Draw for Button {
    fn draw(&self) {
        // 실제로 버튼을 그리는 코드
        println!("Drawing Button");
    }
}

struct SelectBox {
    width: u32,
    height: u32,
    options: Vec<String>,
}

impl Draw for SelectBox {
    fn draw(&self) {
        // 실제로 선택 상자를 그리는 코드
        println!("Drawing SelectBox");
    }
}

pub fn exec() {
    let screen = Screen {
        components: vec![
            Box::new(SelectBox{
                width: 75,
                height: 10,
                options: vec![
                    String::from("Yes"),
                    String::from("Maybe"),
                    String::from("No"),
                ],
            }),
            Box::new(Button{
                width: 50,
                height: 10,
                label: String::from("OK"),
            })
            // SelectBox{
            //         width: 75,
            //         height: 10,
            //         options: vec![
            //             String::from("Yes"),
            //             String::from("Maybe"),
            //             String::from("No"),
            //         ],
            // },
            // Button{
            //     width: 50,
            //     height: 10,
            //     label: String::from("OK"),
            // }
        ]
    };

    screen.run();
}
