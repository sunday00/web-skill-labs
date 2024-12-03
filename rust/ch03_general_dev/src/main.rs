fn main() {
    {
        let mut x = 5;
        println!("The value of x is: {x}");
        x = 6;
        println!("The value of x is: {x}");

        const THREE_HOURS_IN_SECONDS: u32 = 60 * 60 * 3;
        println!("The value of THREE_HOURS_IN_SECONDS is: {THREE_HOURS_IN_SECONDS}");

        let x = x * 2;
        println!("The value of x is: {x}");

        {
            let x = x + 1;
            println!("The value of x is: {x}");
        }

        println!("The value of x is: {x}");

        let spaces = "   ";
        let spaces = spaces.len();

        println!("The value of spaces is: {spaces}");

        // let mut spaces = "   ";
        // spaces = 2; // impossible.

        // let + let => new variable. ignore prev type.
        // mut + re assign => mutate prev value, using prev allocated memory. Not allowed to mutate type.

        let tup: (i32, f64, u8) = (500, 6.4, 1);
        let (x, y, z) = tup;
        println!("The value of xyz are: {y}{x}{z}");

        println!("{}", tup.0);

        let arr: [u8; 5] = [3; 5];
        println!("{:?}", arr);
        println!("{:?}", arr[0]);
    }

    // ========================================================================================
    // ========================================================================================
    // ========================================================================================
    // ========================================================================================

    {
        another_function(3);

        // statement
        let x = 1; // assign, define, no returns anything.
                   // Rust not allowed
                   // let x = y = 6    // 🚫
                   // like.

        println!("The value of x is: {x}");

        // expression
        let x = {
            let x = 2; // --- this is expression scope
            x + 1 // --- returns something.
        };
        // expression evaluates to a resultant value.
        //  --- I think this means, calculate or examine line and returns, in other word.

        println!("The value of x is: {x}");

        println!("The value of x is: {}", five());
    }

    // ========================================================================================
    // ========================================================================================
    // ========================================================================================
    // ========================================================================================

    {
        let x = 1;
        if x < 3 {
            println!("The value of x is: {}", x);
        }

        let x = '0';

        if !x.eq(&1.to_string().chars().nth(0).unwrap()) {
            println!("diff");
        } else {
            println!("same");
        }

        let x = 8;

        let y = if x > 5 { "big" } else { "small" };
        // in other lang: let y = x > 5 ? "big" : "small";
        println!("The value of y is: {y}");
    }

    // ========================================================================================
    // ========================================================================================
    // ========================================================================================
    // ========================================================================================

    {
        let mut count = 0;

        let result = loop {
            count += 1;
            if count == 10 {
                break count * 2;
            }
        };

        println!("The value of result is: {result}");

        let mut count = 0;
        'outer: loop {
            println!("count is: {count}");
            let mut remaining = 10;

            'inner: loop {
                println!("count is: {remaining}");

                if remaining == 9 {
                    break 'inner; // == just break
                }

                if count == 2 {
                    break 'outer;
                }

                remaining -= 1;
            }

            count += 1;
        }

        let mut count = 0;
        while count < 10 {
            count += 1;
            println!("count is: {count}!!");
        }

        let a = [1, 2, 3, 4, 5];
        for aa in a {
            println!("{}", aa);
        }

        for n in (1..=11).rev() {
            println!("{}", n);
        }
    }
}

fn another_function(x: u8) {
    println!("Another function. {}", x);
}

fn five() -> u8 {
    5
}
