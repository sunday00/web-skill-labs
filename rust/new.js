const fs = require('fs');
const path = require('path');

const makeMainFile = async (project) => {
    const content = `use ${project.replaceAll('-', '_')}::run;

fn main() {
    run()
}`

    fs.writeFileSync(path.join(process.cwd(), project, 'src', 'main.rs'), content)
}

const makeLibFile = async (project) => {
    const content = `use std::env;

pub mod a_base;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "base" => {
            if args.get(2).unwrap().eq("1") {
                a_base::ch1::exec()
            }
        }
        _ => {}
    }
}`;

    fs.writeFileSync(path.join(process.cwd(), project, 'src', 'lib.rs'), content)
}

const makeModFile = async (project) => {
    const content = `pub mod ch1;`

    fs.writeFileSync(path.join(process.cwd(), project, 'src', 'a_base', 'mod.rs'), content)
}

const makeCh1File = async (project) => {
    const content = `pub fn exec() {
    println!("hello world");
}`

    fs.writeFileSync(path.join(process.cwd(), project, 'src', 'a_base', 'ch1.rs'), content)
}

const main = async() => {
    const project = process.argv[2]

    await makeMainFile(project);
    await makeLibFile(project);

    fs.mkdirSync(path.join(process.cwd(), project, 'src', 'a_base'))

    await makeModFile(project);
    await makeCh1File(project);
}

main().then(() => {
    console.log('done')
})