use std::process::Stdio;
use ffmpeg_cli::{FfmpegBuilder, File, Parameter};
use rocket::http::hyper::body::HttpBody;
use sqlx::{Pool, Sqlite};
use tokio::runtime::Handle;
use crate::models::post::Post;
use crate::models::worker::Message;

pub fn process_video(pool: &Pool<Sqlite>, wm: Message) -> Result<(), ()> {
    let mut dest = String::from("./static/");
    dest.push_str(&wm.orig_filename);

    let mut from = String::from("/tmp/");
    from.push_str(&wm.orig_filename);

    let builder = FfmpegBuilder::new()
        .stderr(Stdio::piped())
        .option(Parameter::Single("nostdin"))
        .option(Parameter::Single("y"))
        // .input(File::new(&wm.orig_filename))
        .input(File::new(from.as_str()))
        .output(
            File::new(&dest)
            // File::new("./static/cc.mp4")
                .option(Parameter::KeyValue("vcodec", "libx265"))
                .option(Parameter::KeyValue("crf", "28"))
        );

    let make_permanent = async {
        let ffmpeg = builder.run().await;

        match ffmpeg {
            Ok(ff) => { ff.process.wait_with_output().unwrap(); }
            Err(e) => { println!("{:#?}", e) }
        }

        let mut display_path = String::from("/assets/");
        display_path.push_str(&wm.orig_filename);

        Post::make_permanent(pool, &wm.uuid, &display_path).await
    };

    let handle = Handle::current();

    Ok(
        handle.block_on(make_permanent)
            .map(|_| ())
            .map_err(|_| ())?
    )
}
