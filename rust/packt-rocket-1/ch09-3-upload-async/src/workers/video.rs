// pub fn process_video(pool: Pool<SqlitePool>, wm: Message) -> Result<(), ()> {
//     let mut dest = String::from("static/");
//     dest.push_str(&wm.dest_file_name);
//
//     let builder = FfmpegBuilder::new()
//         .stderr(Stdio::piped())
//         .option(Parameter::Single("nostdin"))
//         .option(Parameter::Single("y"))
//         .input(File::new(&wm.orig_file_name))
//         .output(
//             File::new(&dest)
//                 .option(Parameter::KeyValue("vcodec", "libx265"))
//                 .option(Parameter::KeyValue("crf", "28"))
//         );
//
//     let make_permanent = async {
//         let ffmpeg = builder.run().await.unwrap();
//         let _ = ffmpeg.process.wait_with_output()
//             .unwrap();
//         let mut display_path = String::from("/assets/");
//         display_path.push_str(&wm.dest_file_name);
//
//         // Post::make_permanent(pool, &wm.uuid, &display_path).await
//     };
//
//     let handle = Handle::current();
//
//     Ok(
//         handle.block_on(make_permanent)
//             .map(|_| ())
//             .map_err(|_| ())?
//     )
// }